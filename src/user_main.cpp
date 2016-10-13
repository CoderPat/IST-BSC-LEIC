#include <iostream>
#include <tuple>
#include <sstream>
#include <fstream>

#include "tcplib.hpp"
#include "udplib.hpp"
#include "utils.hpp"


struct TRCException : public std::exception {
   std::string s;
   TRCException(std::string ss) : s(ss) {}
   ~TRCException() throw () {}
   const char* what() const throw() { return s.c_str(); }
};
struct translation_not_available : public TRCException{
   translation_not_available(std::string ss) : TRCException(ss) {}
   ~translation_not_available() throw () {}
};
struct invalid_request : public TRCException{
   invalid_request(std::string ss) : TRCException(ss) {}
   ~invalid_request() throw () {}
};


/** 
 * Client interface that issues requests for words and images transaltions
 */
class TRCClientInterface {
private:
    std::string _hostname;
    u_short _port;
    UDPChannel _TCSChannel;
    std::vector<std::string> lang_names; 

public:
    TRCClientInterface(const std::string& hostname, u_short port) :  _TCSChannel(hostname, port), _hostname(hostname), _port(port) {}


    /** 
     * Requests the IP and port of a Translation Server to the TCS 
     */
    std::tuple<std::string,u_short> UNQ(int lang_id) {

        //Write protocol request for the transsalation server IP
        std::string lang = lang_names.at(lang_id); //throws out_of_range
        std::string request = std::string("UNQ ");
        request += lang;
        request += "\n";
        _TCSChannel.Write(std::vector<uint8_t>(request.begin(), request.end()));
        std::vector<uint8_t> response = _TCSChannel.Read(); 
        
        //Get the tokens in the response
        std::vector<std::string> tokens;
        std::string aux;
        std::stringstream ss(std::string(response.begin(), response.end()));
        while (ss >> aux)
            tokens.push_back(aux);
        
        //Check for bad protocol responses
        if (tokens.at(0) != "UNR") throw std::invalid_argument("Unknown response");
        if (tokens.at(1) == "ERR") throw invalid_request("Server got an invalid request");
        if (tokens.at(1) == "EOF") throw translation_not_available("No translation could be provided for the requested language");
        if (tokens.size() != 3) throw std::invalid_argument("Invalid number of parameters on the response");

        //Check port and hostname integrity and return them
        std::string hostname = tokens.at(1);
        unsigned long i_port = std::stoul(tokens.at(2));
        if (i_port >= (1<<16)) 
            throw std::out_of_range("Invalid port name");
        
        return std::make_tuple((std::string) hostname, (u_short) i_port);
    }

    /** 
     * Requests the list of languages available and connected to the TCS
     */
    std::vector<std::string> TLQ() {
        std::string request = "ULQ\n";
        _TCSChannel.Write(std::vector<uint8_t>(request.begin(), request.end()));
        std::vector<uint8_t> response = _TCSChannel.Read();
        
        //Get the tokens in the message
        std::vector<std::string> tokens;
        std::string aux;
        std::stringstream ss(std::string(response.begin(), response.end()));
        while (ss >> aux)
            tokens.push_back(aux);
        
        //Check for bad protocol responses
        if (tokens.at(0) != "ULR") throw std::invalid_argument("Unknown response");
        if (tokens.at(1) == "ERR") throw invalid_request("Server got an invalid request");
        if (tokens.at(1) == "EOF") 
            return std::vector<std::string>();
        
        unsigned long n = std::stoul(tokens.at(1));
        if (tokens.size() != n + 2) throw std::invalid_argument("Invalid number of parameters on the response");

        return std::vector<std::string>(tokens.begin()+2, tokens.end());
    }

    /** 
     * Requests the translation of words to the TRS in the other side of the channel
     */
    std::vector<std::string> TRQ(TCPChannel& channel, const std::vector<std::string>& wordlist) {
        
        //Write protocol request for word transaltion
        channel.Write("TRQ t ");
        channel.Write(std::to_string(wordlist.size()));
        for (auto w : wordlist) {
            channel.Write(" ");
            channel.Write(w);
        }
        channel.Write("\n");

        //Wait for response and get the translated words
        std::string response = channel.ReadUntil('\n', std::string());
        std::vector<std::string> tokens;
        std::string aux;
        std::stringstream ss(response);
        while (ss >> aux)
            tokens.push_back(aux);
        
        //Check for bad protocol responses
        if (tokens.at(0) != "TRR") throw std::invalid_argument("Unknown response");
        if (tokens.at(1) == "ERR") throw invalid_request("Server got an invalid request");
        if (tokens.at(1) == "NTA") throw translation_not_available("No translation could be provided for the requested language");

        if (tokens.at(1) != "t") throw std::invalid_argument("Invalid response type");
        int n = std::stoi(tokens.at(2));
        if (n != wordlist.size()) throw std::invalid_argument("Word count mismatch");
        if (tokens.size() != n + 3) throw std::invalid_argument("Word count mismatch");
        return std::vector<std::string>(tokens.begin()+3, tokens.end());
    }

    /** 
     * Requests the translation of an image to the TRS in the other side of the channel
     */
    std::tuple<std::string, size_t> TRQ(TCPChannel& channel, std::string& filename, size_t sizeInBytes) {

        //Write protocol request for file transaltion
        channel.Write("TRQ f ");
        channel.Write(filename);
        channel.Write(" ");
        channel.Write(std::to_string(sizeInBytes));
        channel.Write(" ");
        std::ifstream inFile;
        inFile.open(filename, std::ios::in);
        channel.Write(inFile);
        channel.Write("\n");

        std::string response = channel.ReadUntil(' ', std::string());
        if (response != "TRR") throw std::invalid_argument("Unknown response");

        //Check for bad protocol responses
        response = channel.ReadUntil(" \n", std::string());
        if (response == "ERR") throw invalid_request("Server got an invalid request");
        if (response == "NTA") throw translation_not_available("No translation could be provided for the requested language");
        if (response != "f") throw std::invalid_argument("Invalid response type");

        //Read file name and size
        std::string ofilename = channel.ReadUntil(' ', std::string());
        response = channel.ReadUntil(' ', std::string());
        int n = std::stoi(response);

        //Read file data
        std::ofstream ofile;
        ofile.open(ofilename, std::ios::out);
        std::vector<uint8_t> bytes = channel.Read(n);
        ofile.write((char*)bytes.data(), bytes.size());
        ofile.close();

        return std::make_tuple(ofilename, n);
    }

    void RequestWords(int lang, const std::vector<std::string>& wordlist) {
        std::string hostname;
        u_short port;

        //Get translation server ip and port
        try {
            tie(hostname, port) = UNQ(lang);
        }
        catch (translation_not_available& e) {
            std::cerr << "request t: TCS server didn't have have a translation server for laguange " << lang << std::endl;
            return;
        }
        catch (...) {
            std::cerr << "request t: something went wrong when getting the TRS server for language " << lang << "!" << std::endl;
            return;
        }

        TCPChannel channel;
        try {
            channel = TCPChannel(hostname.c_str(),port);
        } catch (...) {
            std::cerr << "request t: could not connect to server " << hostname << " on port " << port << std::endl;
            return;
        }
        
        //Translate the words and print them
        try {
            std::vector<std::string> translation = TRQ(channel, wordlist);
            std::cout << "Translations: " << std::endl; 
            for (size_t i=0; i<wordlist.size(); i++) {
                const std::string& word  = wordlist.at(i);
                const std::string& trans = translation.at(i);
                std::cout << "\t"  << word << " <-> \t" << trans << std::endl;
            }
        } 
        catch(translation_not_available& e) {
            std::cerr << "request t: no translation for one of the words" << std::endl;
            return;
        } 
        catch(...){
            std::cerr << "request t: something wrong reading the request (possibly a protocol error)" << std::endl;
            return;
        }
    }

    void RequestImage(int lang, std::string& filename) {
        std::string hostname;
        u_short port;

        //Get the file size and check if it existes
        size_t sizeInBytes;    
        try {
            std::ifstream inputFile;
            inputFile.open(filename, std::ios::binary | std::ios::ate);
            sizeInBytes = inputFile.tellg();
            if(sizeInBytes==-1) throw std::exception();
            inputFile.close();
        } 
        catch (...) {
            std::cerr << "request f: could not open file " << filename << " for reading" << std::endl;
            return;
        }

        //Get translation server ip and port
        try {
            tie(hostname, port) = UNQ(lang);
        } 
        catch (...) {
            std::cerr << "request f: could not get TRS server for language " << lang << "!" << std::endl;
            return;
        }

        TCPChannel conn;
        try {
            conn = TCPChannel(hostname.c_str(),port);
        } 
        catch (...) {
            std::cerr << "request f: could not connect to server " << hostname << " on port " << port << std::endl;
            return;
        }

        //Translate the file and saves it in a temp location
        try {
            std::string outfilename;
            size_t outfilelen;
            tie(outfilename,outfilelen) = TRQ(conn, filename, sizeInBytes);
            std::cout << "\tdownloaded translated file: " << outfilename << " (" << outfilelen << " bytes)" << std::endl;
        } 
        catch(translation_not_available& e) {
            std::cerr << "request t: no translation for the image" << std::endl;
            return;
        }
        catch(...){
            std::cerr << "request t: something wrong reading the request (possibly a protocol error in the message)" << std::endl;
            return;
        }
    }

    void Request(std::vector<std::string>& tokens) {
        if (tokens.size() < 4) {
            std::cerr << "request: insuficient arguments" << std::endl;
            return;
        }
        // Request for words
        if (tokens[2]=="t") {
            int n=0;
            int lang=0;
            try {
                lang = std::stoi(tokens[1]);
            } catch (...) {
                std::cerr << "request: invalid transaltion server number" << std::endl;
                return;
            }
            RequestWords(lang-1, std::vector<std::string>(tokens.begin()+3, tokens.end()));
        //Request for images
        } else if (tokens[2]=="f") {
            int lang=0;
            try {
                lang = std::stoi(tokens[1]);
                if ((tokens.size() != 4) 
                   || lang <= 0) {
                       throw std::invalid_argument("");
                   }
            } catch (...) {
                std::cerr << "request: invalid argument" << std::endl;
                return;
            }
            RequestImage(lang-1, tokens[3]);
        } else {
            std::cerr << "request: unknow mode '" << tokens[2] << "'" << std::endl;
            return;
        }
    }


    void List() {
        //Get the list and print them
        try {
            const std::vector<std::string>& ltmp = TLQ();
            lang_names = ltmp;
            for (size_t i=0; i<lang_names.size(); i++) 
                std::cout << i+1 << "- " << lang_names[i] << std::endl; 
            
        } catch(std::exception& e) {
            std::cout << "Caught exception" << e.what() << std::endl;
            return;
        }
    }

    /** 
    * User execution loop, reads commands and issues requests
    */
    void Loop() {
        std::string line;
        //Connect to server
        while(getline(std::cin,line)) {
            std::vector<std::string> tokens;
            std::string aux;
            std::stringstream ss(line);
            while (ss >> aux) 
                tokens.push_back(aux);


            if (tokens.size()==0) continue; //empty line

            if (tokens[0] == "list") {
                List();
            } else if (tokens[0] == "exit") {
                std::cout << "Bye!" << std::endl;
                return;
            } else if (tokens[0] == "request") {
                Request(tokens);
            } else {
                std::cout << "Unknown option (" << line << ")" << std::endl; 
            }      
        }
    }
};


int main(int argc, char **argv) {
    u_short port = 58000 + GN; 
    std::string hostname = "localhost";

    if(argc%2==0){
        std::cerr << "Usage [-p <port>] [-n <hostname>]" << std::endl;
        return 1;
    }
    //Parse the arguments
    for (int i = 1; i+1<argc; i++) {
        try{
            if (!strcmp("-n", argv[i])) {
                i++;
                hostname = argv[i];
            } else if (!strcmp("-p", argv[i])) {
                i++;
                unsigned long aux = std::stoul(std::string(argv[i]));
                if (aux >= (1<<16)) throw std::out_of_range("");
                port = (u_short) aux;
            } else 
                throw std::invalid_argument("Unknown option " + std::string(argv[i]));
        }
        catch(std::out_of_range& e){
            std::cerr << "Port numbers must be in the range 1-65535" << std::endl;
            exit(1); 
        }
        catch(std::invalid_argument& e){ 
            std::string message = (!strcmp(e.what(),"stol") ? "Ports must be a number" : e.what());
            std::cerr << message << std::endl;
            exit(1);
        }

    }
    //Start server loop
    try {
        TRCClientInterface client = TRCClientInterface(hostname, port);
        client.Loop();
    } catch (...) {
        std::cerr << "Something bad happened and we don't know how to take care of it. If you are using a bug-free version of libc and libstdc++ any good symbolic executor will tell you that this line is not reachable.'" << std::endl;
    }   

    return 0;
}