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


//Everything has strong exception safety guarantee
class TRCClientInterface {
    std::string _hostname;
    u_short _port;
    UDPChannel _TCSChannel;
    std::vector<std::string> lang_names; 
public:
    TRCClientInterface(const std::string& hostname, u_short port) :  _TCSChannel(hostname, port), _hostname(hostname), _port(port) {}

    std::tuple<std::string,u_short> UNQ(int lang_id) {
        std::string lang = lang_names.at(lang_id); //throws out_of_range
        std::string request = std::string("UNQ ");
        request += lang;
        request += "\n";
        _TCSChannel.Write(std::vector<uint8_t>(request.begin(), request.end()));
        std::vector<uint8_t> response = _TCSChannel.Read(); //TODO: timeout = 10
        
        std::vector<std::string> tokens;
        std::string aux;
        std::stringstream ss(std::string(response.begin(), response.end()));
        while (ss >> aux)
            tokens.push_back(aux);
        
        if (tokens.at(0) != "UNR") throw std::invalid_argument("Unknown response");
        if (tokens.at(1) == "ERR") throw invalid_request("Server got an invalid request");
        if (tokens.at(1) == "EOF") throw translation_not_available("No translation could be provided for the requested language");
        if (tokens.size() != 3) throw std::invalid_argument("Invalid number of parameters on the response");

        std::string hostname = tokens.at(1);
        unsigned long i_port = std::stoul(tokens.at(2));
        if (i_port >= (1<<16)) 
            throw std::out_of_range("Invalid port name");
        
        return std::make_tuple((std::string) hostname, (u_short) i_port);
    }

    std::vector<std::string> TLQ() {
        std::string request = std::string("ULQ\n");
        _TCSChannel.Write(std::vector<uint8_t>(request.begin(), request.end()));
        std::vector<uint8_t> response = _TCSChannel.Read(); //TODO: timeout = 10
        
        std::vector<std::string> tokens;
        std::string aux;
        std::stringstream ss(std::string(response.begin(), response.end()));
        while (ss >> aux)
            tokens.push_back(aux);
        
        if (tokens.at(0) != "ULR") throw std::invalid_argument("Unknown response");
        if (tokens.at(1) == "ERR") throw invalid_request("Server got an invalid request");
        if (tokens.at(1) == "EOF") 
            return std::vector<std::string>();
        
        unsigned long n = std::stoul(tokens.at(1));
        if (tokens.size() != n + 2)  {
            std::invalid_argument("Invalid number of parameters on the response");
        }
        return std::vector<std::string>(tokens.begin()+2, tokens.end());
    }

    std::vector<std::string> TRQ(TCPChannel& channel, const std::vector<std::string>& wordlist) {
        channel.Write("TRQ t ");
        channel.Write(std::to_string(wordlist.size()));
        for (auto w : wordlist) {
            channel.Write(" ");
            channel.Write(w);
        }
        channel.Write("\n");
        std::string response = channel.ReadUntil('\n', std::string());
        std::vector<std::string> tokens;
        std::string aux;
        std::stringstream ss(response);
        while (ss >> aux)
            tokens.push_back(aux);
        
        if (tokens.at(0) != "TRR") throw std::invalid_argument("Unknown response");
        if (tokens.at(1) == "ERR") throw invalid_request("Server got an invalid request");
        if (tokens.at(1) == "NTA") {
            throw translation_not_available("No translation could be provided for the requested language");
        }
        if (tokens.at(1) != "t") throw std::invalid_argument("Invalid response type");
        int n = std::stoi(tokens.at(2));
        if (n != wordlist.size()) throw std::invalid_argument("Word count mismatch");
        if (tokens.size() != n + 3) throw std::invalid_argument("Word count mismatch");
        return std::vector<std::string>(tokens.begin()+3, tokens.end());
    }

    std::tuple<std::string, size_t> TRQ(TCPChannel& channel, std::string& filename, size_t sizeInBytes) {
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

        response = channel.ReadUntil(' ', std::string());
        if (response == "ERR") throw invalid_request("Server got an invalid request");
        if (response == "NTA") throw translation_not_available("No translation could be provided for the requested language");
        if (response != "f") throw std::invalid_argument("Invalid response type");

        std::string ofilename = channel.ReadUntil(' ', std::string());
        response = channel.ReadUntil(' ', std::string());
        int n = std::stoi(response);

        std::ofstream ofile;
        ofile.open(ofilename, std::ios::out);
        std::vector<uint8_t> bytes = channel.Read(n);
        ofile.write((char*)bytes.data(), bytes.size());
        ofile.close();

        return make_tuple(filename, n);
    }

    void RequestWords(int lang, const std::vector<std::string>& wordlist) {
        std::string hostname;
        u_short port;
        try {
            tie(hostname, port) = UNQ(lang);
        } catch (...) {
            std::cerr << "request t: Could not get TRS server for language " << lang << "!" << std::endl;
            return;
        }

        TCPChannel channel;
        try {
            channel = TCPChannel(hostname.c_str(),port);
        } catch (...) {
            std::cerr << "request t: Could not conect to server " << hostname << " on port " << port << std::endl;
            return;
        }
        
        try {
            std::vector<std::string> translation = TRQ(channel, wordlist);
            std::cout << "Translations: " << std::endl; 
            for (size_t i=0; i<wordlist.size(); i++) {
                const std::string& word  = wordlist.at(i);
                const std::string& trans = translation.at(i);
                std::cout << "\t"  << word << " <-> \t" << trans << std::endl;
            }
        } catch(...) {
            std::cerr << "request t: Translation failed";
            return;
        }
    }

    void RequestImage(int lang, std::string& filename) {
        std::string hostname;
        u_short port;
        try {
            tie(hostname, port) = UNQ(lang);
        } catch (...) {
            std::cerr << "request f: Could not get TRS server for language " << lang << "!" << std::endl;
            return;
        }

        size_t sizeInBytes;    
        try {
            std::ifstream inputFile;
            inputFile.open(filename, std::ios::binary | std::ios::ate);
            sizeInBytes = inputFile.tellg();
            inputFile.close();
        } catch (...) {
            std::cerr << "request f: Could not open file " << filename << " for reading" << std::endl;
            return;
        }

        TCPChannel conn;
        try {
            conn = TCPChannel(hostname.c_str(),port);
        } catch (...) {
            std::cerr << "request f: Could not conect to server " << hostname << " on port " << port << std::endl;
            return;
        }

        try {
            std::string outfilename;
            size_t outfilelen;
            tie(outfilename,outfilelen) = TRQ(conn, filename, sizeInBytes);
            std::cout << "Downloaded tranlated file: " << outfilename << " (" << outfilelen << " bytes)" << std::endl;
        } catch (...) {
            std::cerr << "request f: TRQ failed :(" << std::endl;
            return;
        }
    }

    void Request(std::vector<std::string>& tokens) {
        if (tokens.size() < 4) {
            std::cerr << "request: Insuficient arguments" << std::endl;
            return;
        }

        if (tokens[2]=="t") {
            int n=0;
            int lang=0;
            try {
                lang = std::stoi(tokens[1]);
                n = std::stoi(tokens[3]);
                if ((tokens.size() - 4 != (unsigned int) n)
                   || lang <= 0 
                   || n <= 0) {
                       throw std::invalid_argument("");
                   }
            } catch (...) {
                std::cerr << "request: Invalid argument" << std::endl;
                return;
            }
            RequestWords(lang-1, std::vector<std::string>(tokens.begin()+4, tokens.end()));
        } else if (tokens[2]=="f") {
            int lang=0;
            try {
                lang = std::stoi(tokens[1]);
                if ((tokens.size() != 4) 
                   || lang <= 0) {
                       throw std::invalid_argument("");
                   }
            } catch (...) {
                std::cerr << "request: Invalid argument" << std::endl;
                return;
            }
            RequestImage(lang-1, tokens[3]);
        } else {
            std::cerr << "request: Unknow mode '" << tokens[2] << "'" << std::endl;
            return;
        }
    }


    void List() {
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
    for (int i = 1; i+1<argc; i++) {
        if (!strcmp("-n", argv[i])) {
            i++;
            hostname = argv[i];
        } else if (!strcmp("-p", argv[i])) {
            i++;
            try {
                unsigned long aux = std::stoul(std::string(argv[i]));
                if (aux >= (1<<16)) throw std::out_of_range("Port name too high");
                port = (u_short) aux;
            } catch (...) {
                std::cerr << "Invalid port..." << std::endl;
            }
        } else {
            std::cerr << "Unknow option: " << argv[i] << std::endl;
        }
    }

    try {
        TRCClientInterface client = TRCClientInterface(hostname, port);
        client.Loop();
    } catch (...) {
        std::cerr << "Something bad happened and we don't know how to take care of it. If you are using a bug-free version of libc and libstdc++ any good symbolic executor will tell you that this line is not reachable.'" << std::endl;
    }   

    return 0;
}