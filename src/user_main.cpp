#include "tcplib.hpp"
#include "udplib.hpp"
#include <iostream>
#include <tuple>
#include <sstream>
#include <fstream>
#define GN 7
#define LIM_nL_MAX 99
#define LIM_nL_MIN 99
using namespace std; 


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
    string _hostname;
    u_short _port;
    UDPChannel _TCSChannel;
    vector<string> lang_names; 
public:
    TRCClientInterface(const string& hostname, u_short port) :  _TCSChannel(hostname, port), _hostname(hostname), _port(port) {}

    tuple <string,u_short> UNQ(int lang_id) {
        string lang = lang_names.at(lang_id); //throws out_of_range
        string request = string("UNQ ");
        request += lang;
        request += "\n";
        _TCSChannel.Write(vector<uint8_t>(request.begin(), request.end()));
        vector<uint8_t> response = _TCSChannel.Read(); //TODO: timeout = 10
        
        vector<string> tokens;
        string aux;
        stringstream ss(string(response.begin(), response.end()));
        while (ss >> aux)
            tokens.push_back(aux);
        
        if (tokens.at(0) != "UNR") throw invalid_argument("Unknown response");
        if (tokens.at(1) == "ERR") throw invalid_request("Server got an invalid request");
        if (tokens.at(1) == "EOF") {
            throw translation_not_available("No translation could be provided for the requested language");
        }
        if (tokens.size() != 3)  {
            invalid_argument("Invalid number of parameters on the response");
        }
        string hostname = tokens.at(1);
        unsigned long i_port = stoul(tokens.at(2));
        if (i_port >= (1<<16)) {
            throw out_of_range("Invalid port name");
        }
        return make_tuple((string) hostname, (u_short) i_port);
    }

    vector<string> TLQ() {
        string request = string("ULQ\n");
        _TCSChannel.Write(vector<uint8_t>(request.begin(), request.end()));
        vector<uint8_t> response = _TCSChannel.Read(); //TODO: timeout = 10
        
        vector<string> tokens;
        string aux;
        stringstream ss(string(response.begin(), response.end()));
        while (ss >> aux)
            tokens.push_back(aux);
        
        if (tokens.at(0) != "ULR") throw invalid_argument("Unknown response");
        if (tokens.at(1) == "ERR") throw invalid_request("Server got an invalid request");
        if (tokens.at(1) == "EOF") {
            return vector<string>();
        }
        unsigned long n = stoul(tokens.at(1));
        if (tokens.size() != n + 2)  {
            invalid_argument("Invalid number of parameters on the response");
        }
        return vector<string>(tokens.begin()+2, tokens.end());
    }

    vector<string> TRQ(TCPChannel& channel, const vector<string>& wordlist) {
        channel.Write(string("TRQ t "));
        channel.Write(to_string(wordlist.size()));
        for (auto w : wordlist) {
            channel.Write(string(" "));
            channel.Write(w);
        }
        channel.Write(string("\n"));
        string response = channel.ReadUntil('\n', string());
        vector<string> tokens;
        string aux;
        stringstream ss(response);
        while (ss >> aux)
            tokens.push_back(aux);
        
        if (tokens.at(0) != "TRR") throw invalid_argument("Unknown response");
        if (tokens.at(1) == "ERR") throw invalid_request("Server got an invalid request");
        if (tokens.at(1) == "NTA") {
            throw translation_not_available("No translation could be provided for the requested language");
        }
        if (tokens.at(1) != "t") throw invalid_argument("Invalid response type");
        int n = stoi(tokens.at(2));
        if (n != wordlist.size()) throw invalid_argument("Word count mismatch");
        if (tokens.size() != n + 3) throw invalid_argument("Word count mismatch");
        return vector<string>(tokens.begin()+3, tokens.end());
    }

    tuple<string, size_t> TRQ(TCPChannel& channel, string& filename, size_t sizeInBytes) {
        channel.Write(string("TRQ f "));
	channel.Write(filename);
	channel.Write(string(" "));
        channel.Write(to_string(sizeInBytes));
	channel.Write(string(" "));
        ifstream inFile;
        inFile.open(filename, ios::in);
        channel.Write(inFile);
        channel.Write(string("\n"));
        string response = channel.ReadUntil(' ', string());
        if (response != "TRR") throw invalid_argument("Unknown response");

        response = channel.ReadUntil(' ', string());
        if (response == "ERR") throw invalid_request("Server got an invalid request");
        if (response == "NTA") {
            throw translation_not_available("No translation could be provided for the requested language");
        }
        if (response != "f") throw invalid_argument("Invalid response type");

        string ofilename = channel.ReadUntil(' ', string());
        response = channel.ReadUntil(' ', string());
        int n = stoi(response);

        ofstream ofile;
        ofile.open(ofilename, ios::out);
        vector<uint8_t> bytes = channel.Read(n);
        ofile.write((char*)bytes.data(), bytes.size());
        ofile.close();

        return make_tuple(filename, n);
    }

    void RequestWords(int lang, const vector<string>& wordlist) {
        string hostname;
        u_short port;
        try {
            tie(hostname, port) = UNQ(lang);
        } catch (...) {
            cerr << "request t: Could not get TRS server for language " << lang << "!" << endl;
            return;
        }

        TCPChannel channel;
        try {
            channel = TCPChannel(hostname.c_str(),port);
        } catch (...) {
            cerr << "request t: Could not conect to server " << hostname << " on port " << port << endl;
            return;
        }
        
        try {
            vector<string> translation = TRQ(channel, wordlist);
            cout << "Translations: " << endl; 
            for (size_t i=0; i<wordlist.size(); i++) {
                const string& word  = wordlist.at(i);
                const string& trans = translation.at(i);
                cout << "\t"  << word << " <-> \t" << trans << endl;
            }
        } catch(...) {
            cerr << "request t: Translation failed";
            return;
        }
    }

    void RequestImage(int lang, string& filename) {
        string hostname;
        u_short port;
        try {
            tie(hostname, port) = UNQ(lang);
        } catch (...) {
            cerr << "request f: Could not get TRS server for language " << lang << "!" << endl;
            return;
        }

        size_t sizeInBytes;    
        try {
            std::ifstream inputFile;
            inputFile.open(filename, ios::binary | ios::ate);
            sizeInBytes = inputFile.tellg();
            inputFile.close();
        } catch (...) {
            cerr << "request f: Could not open file " << filename << " for reading" << endl;
            return;
        }

        TCPChannel conn;
        try {
            conn = TCPChannel(hostname.c_str(),port);
        } catch (...) {
            cerr << "request f: Could not conect to server " << hostname << " on port " << port << endl;
            return;
        }

        //TODO: stuff...
        try {
            string outfilename;
            size_t outfilelen;
            tie(outfilename,outfilelen) = TRQ(conn, filename, sizeInBytes);
        } catch (...) {
            cerr << "request f: TRQ failed :(" << endl;
            return;
        }
    }

    void Request(vector<string>& tokens) {
        if (tokens.size() < 4) {
            cerr << "request: Insuficient arguments" << endl;
            return;
        }

        if (tokens[2]=="t") {
            int n=0;
            int lang=0;
            try {
                lang = stoi(tokens[1]);
                n = stoi(tokens[3]);
                if ((tokens.size() - 4 != (unsigned int) n)
                   || lang <= 0 
                   || n <= 0) {
                       throw invalid_argument("");
                   }
            } catch (...) {
                cerr << "request: Invalid argument" << endl;
                return;
            }
            RequestWords(lang-1, vector<string>(tokens.begin()+4, tokens.end()));
        } else if (tokens[2]=="f") {
            int lang=0;
            try {
                lang = stoi(tokens[1]);
                if ((tokens.size() != 4) 
                   || lang <= 0) {
                       throw invalid_argument("");
                   }
            } catch (...) {
                cerr << "request: Invalid argument" << endl;
                return;
            }
            RequestImage(lang-1, tokens[3]);
        } else {
            cerr << "request: Unknow mode '" << tokens[2] << "'" << endl;
            return;
        }
    }


    void List() {
        try {
            const vector<string>& ltmp = TLQ();
            lang_names = ltmp;
            for (size_t i=0; i<lang_names.size(); i++) {
                cout << i+1 << "- " << lang_names[i] << endl; 
            }
        } catch(exception& e) {
	    cout << "Caught exception" << e.what() << endl;
            return;
        }
    }

    void Loop() {
        string line;
        //Connect to server
        while(getline(cin,line)) {
            vector<string> tokens;
            string aux;
            stringstream ss(line);
            while (ss >> aux) 
                tokens.push_back(aux);


            if (tokens.size()==0) continue; //empty line

            if (tokens[0] == "list") {
                List();
            } else if (tokens[0] == "exit") {
                cout << "Bye!" << endl;
                return;
            } else if (tokens[0] == "request") {
                Request(tokens);
            } else {
                cout << "Unknown option (" << line << ")" << endl; 
            }      
        }
    }
};


int main(int argc, char **argv) {
    u_short port = 58000 + GN; 
    string hostname = string("localhost");
    for (int i = 1; i+1<argc; i++) {
        if (!strcmp("-n", argv[i])) {
            i++;
            hostname = string(argv[i]);
        } else if (!strcmp("-p", argv[i])) {
            i++;
            try {
                unsigned long aux = stoul(string(argv[i]));
                if (aux >= (1<<16)) throw out_of_range("Port name too high");
                port = (u_short) aux;
            } catch (...) {
                cerr << "Invalid port..." << endl;
            }
        } else {
            cerr << "Unknow option: " << argv[i] << endl;
        }
    }

    TRCClientInterface client = TRCClientInterface(hostname, port);
    try {
        client.Loop();
    } catch (...) {
        cerr << "Something bad happened and we don't know how to take care of it. If you are using a bug-free version of libc and libstdc++ any good symbolic executor will tell you that this line is not reachable.'" << endl;
    }

    return 0;
}