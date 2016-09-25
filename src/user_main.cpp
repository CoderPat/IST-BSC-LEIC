#include "tcplib.hpp"
//#include "udplib.hpp"
#define GN 7
#define LIM_nL_MAX 99
#define LIM_nL_MIN 99
using namespace std; 


struct TRCException : public std::exception
{
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


class TRCClientInterface {
    string _hostname;
    uint16_t _port;
    UDPChannel _TCSChannel;
    vector<string> lang_names; 
public:
    TRCClientInterface(const string& hostname, uint16_t port) :  _TCSChannel(hostname, port), _hostname(hostname), _port(port) {}

    tuple <string,uint16_t> UNQ(int lang_id) {
        string lang = lang_names.at(lang_id); //throws out_of_range
        string request = string("UNQ ");
        request += lang;
        request += "\n";
        response = _TCSChannel.send(request, 10); //timeout = 10

        //TODO: check response and turn it into a tuple
    }

    void TLQ() {
        //TODO: This updates the lang_names
    }

    vector<string> TRQ(const TCPChannel& channel, const vector<string>& wordlist) {
        channel.WriteBytes("TRQ ");
        channel.WriteBytes(to_string(wordlist.size()));
        for (auto w : wordlist) {
            channel.WriteBytes(" ");
            channel.WriteBytes(w);
        }
        channel.WriteBytes("\n");
        string response = channel.ReadLine();
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
        if (token.size() != n + 3) throw invalid_argument("Word count mismatch");
        return vector<string>(tokens.begin()+3, tokens.end());
    }

    vector<string> TRQ(const TCPChannel& channel, /*TODO: template for file translation*/) {
        //TODO: Do stuff to send the file
    }

    void RequestWords(int lang, int n, const vector<string>& wordlist) {
        string hostname;
        uint16_t port;
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
            //TODO: Print out the translation in the required format...
        } catch(...) {
            cerr << "request t: Translation failed";
            return;
        }
    }

    void RequestImage(int lang, const string& filename) {
        string hostname;
        uint16_t port;
        try {
            tie(hostname, port) = UNQ(lang);
        } catch (...) {
            cerr << "request t: Could not get TRS server for language " << lang << "!" << endl;
            return;
        }

        TCPChannel conn;
        try {
            conn = TCPChannel(hostname.c_str(),port);
        } catch (...) {
            cerr << "request t: Could not conect to server " << hostname << " on port " << port << endl;
            return;
        }

        //TODO: stuff...
    }

    void Request(const vector<string>& tokens) {
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
                if ((tokens.size() + 4 != (unsigned int) n)
                   || lang <= 0 
                   || n <= 0) {
                       throw invalid_argument("");
                   }
            } catch (invalid_argument, out_of_range) {
                cerr << "request: Invalid argument" << endl;
                return;
            }
            RequestWords(lang, vector<string>(tokens.begin()+4, tokens.end()));
        } else if (tokens[2]=="f") {
            int lang=0;
            try {
                lang = stoi(tokens[1]);
                if ((tokens.size() + 4 != (unsigned int) n) 
                   || lang <= 0) {
                       throw invalid_argument("");
                   }
            } catch (invalid_argument, out_of_range) {
                cerr << "request: Invalid argument" << endl;
                return;
            }
            RequestFile(lang, tokens[3]);
        } else {
            cerr << "request: Unknow mode '" << tokens[2] << "'" << endl;
            return;
        }
    }


    void List() {
        try {
            TQL();
        } catch(...) {
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
    uint16_t port = 58000 + GN; 
    string hostname = string("localhost");
    for (int i = 1; i+1<argc; i++) {
        if (!strcmp("-n", argv[i])) {
            i++;
            hostname = string(argv[i]);
        } else if (!strcmp("-p", argv[i])) {
            i++;
            hostname = string(argv[i]);
        }
        cerr << "Unknow option: " << argv[i] << endl;
    }

    TRCClientInterface client = new TRCClientInterface(hostname, port);
    try {
        client.Loop();
    } catch (...) {
        cerr << "Something bad happened and we don't know how to take care of it. If you are using a bug-free version of libc and libstdc++ any good symbolic executor will tell you that this line is not reachable.'" << endl;
    }

    return 0;
}