#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>

#include <map>
#include <algorithm>
#include <string>
#include <vector>
#include <fstream>
#include <cstring>
#include <iostream>

#include "udplib.hpp"
#include "utils.hpp"

typedef std::pair<std::string, u_short> sinfo;

struct DupicatedLanguageException : public std::exception
{
	~DupicatedLanguageException() throw () {}
	const char* what() const throw() { return "Duplicated Language Exception"; }
};

class TCS : public UDPServer{
private:
    std::map<std::string, sinfo> _langs;
    std::string _langsfile = "langs.txt";
public:
    
    TCS(u_short port) : UDPServer(port) { 
        _langsfile = "langs.txt";
    }

    std::map<std::string, sinfo> get_langs() {
        return _langs;
    }

    std::vector<std::string> get_lang(std::string lang) {
        std::vector<std::string> out;
        out.push_back(_langs.at(lang).first);
        out.push_back(std::to_string(_langs.at(lang).second));
        return out;
    }

    void add_language(std::string lang, std::string ip, std::string port) {
	if ( _langs.find(lang) != _langs.end() ) {
		throw new DupicatedLanguageException();	
	}
        _langs[lang] = sinfo(ip, std::stoi(port));
        std::cout << "+" + lang + " " + ip + " " + port << std::endl; 
    }

    void remove_language(std::string lang, std::string ip, std:: string port) {
        if(_langs.at(lang) == sinfo(ip, std::stoi(port))) {
            _langs.erase(lang);
            std::cout << "-" + lang + " " + ip + " " + port << std::endl; 
        }
    }

    void  parse_avaliable_languages() {
        std::ifstream filetoparse;
        std::string lang;
        filetoparse.open(_langsfile);
        if(filetoparse.is_open()) {
            while (std::getline(filetoparse, lang)) {
                std::vector<std::string> inputs = tokenize(lang);
                _langs[inputs.at(0)] = sinfo(inputs.at(1), std::stoi(inputs.at(2)));
                std::cout << inputs.at(0) << "  " << inputs.at(1) <<  " " << _langs[inputs.at(0)].first << " " << _langs[inputs.at(0)].second << std::endl;
            }
        }	
    }

    std::vector<std::string>  get_avaliable_languages() {
        std::vector<std::string> out;
        for(std::map<std::string, sinfo>::iterator it = _langs.begin(); it != _langs.end(); ++it) {
            out.push_back(it->first);
        }
        return out;
    }

    ~TCS() {
        if (closed_) return;

        try{
            Close();
        } 
        catch (UDPException e){}
    }
};

int main(int argc, char* argv[]) {

    if(argc > 3){
        std::cerr << "Usage [-p <tcs_port>]";
        exit(1);
    }
    u_short tcs_port = 58000 + GN;
    
    if(argc == 3) {
        try {
            if (!strcmp("-p", argv[1])) {
                long aux;
                aux = std::stol(argv[2]);
                if(aux > (1<<16) || aux <= 0) throw std::out_of_range("");
                tcs_port = aux;
            } 
            else { 
                    throw std::invalid_argument("Unknown option " + std::string(argv[1]));
            }
        }
        catch(std::out_of_range& e) {
            std::cerr << "Port numbers must be in the range 1-65535" << std::endl;
            exit(1); 
        }
        catch(std::invalid_argument& e) { 
            std::string message = (!strcmp(e.what(),"stol") ? "Ports must be a number" : e.what());
            std::cerr << message << std::endl;
            exit(1);
        }
    }

    TCS server(tcs_port);
    server.parse_avaliable_languages();
    server.get_avaliable_languages();

    while(1) {
        std::vector<uint8_t> msg = server.Read(false);
        std::string msgstr = string_cast(msg);
        std::vector<std::string> input = tokenize(msgstr);
        std::vector<std::string> avlangs = server.get_avaliable_languages();
        std::string response = "";

        bool secure = input.size();
        if(secure &&  input.at(0) == "ULQ") {
            response = "ULR " + std::to_string(avlangs.size()) + " ";
            for(int i = 0; i < avlangs.size(); i++) {
                response = response + avlangs.at(i) + " ";
            }
            std::string tmp = response;
            //std::cout << "List request: " << server.address_.sin_port << std::endl;
            std::cout << "\t" << tmp.erase(0, 6) << std::endl;
        }
        else if (secure && input.at(0) == "UNQ") {
			try {
				response = "UNR ";
				if(std::find(avlangs.begin(), avlangs.end(), input.at(1)) != avlangs.end())
				{
					response = response + server.get_lang(input.at(1)).at(0);
			        response = response + " " + server.get_lang(input.at(1)).at(1);
                    std::string tmp = response;
                    std::cout << "\t" << tmp.erase(0, 4) << std::endl;
				}
				else {
					response = "Language not supported";
				}
			} catch(...) {
				response = "UNR ERR";
			}
        }
        else if (secure && input.at(0) == "SRG") {
		try {
			server.add_language(input.at(1), input.at(2), input.at(3));		
			response = "SRR OK";
		}
		catch (...) {
			response = "SRR NOK";		
		}		
        }
	else if (secure && input.at(0) == "SUN") {
		try {
			server.remove_language(input.at(1), input.at(2), input.at(3));		
			response = "SUR OK";
		}
		catch (...) {
			response = "SUR NOK";		
		}		
        }
        else {
            response = "ERR Secure is false";
			secure = false;
        }
        if(secure) {
            server.Write(byte_cast(response + "\n"));
        }
    }
}

