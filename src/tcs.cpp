#include <unistd.h>
#include <map>
#include <algorithm>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string>
#include <vector>
#include <fstream>
#include <cstring>
#include <iostream>
#include <stdlib.h>
#include "../include/udplib.hpp"
#include "../include/utils.hpp"

typedef std::pair<std::string, u_short> sinfo;

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
        _langs[lang] = sinfo(ip, std::stoi(port));
    }

    void remove_language(std::string lang, std::string ip, std:: string port) {
        if(_langs.at(lang) == sinfo(ip, std::stoi(port))) {
            _langs.erase(lang);
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

int main(int argc, char* args[]) {


    TCS server(50001);
    server.parse_avaliable_languages();
    server.get_avaliable_languages();

    while(1) {
        std::vector<uint8_t> msg = server.Read();
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
            std::cout << response << std::endl;
        }
        else if (secure && input.at(0) == "UNQ") {
			try {
				response = "UNR ";
				if(std::find(avlangs.begin(), avlangs.end(), input.at(1)) != avlangs.end())
				{
					response = response + server.get_lang(input.at(1)).at(0);
			response = response + " " + server.get_lang(input.at(1)).at(1);
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
            std::cout << response << std::endl;
            server.Write(std::vector<uint8_t>(response.begin(), response.end()));
        }
    }
}

