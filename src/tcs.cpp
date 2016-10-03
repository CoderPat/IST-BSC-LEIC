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

class TCS : public UDPServer{
private:
    std::map<std::string, std::map<std::string, int>> _langs;
    std::string _langsfile = "langs.txt";
public:
    
    TCS(u_short port) : UDPServer(port) { 
        _langsfile = "langs.txt";
    }

    std::map<std::string, std::map<std::string, int>> get_langs() {
        return _langs;
    }

    std::vector<std::string> get_lang(std::string lang) {
	std::vector<std::string> out;
	for(std::map<std::string, int>::iterator it = _langs.at(lang).begin(); it != _langs.at(lang).end(); ++it) {
            out.push_back(it->first);
	    out.push_back(std::to_string(it->second));
        }
	return out;
    }

    void add_language(std::string lang, std::string ip, std::string port) {
        _langs.at(lang).at(ip) = std::stoi(port.c_str());
    }

    void remove_language(std::string lang, std::string ip, std:: string port) {
        if(_langs.at(lang).at(ip) == std::stoi(port.c_str())) {
            _langs.erase(lang);
        }
    }

    void  parse_avaliable_languages() {
        std::ifstream filetoparse;
        std::string lang;
        filetoparse.open(_langsfile.c_str());
        if(filetoparse.is_open()) {
            while (std::getline(filetoparse, lang)) {
                std::vector<std::string> inputs = tokenize(lang);
                _langs.at(inputs.at(0)).at(inputs.at(1)) = stoi(inputs.at(2).c_str());
                std::cout << inputs.at(0) << "  " << inputs.at(1) <<  " " << _langs.at(inputs.at(0)).at(inputs.at(1)) << std::endl;
            }
        }	
    }

    std::vector<std::string>  get_avaliable_languages() {
        std::vector<std::string> out;
        for(std::map<std::string, std::map<std::string, int>>::iterator it = _langs.begin(); it != _langs.end(); ++it) {
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

        bool secure = (input.size() == 1 || input.size() == 2 || input.size() != 4);
        if(input.size() && !strcmp("ULQ", input.at(0).c_str())) {
            response = "ULR " + std::to_string(avlangs.size()) + " ";
            for(int i = 0; i < avlangs.size(); i++) {
                response = response + avlangs.at(i) + " ";
            }
            std::cout << response << std::endl;
        }
        else if (input.size() == 2 && !strcmp("UNQ", input.at(0).c_str())) {
            response = "UNR ";
            if(std::find(avlangs.begin(), avlangs.end(), input.at(1)) != avlangs.end())
            {
                response = response + server.get_lang(input.at(1)).at(0);
		response = response + " " + server.get_lang(input.at(1)).at(1);
            }
            else {
                response = "Language not supported";
            }
        }
        else if (input.size() == 4 && !strcmp("SRG", input.at(0).c_str())) {
		try {
			server.add_language(input.at(1), input.at(2), input.at(3));		
			response = "SRR OK";
		}
		catch (int e) {
			response = "SRR NOK";		
		}		
        }
	else if (input.size() == 4 && !strcmp("SUN", input.at(0).c_str())) {
		try {
			server.remove_language(input.at(1), input.at(2), input.at(3));		
			response = "SUR OK";
		}
		catch (int e) {
			response = "SUR NOK";		
		}		
        }
        else {
            response = "Error";
        }
        if(secure) {
            std::cout << response << std::endl;
            server.Write(std::vector<uint8_t>(response.begin(), response.end()));
        }
    }
}

