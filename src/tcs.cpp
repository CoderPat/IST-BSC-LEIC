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

class TCS : public UDPServer{
private:
    std::map<std::string, int> _langs;
    std::string _langsfile = "langs.txt";
public:
    
    TCS(u_short port) : UDPServer(port) { 
        _langsfile = "langs.txt";
    }

    std::map<std::string, int> get_langs() {
        return _langs;
    }

    void  parse_avaliable_languages() {
        std::ifstream filetoparse;
        std::string lang;
        filetoparse.open(_langsfile.c_str());
        if(filetoparse.is_open()) {
            while (std::getline(filetoparse, lang)) {
                std::string langport = lang;
                lang = lang.substr(0, lang.find(' ', 0));
                _langs[lang] = atoi(langport.substr(langport.find(' ') + 1).c_str());
                std::cout << lang << "  " << _langs[lang] << std::endl;
            }
        }
    }

    std::vector<std::string>  get_avaliable_languages() {
        std::vector<std::string> out;
        for(std::map<std::string,int>::iterator it = _langs.begin(); it != _langs.end(); ++it) {
            out.push_back(it->first);
        }
        return out;
    }

    std::string uint8_tToString(std::vector<uint8_t> vec) {
        std::string out = "";
        for(int i = 0; i < vec.size(); i++) {
            out += vec[i];
        }
        return out;
    }

    ~TCS() {
        //close(_fd);
    }

    std::vector<std::string> tokenizer(std::string in) {

        std::vector<std::string> out;
        std::string buffer = "";
        for(int i = 0; i < in.length(); i++) {
            if(in[i] != ' ' && in[i] != '\n') {
                buffer += in[i];
            }
            else {
                out.push_back(buffer);
                buffer = "";
            }
        }
        std::cout << out[0] << std::endl;
        return out;
    }
};

int main(int argc, char* args[]) {


    TCS server(8112);
    server.parse_avaliable_languages();
    server.get_avaliable_languages();

    while(1) {
        std::vector<uint8_t> msg = server.Read();
        std::string msgstr = server.uint8_tToString(msg);
        std::vector<std::string> input = server.tokenizer(msgstr);
        std::vector<std::string> avlangs = server.get_avaliable_languages();
        std::string response = "";

        bool secure = (input.size() == 1 || input.size() == 2) ? true : false;

        if(secure && !strcmp("ULQ", input[0].c_str())) {
            for(int i = 0; i < avlangs.size(); i++) {
                response = response + avlangs[i] + " ";
            }
            std::cout << response << std::endl;
        }
        else if (secure && !strcmp("UNQ", input[0].c_str())) {
            
            if(std::find(avlangs.begin(), avlangs.end(), input[1]) != avlangs.end())
            {
                response = std::to_string(server.get_langs()[input[1]]);
            }
            else {
                response = "Language not supported";
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
