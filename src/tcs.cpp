#include <unistd.h>
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
/*
UdpServer::UdpServer(int port, std::string langsfile){
    _fd = socket(AF_INET,SOCK_DGRAM,0);

    _serveraddr.sin_family=AF_INET;
    _serveraddr.sin_addr.s_addr=htonl(INADDR_ANY);
    _serveraddr.sin_port=htons((u_short)port);
    _langsfile = langsfile;

    bind(_fd,(struct sockaddr*)&_serveraddr, sizeof(_serveraddr));
}

std::string UdpServer::wait_message(){
    char buffer[4096];

    _clientaddrlen = sizeof(_clientaddr);
    recvfrom(_fd,buffer,sizeof(buffer),0,(struct sockaddr*)&_clientaddr,(socklen_t*)&_clientaddrlen);

    return std::string(buffer);
}

void UdpServer::answer_last(const std::string& message){
    sendto(_fd, &message[0], message.length() + 1, 0, (struct sockaddr*)&_clientaddr, _clientaddrlen);
}

void  UdpServer::parse_avaliable_languages() {
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

std::vector<std::string>  UdpServer::get_avaliable_languages() {
    std::vector<std::string> out;
    for(std::map<std::string,int>::iterator it = _langs.begin(); it != _langs.end(); ++it) {
        out.push_back(it->first);
    }
	return out;
}

UdpServer::~UdpServer(){
    close(_fd);
}*/

int main(int argc, char* args[]) {

    if(argc != 3) {
        std::cout << "Usage: TCS <port> <langs.txt>" << std::endl;
        return 0;
    }

    UDPServer server(8090);
    //UdpServer server(atoi(args[1]), args[2]);
    //server.parse_avaliable_languages();
    //server.get_avaliable_languages();

    std::vector<uint8_t> msg = server.Read();

    std::string a = "asd";
    
    server.Write(std::vector<uint8_t>(a.begin(), a.end()));
}
