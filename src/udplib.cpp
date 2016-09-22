#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string>
#include "../include/udp_server.hpp"

UdpServer::UdpServer(int port){
    _fd = socket(AF_INET,SOCK_DGRAM,0);

    _serveraddr.sin_family=AF_INET;
    _serveraddr.sin_addr.s_addr=htonl(INADDR_ANY);
    _serveraddr.sin_port=htons((u_short)port);

    bind(_fd,(struct sockaddr*)&_serveraddr, sizeof(_serveraddr));
}

std::string UdpServer::wait_message(){
    char buffer[4096];

    unsigned int _clientaddrlen = sizeof(_clientaddr);
    recvfrom(_fd,buffer,sizeof(buffer),0,(struct sockaddr*)&_clientaddr,&_clientaddrlen);

    return std::string(buffer);
}

void UdpServer::answer_last(const std::string& message){
    sendto(_fd, &message[0], message.length() + 1, 0, (struct sockaddr*)&_clientaddr, _clientaddrlen);
}

UdpServer::~UdpServer(){
    close(_fd);
}

int main(){
    UdpServer server(80);
    std::string msg = server.wait_message();
    server.answer_last("fy niggah");
}