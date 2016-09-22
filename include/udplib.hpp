#include <string>
#include <unistd.h>
#include <sys/types.h>

class UdpServer{
private:
    int _fd;
    struct sockaddr_in _serveraddr, _clientaddr;
    int _clientaddrlen;

public:
    UdpServer(int port);
    std::string wait_message(); //TODO: exception
    void answer_last(const std::string& message) ; //TODO: exception
    ~UdpServer();
};