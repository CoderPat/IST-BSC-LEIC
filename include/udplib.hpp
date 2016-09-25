#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <string>
#include <exception>

struct UDPException : public std::exception
{
	std::string s;
	UDPException(std::string ss) : s(ss) {}
	~UDPException() throw () {}
	const char* what() const throw() { return s.c_str(); }
};



class UPDConnection{
protected:
	int fd_;
	bool closed_;
	struct sockaddr_in address_;
	socklen_t addrlen_;

	inline void check_closed(){ if(closed_) throw TCPException("Socket already closed"); }

public:
	UPDConnection() : closed_(false){
		fd_ = socket(AF_INET,SOCK_DGRAM,0);
		if (fd_ == -1) 
            throw UDPException("Could not create socket");
 
	}

    /**
     *  Reads bytes from the socket onto a string.
     *	Blocks until a package is received
	 *
     *  @throws TCPException
     */
    std::string read_from(){
    	check_closed();

	    char buffer[4096];

	    addrlen_ = sizeof(address_);
	    if (recvfrom(fd_,buffer,sizeof(buffer),0,(struct sockaddr*)&address_,&addrlen_) == -1)
	    	throw UDPException("Error receiving a message");

	    return std::string(buffer);
    } 

     /**
     *  Writes a string (of bytes) to a socket.
     *  Blocks until all bytes are written.
     *
     *  @param  message
     *              the message to be written
     *  @throws TCPException
     */
    void write_in(const std::string& message){ 
    	check_closed();

    	if( sendto(fd_, message.c_str(), message.length() + 1, 0, (struct sockaddr*)&address_,addrlen_) == -1)
    		throw UDPException("Could not send message");
    }

    void close_socket() {
        check_closed();

        if ( close(fd_) == -1 ) {
            throw TCPException("Error closing socket");
        }
        closed_ = true; 
    }

    virtual ~UPDConnection() = 0;
};

UPDConnection::~UPDConnection(){
    if (closed_) return;

    try{
        close_socket();
    } 
    catch (TCPException e){}
}


class UPDChannel : public UPDConnection{
public:
	UPDChannel(const std::string& host, u_short port){
		struct hostent* hostptr;

		hostptr=gethostbyname(host.c_str());
		if (hostptr==NULL) 
            throw TCPException("Invalid Hostname");

		memset((void*)&address_, 0,sizeof(address_));
		address_.sin_family=AF_INET;
		address_.sin_addr.s_addr=((struct in_addr *)(hostptr->h_addr_list[0]))->s_addr;
		address_.sin_port=htons(port);
	}
};

class UDPServer : public UPDConnection{
public:
    UDPServer(u_short port){
    	struct sockaddr_in serveraddr;

	    memset((void*)&serveraddr, 0, sizeof(serveraddr));
	    serveraddr.sin_family=AF_INET;
	    serveraddr.sin_addr.s_addr=htonl(INADDR_ANY);
	    serveraddr.sin_port=htons(port);

	    if(bind(fd_,(struct sockaddr*)&serveraddr, sizeof(serveraddr)) == -1)
	    	throw UDPException("Could not bind to port");
    }
};