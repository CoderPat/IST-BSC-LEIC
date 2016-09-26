#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <string>
#include <vector>
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

	inline void check_closed(){ if(closed_) throw UDPException("Socket already closed"); }

public:
	UPDConnection() : closed_(false){
		fd_ = socket(AF_INET,SOCK_DGRAM,0);
		if (fd_ == -1) 
            throw UDPException("Could not create socket");
 
	}

    /**
     *  Reads a UDP packet onto a byte container
     *	Blocks until a packet is received
	 *
     *  @throws TCPException
     */
    std::vector<uint8_t> Read(){
    	check_closed();

    	std::vector<uint8_t> ret;
	    char buffer[4096];
	    
	    addrlen_ = sizeof(address_);

	    int nbytes = recvfrom(fd_,buffer,sizeof(buffer),0,(struct sockaddr*)&address_,&addrlen_);
	    if (nbytes == -1)
	    	throw UDPException("Error receiving a message"); //TODO: Deal with this better (check errno)

	    for (size_t i=0; i<(size_t)nbytes; i++)
            ret.push_back(buffer[i]);

	    return ret;
    } 

     /**
     *  Writes a vector of bytes to a socket.
     *  Blocks until all bytes are written.
     *
     *  @param  byte_array
     *              the bytes to be written
     *  @throws UDPException
     */
    void Write(const std::vector<uint8_t>& byte_array){ 
    	check_closed();

    	if( sendto(fd_, byte_array.data(), byte_array.size(), 0, (struct sockaddr*)&address_,addrlen_) == -1)
    		throw UDPException("Could not send data"); //TODO: Deal with this better (check errno)
    }

    void Close() {
        check_closed();

        if ( close(fd_) == -1 ) {
            throw UDPException("Error closing socket"); //TODO: Deal with this better (check errno)
        }
        closed_ = true; 
    }

    virtual ~UPDConnection() = 0;
};

UPDConnection::~UPDConnection(){
    if (closed_) return;

    try{
        Close();
    } 
    catch (UDPException e){}
}


class UPDChannel : public UPDConnection{
public:
	UPDChannel(const std::string& host, u_short port){
		struct hostent* hostptr;

		hostptr=gethostbyname(host.c_str());
		if (hostptr==NULL) 
            throw UDPException("Invalid Hostname");

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