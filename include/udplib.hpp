#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/select.h>

#include <string>
#include <vector>
#include <exception>

#define DEFAULT_UDP_TIMEOUT_SEC 3  //TODO: Put as class parameters?
#define DEFAULT_UDP_TIMEOUT_USEC 0 //TODO: Put as class parameters?

struct UDPException : public std::exception
{
	std::string s;
	UDPException(std::string ss) : s(ss) {}
	~UDPException() throw () {}
	const char* what() const throw() { return s.c_str(); }
};

struct udp_socket_timeout : public UDPException{
    udp_socket_timeout(std::string ss) : UDPException(ss){};
    ~udp_socket_timeout() throw(){}
};


class UDPConnection{
protected:
	int fd_;
	bool closed_;
	struct sockaddr_in address_;
	socklen_t addrlen_;

    struct timeval timeout_;

	inline void check_closed(){ if(closed_) throw UDPException("Socket already closed"); }
    inline void check_timout(){
        fd_set set;
        FD_ZERO(&set);
        FD_SET(fd_, &set);
        timeout_.tv_sec = DEFAULT_UDP_TIMEOUT_SEC;
        timeout_.tv_usec = DEFAULT_UDP_TIMEOUT_USEC;

        if(!select(FD_SETSIZE, &set, NULL, NULL, &timeout_)) throw udp_socket_timeout("");
    }

public:
	UDPConnection() : closed_(false){
		fd_ = socket(AF_INET,SOCK_DGRAM,0);
		if (fd_ == -1) 
            throw UDPException("Could not create socket");
 
	}

    /** Move constructor to avoid the original object closing the socket for the new one */
    UDPConnection(UDPConnection&& c) : fd_(c.fd_), address_(c.address_), addrlen_(addrlen_), 
                                       closed_(c.closed_), timeout_(c.timeout_){
        c.closed_ = true;
    }

    UDPConnection& operator=(UDPConnection&& c){
        timeout_ = c.timeout_;
        address_ = c.address_;
        addrlen_ = c.addrlen_;
        fd_ = c.fd_;
        closed_ = c.closed_;
        c.closed_ = true;
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

        check_timout();
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
    		throw UDPException("Could not send data"   + std::to_string(errno)); //TODO: Deal with this better (check errno)
    }

    void Close() {
        check_closed();

        if ( close(fd_) == -1 ) {
            throw UDPException("Error closing socket"); //TODO: Deal with this better (check errno)
        }
        closed_ = true; 
    }

    virtual ~UDPConnection() = 0;
};

UDPConnection::~UDPConnection(){
    if (closed_) return;
    try{
        Close();
    } 
    catch (UDPException e){}
}


class UDPChannel : public UDPConnection{
public:

    UDPChannel() { closed_ = true; } 

	UDPChannel(const std::string& host, u_short port){
		struct hostent* hostptr;

		hostptr=gethostbyname(host.c_str());
		if (hostptr==NULL) 
            throw UDPException("Invalid Hostname");

		memset((void*)&address_, 0,sizeof(address_));
		address_.sin_family=AF_INET;
		address_.sin_addr.s_addr=((struct in_addr *)(hostptr->h_addr_list[0]))->s_addr;
		address_.sin_port=htons(port);
		addrlen_ = sizeof(address_); 
	}
};

class UDPServer : public UDPConnection{
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