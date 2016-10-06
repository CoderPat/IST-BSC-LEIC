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
#include <exception>
#include <vector>
#include <fstream>

#define DEFAULT_TCP_TIMEOUT_SEC 3  //TODO: Put as class parameters?
#define DEFAULT_TCP_TIMEOUT_USEC 0 //TODO: Put as class parameters?

struct TCPException : public std::exception
{
   std::string s;
   TCPException(std::string ss) : s(ss) {}
   ~TCPException() throw () {}
   const char* what() const throw() { return s.c_str(); }
};

struct tcp_socket_timeout : public TCPException{
    tcp_socket_timeout(std::string ss) : TCPException(ss){};
    ~tcp_socket_timeout() throw(){}
};

struct other_inputs_available : public TCPException{
    other_inputs_available(std::string ss) : TCPException(ss){};
    ~other_inputs_available() throw(){}
};


/**
 *  Class for a TCPChannel to soothe read/writes
 */
class TCPChannel {
private:
    int fd_;
    bool closed_;

    struct timeval timeout_;

    inline void check_closed(){ if(closed_) throw TCPException("Socket already closed"); }
    inline int check_timout(){
        fd_set set;
        FD_ZERO(&set);
        FD_SET(fd_, &set);
        timeout_.tv_sec = DEFAULT_TCP_TIMEOUT_SEC;
        timeout_.tv_usec = DEFAULT_TCP_TIMEOUT_USEC;
        int ret = select(FD_SETSIZE, &set, NULL, NULL, &timeout_);
        if(!ret) throw tcp_socket_timeout("");
    }

public:

    /** Creates an empty TCPChannel*/
    TCPChannel() : fd_(-1), closed_(true) {}

    /** Creates a TCPChannel on an already open socket, no error checking is performed */
    TCPChannel(int fd) : fd_(fd), closed_(false) {} 
    
    /**
     *  Creates a TCPChannel on a new socket
     *
     *  @param  hostname     
     *              the name of the server for dns retrieving
     *  @param  port         
     *              port open for communication on the server
     *  @throws TCPException
     */
    TCPChannel(const std::string& hostname, u_short port) : closed_(false) {
        struct hostent* hostptr;
        struct sockaddr_in serveraddr;

        fd_=socket(AF_INET,SOCK_STREAM,0);
        if (fd_ == -1) 
            throw TCPException("Could not create socket");

        hostptr=gethostbyname(hostname.c_str());
        if (hostptr==NULL)
            throw TCPException("Invalid Hostname");

        memset((void*)&serveraddr, 0, sizeof(serveraddr));
        serveraddr.sin_family=AF_INET; 
        serveraddr.sin_addr.s_addr=((struct in_addr *)(hostptr->h_addr_list[0]))->s_addr;
        serveraddr.sin_port=htons(port);

        if (connect(fd_,(struct sockaddr*)&serveraddr, sizeof(serveraddr))) {
            //TODO: Deal with this better (check errno)
            throw TCPException("Connection failed");
        }
    }

    /** Move constructor to avoid the original object closing the socket for the new one */
    TCPChannel(TCPChannel&& c) : fd_(c.fd_), closed_(c.closed_), timeout_(c.timeout_) {
        c.closed_ = true;
    }

    /** Move assignment operator to avoid the original object closing the socket for the new one */
    TCPChannel& operator=(TCPChannel&& c){
        timeout_ = c.timeout_;
        fd_ = c.fd_;
        closed_ = c.closed_;
        c.closed_ = true;
    }


    
    /**
     *  Reads an amount of bytes. 
     *  Blocks until all the bytes are read, even if they are not yet on the file buffer.
     *   
     *  @param  count       
     *              the number of bytes to be read
     *  @throws TCPException
     */
    std::vector<uint8_t> Read(size_t count) {

        check_closed();

        std::vector<uint8_t> ret;
        char buf[std::min(count,(size_t)4096)];

        while (count) {
            check_timout();

            int nbytes = read(fd_,buf,std::min(count,(size_t)4096));
            if (nbytes == -1)   //TODO: Deal with this better (check errno)
                throw TCPException("Read failed\n");

            for (size_t i=0; i<(size_t)nbytes; i++)
                ret.push_back(buf[i]);

            count -= nbytes;
        }

        return ret;
    }

    std::string Read(size_t count, const std::string& overrider) {
        std::vector<uint8_t> aux = Read(count);
        return std::string(aux.begin(), aux.end());
    }

    std::vector<uint8_t> ReadUntil(uint8_t term) {
        check_closed();

        std::vector<uint8_t> ret;
        uint8_t c;
        while((c=Read(1)[0]) != term) {
            ret.push_back(c);
        }
        return ret;
    }
    
    std::string ReadUntil(uint8_t term, const std::string& overrider) {
        std::vector<uint8_t> aux = ReadUntil(term);
        return std::string(aux.begin(), aux.end());
    }

    /**
     *  Writes a vector of bytes to a socket.
     *  Blocks until all bytes are written.
     *
     *  @param  byte_array
     *              the bytes to be written
     *  @throws TCPException
     */
    void Write(const std::vector<uint8_t>& byte_array) {
        check_closed();
        
        int written = 0;
        while ( (size_t)written != byte_array.size()) {
            int nbytes = write(fd_, byte_array.data() + written, (byte_array.size()-written));
            if (nbytes == -1)   //TODO: Deal with this better (check errno)
                throw TCPException("Write failed\n");

            written += nbytes;
        }
    }

    void Write(const std::string& s) {
        Write(std::vector<uint8_t>(s.begin(), s.end()));
    }

    void Write(std::ifstream& s) {
        check_closed();
        char buf[2];
        while(s.get(buf[0])) {
            uint8_t c = buf[0];
            while (1) {
                int nbytes = write(fd_, &c, 1);
                if (nbytes == -1)   //TODO: Deal with this better (check errno)
                    throw TCPException("Write failed\n");
                if (nbytes == 1) 
                    break;
            }
        }
    }

    void Close() {

        check_closed();

        if ( close(fd_) == -1 ) {
            throw TCPException("Error closing socket");
        }
        closed_ = true; 
    }


    ~TCPChannel() {
        if (closed_) return;

        try{
            Close();
        } 
        catch (TCPException e){}
    }
};


class TCPServer {
    int fd_;
    bool closed_;

    inline void check_closed(){ if(closed_) throw TCPException("Socket already closed"); }

public:
    TCPServer() : closed_(true) {} 

    // @throws TCPException
    TCPServer(u_short port) : closed_(false) {
        struct sockaddr_in serveraddr;
        fd_=socket(AF_INET,SOCK_STREAM,0);
        if (fd_ == -1) 
            throw TCPException("Could not create socket");

        memset((void*)&serveraddr,0, sizeof(serveraddr));
        serveraddr.sin_family=AF_INET;
        serveraddr.sin_addr.s_addr=htonl(INADDR_ANY); 
        serveraddr.sin_port=htons(port);

        if (bind(fd_,(struct sockaddr*)&serveraddr, sizeof(serveraddr)) == -1) 
            throw TCPException("Could not bind to port");                  
    }

    /** Move constructor to avoid the original object closing the socket for the new one */
    TCPServer(TCPServer&& c) : fd_(c.fd_), closed_(c.closed_) {
        c.closed_ = true;
    }

    TCPServer& operator=(TCPServer&& c){
        fd_ = c.fd_;
        closed_ = c.closed_;
        c.closed_ = true;
    }

    /**
     *  Listens for a connection and returns a TCPChannel for communication with the client
     *
     *  @param  listen_queue
     *              the maximum number of of attempt of connections that can wait in the queue
     *  @returns
     *              a dedicated TCPChannel for communication with the client.
     *  @throws TCPException
     */
    TCPChannel Listen(int listen_queue, fd_set other_inputs) {
        check_closed();

        struct sockaddr_in clientaddr;
        int clientfd;
        socklen_t clientlen;
        if (listen(fd_, listen_queue)) {
            throw TCPException("Listen Failed");
        }

        clientlen=sizeof(clientaddr);

        //Check for other input availability
        FD_SET(fd_, &other_inputs);
        int ready_n = select(FD_SETSIZE, &other_inputs, NULL, NULL, NULL);
        if(!FD_ISSET(fd_, &other_inputs) || ready_n > 1)
            throw other_inputs_available("");

        clientfd=accept(fd_,(struct sockaddr*)&clientaddr, &clientlen);
        if (clientfd == -1) {
            throw TCPException("Could not accept connection");
        }
        return TCPChannel(clientfd);
    }

    TCPChannel Listen(fd_set set){return Listen(10, set);}
    //See TCPServer::Listen(inte backLog)
    TCPChannel Listen() { 
        fd_set set; 
        FD_ZERO(&set);
        return Listen(10, set); 
    }

    void Close() {
        check_closed();

        if ( close(fd_) == -1 ) {
            throw TCPException("Error closing socket");
        }
        closed_ = true; 
    }


    ~TCPServer() {
        if (closed_) return;

        try{
            Close();
        } 
        catch (TCPException e){}
    }
};