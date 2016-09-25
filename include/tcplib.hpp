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


struct TCPException : public std::exception
{
   std::string s;
   TCPException(std::string ss) : s(ss) {}
   ~TCPException() throw () {}
   const char* what() const throw() { return s.c_str(); }
};


/**
 *  Class for a TCPChannel to soothe read/writes
 */
class TCPChannel {
private:
    int fd_;
    bool closed_;

    inline void check_closed(){ if(closed_) throw TCPException("Socket already closed"); }

public:

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


    /**
     *  Reads bytes from the socket onto a string. 
     *  Blocks until all the bytes are read, even if they are not yet on the file buffer.
     *   
     *  @param  count       
     *              the number of bytes to be read
     *  @throws TCPException
     */
    std::string read_from(size_t count) {

        check_closed();

        char buf[std::min(count,(size_t)4096)];

        while (count) {
            int nbytes = read(fd_,buf,std::min(count,(size_t)4096));
            if (nbytes == -1)   //TODO: Deal with this better (check errno)
                throw TCPException("Read failed\n");

            count -= nbytes;
        }

        return std::string(buf);
    }

    /**
     *  Writes a string (of bytes) to a socket.
     *  Blocks until all bytes are written.
     *
     *  @param  message
     *              the message to be written
     *  @throws TCPException
     */
    void write_in(const std::string& message) {

        check_closed();

        int written = 0;
        while ( (size_t)written != message.length()+1) {
            int nbytes = write(fd_,message.c_str() + written, (message.length()+1-written));
            if (nbytes == -1)   //TODO: Deal with this better (check errno)
                throw TCPException("Write failed\n");

            written += nbytes;
        }
    }

    void close_socket() {

        check_closed();

        if ( close(fd_) == -1 ) {
            throw TCPException("Error closing socket");
        }
        closed_ = true; 
    }


    ~TCPChannel() {
        if (closed_) return;

        try{
            close_socket();
        } 
        catch (TCPException e){}
    }

};


class TCPServer {
    int fd_;

public:
    // @throws TCPException
    TCPServer(u_short port) {
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

    /**
     *  Listens for a connection and returns a TCPChannel for communication with the client
     *
     *  @param  listen_queue
     *              the maximum number of of attempt of connections that can wait in the queue
     *  @returns
     *              a dedicated TCPChannel for communication with the client.
     *  @throws TCPException
     */
    TCPChannel await_connection(int listen_queue) {
        struct sockaddr_in clientaddr;
        int clientfd;
        socklen_t clientlen;
        if (listen(fd_, listen_queue)) {
            //TODO: Deal with this better (check errno)
            throw TCPException("Listen Failed");
        }

        clientlen=sizeof(clientaddr);
        clientfd=accept(fd_,(struct sockaddr*)&clientaddr, &clientlen);
        if (clientfd == -1) {
            //TODO: Deal with this better (check errno)
            throw TCPException("Could not accept connection");
        }
        return TCPChannel(clientfd);
    }

    //See TCPServer::Listen(inte backLog)
    TCPChannel await_connection() {return await_connection(10);}
};