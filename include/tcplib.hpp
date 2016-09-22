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

typedef std::vector<uint8_t> ByteVector; //XXX: Should this be ByteVector or ByteArray?

struct TCPException : public std::exception
{
   std::string s;
   TCPException(std::string ss) : s(ss) {}
   ~TCPException() throw () {}
   const char* what() const throw() { return s.c_str(); }
};
struct TCPServerException : public TCPException{
   TCPServerException(std::string ss) : TCPException(ss) {}
   ~TCPServerException() throw () {}
};
struct TCPChannelException : public TCPException{
   TCPChannelException(std::string ss) : TCPException(ss) {}
   ~TCPChannelException() throw () {}
};

/*
    Class for a TCPChannel to soothe read/writes
*/
class TCPChannel {
    int fd;
    bool invalidated;
    bool opened;

    void _CheckTalk() {
        if (invalidated) {
            throw TCPChannelException("Channel is in invalid state\n");
        }
        if (!opened) {
            throw TCPChannelException("Channel is closed\n");
        }
    }

    /*
        Desc: Closes the socket file
        Return code: {
            0 => ok,
            1 => socket was already closed,
            2 => EBADF,
            3 => EIO,
            4 => too many close retries on EINTR
        }
    */
    int _Close() {
        if (!opened) return 1;
        int rc;
        if ( (rc = close(fd)) ) {
            //TODO: Deal with this better (check errno). We dont throw because of the desctructor
            return 3;
        }
        return 0;
    }

public:

    //Creates a TCPChannel on an already open socket, no error checking is performed
    TCPChannel(int fd) : fd(fd), invalidated(false), opened(true) {} 
    
    /*
        Creates a TCPChannel on a new socket 
        @throws TCPChannelException
        XXX: gethostbyname is deprecated!!! We should ask the lecturer if we are required to use it
    */
    TCPChannel(const char * hostname, u_short port) : invalidated(true), opened(false) {
        struct hostent* hostptr;
        struct sockaddr_in serveraddr;
        fd=socket(AF_INET,SOCK_STREAM,0);
        if (fd == -1) {
            throw TCPServerException("Could not create socket");
        }
        opened = true;

        hostptr=gethostbyname(hostname);
        if (hostptr==NULL) {
            throw TCPChannelException("Invalid Hostname");
        }
        memset((void*)&serveraddr, 0, sizeof(serveraddr));
        serveraddr.sin_family=AF_INET; 
        serveraddr.sin_addr.s_addr=((struct in_addr *)(hostptr->h_addr_list[0]))->s_addr;
        serveraddr.sin_port=htons(port);
        if (connect(fd,(struct sockaddr*)&serveraddr, sizeof(serveraddr))) {
            //TODO: Deal with this better (check errno)
            throw TCPChannelException("Connection failed");
        }
        invalidated = false;
    }

    ~TCPChannel() {
        _Close();
    }

    /*
        Desc: Reads count bytes from socket. Blocks until all the bytes are read, even if they are not yet on the file buffer.
        @sync
        @basic exception safety
        @throws TCPChannelException, std::bad_alloc
    */
    ByteVector ReadBytes(size_t count) {
        _CheckTalk();
        ByteVector ret;
        unsigned char buf[std::min(count,(size_t)4096)];
        while (count) {
            int nbytes = read(fd,buf,std::min(count,(size_t)4096));
            if (nbytes == -1)  {
                //TODO: Deal with this better (check errno)
                throw TCPChannelException("Read failed\n");
            } else {
                for (size_t i=0; i<(size_t)nbytes; i++)
                    ret.push_back(buf[i]); //@throws bad_alloc
                count -= nbytes;
            }
        }
        return ret;
    }

    /*
        Desc: Writes a ByteVector to a socket, blocks until all bytes are written
        @sync
        @basic exception safety
        @throws TCPChannelException
    */
    void WriteBytes(const ByteVector& buf) {
        _CheckTalk();
        int written = 0;
        while ((size_t)written != buf.size()) {
            int nbytes = write(fd,buf.data() + written, (buf.size()-written));
            if (nbytes == -1)  {
                //TODO: Deal with this better (check errno)
                throw TCPChannelException("Read failed\n");
            } else {
                written += nbytes;
            }
        }
    }

    void Close() {
        if (_Close() > 1) {
            throw TCPChannelException("Could not close socket");
        }
    }
};


class TCPServer {
    int fd;

public:
    // @throws TCPServerException
    TCPServer(u_short port) {
        struct sockaddr_in serveraddr;
        fd=socket(AF_INET,SOCK_STREAM,0);
        if (fd == -1) {
            throw TCPServerException("Could not create socket");
        }
        memset((void*)&serveraddr,0, sizeof(serveraddr));
        serveraddr.sin_family=AF_INET;
        serveraddr.sin_addr.s_addr=htonl(INADDR_ANY); 
        serveraddr.sin_port=htons(port);
        if (bind(fd,(struct sockaddr*)&serveraddr, sizeof(serveraddr))) {
            throw TCPServerException("Could not bind to port");           
        }
    }

    /*
        Desc: Listens for a connection and returns a TCPChannel in case of success
        @sync
        @basic exception safety
        @throws TCPServerException
    */
    TCPChannel Listen(int backLog) {
        struct sockaddr_in clientaddr;
        int clientfd;
        socklen_t clientlen;
        if (listen(fd,backLog)) {
            //TODO: Deal with this better (check errno)
            throw TCPServerException("Listen Failed");
        }
        clientlen=sizeof(clientaddr);
        clientfd=accept(fd,(struct sockaddr*)&clientaddr, &clientlen);
        if (clientfd == -1) {
            //TODO: Deal with this better (check errno)
            throw TCPServerException("Could not accept connection");
        }
        return TCPChannel(clientfd);
    }
    //See TCPServer::Listen(inte backLog)
    TCPChannel Listen() {return Listen(10);}
};