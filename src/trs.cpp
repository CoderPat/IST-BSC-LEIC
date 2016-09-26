#include <tcplib.hpp>
#include <udplib.hpp>

class TRSInterface{
private:
    TCPServer server
}

int main(int argc, char **argv) {
    ushort_t trs_port = 59000 + GN;
    ushort_t tcs_port = 58000 + GN; 
    string hostname = string("localhost");

    if(argc < 2){
        cerr << "Usage <language> [-p <trs_port>] [-n <hostname>] [-e <tcs_port>]"
        exit(1)
    }
    std::string lang(argv[1])
    for (int i = 2; i<argc-1; i++) {
        if (!strcmp("-p", argv[i])){
            i++;
            trs_port = stoi(argv[i])
        } else if (!strcmp("-n", argv[i])) {
            i++;
            hostname = string(argv[i]);
        } else if (!strcmp("-e", argv[i])) {
            i++;
            tcs_port = stoi(argv[i]);
        }
        cerr << "Unknow option: " << argv[i] << endl;
    }
}
