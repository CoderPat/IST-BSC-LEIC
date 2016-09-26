#include <pthread.h>

#include <vector>
#include <string>

#include "tcplib.hpp"
#include "udplib.hpp"

inline std::vector<uint8_t> byte_cast(const std::string& str){
	return std::vector<uint8_t>(str.begin(), str.end());
}

inline std::vector<uint8_t> string_cast(const std::vector<uint8_t>& bytes){
	return std::string(bytes.begin(), bytes.end());
}


class TRSInterface{
private:
    TCPServer server_;
    UDPChannel tcs_channel_;
    ushort_t port_;
    std::string language_;

    void _status_update(const std::string& request, const std::string& expected_response){
    	std::string message = request + " " + lang_ + " " + gethostname() + " " + port_ + "\n";

		tcs_channel_.Write(byte_cast(message));
		std::string response = string_cast(tcs_channel_.Read());

		std::vector<string> tokens;
        std::string aux;
        std::stringstream ss(response);
        while (ss >> aux)
            tokens.push_back(aux);

        if(tokens.at(0) != expected_response) throw invalid_argument("Unknown response");
        if(tokens.at(1) == "NOK") throw //TODO
        if(tokens.at(1) == "ERR") throw //TODO
    }

public:
	TCPServer(std::string language,
			  ushort_t my_port, 
			  const std::string& tcs_hostname, 
			  ushort_t tcs_port) : port(my_port), language_(language), server(my_port), tcs_channel(hostname, tcs_port) {
	}

	void SRG(){
		_status_update("SRG", "SRR");
	}

	void SUN(){
		_status_update("SUN", "SUR");
	}

	void TRR(){
	}

};


void keyboard_processing(){
    std::string line;
    //Connect to server
    while(getline(cin,line)) {
        vector<string> tokens;
        string aux;
        stringstream ss(line);
        while (ss >> aux) 
            tokens.push_back(aux);

        if (tokens.size()==0) continue; //empty line

        if (tokens[0] == "exit"){
            return; // TODO: Send message to TRC warning of disconnection
        }
}

int main(int argc, char **argv) {
    ushort_t trs_port = 59000 + GN;
    ushort_t tcs_port = 58000 + GN; 
    string hostname = string("localhost");

    if(argc < 2){
        cerr << "Usage <language> [-p <trs_port>] [-n <hostname>] [-e <tcs_port>]"
        exit(1)
    }
    std::string language(argv[1])
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
