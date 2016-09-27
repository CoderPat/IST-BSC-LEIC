#include <pthread.h>

#include <vector>
#include <string>

#include "tcplib.hpp"
#include "udplib.hpp"
#include "utils.hpp"

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

		std::vector<string> tokens = tokenize(response);

        if(tokens.at(0) != expected_response) throw invalid_argument("Unknown response");
        if(tokens.at(1) == "NOK") throw //TODO
        if(tokens.at(1) == "ERR") throw //TODO
        
    }

public:
	TCPServer(std::string language,
			  ushort_t my_port, 
			  const std::string& tcs_hostname, 
			  ushort_t tcs_port) : port(my_port), language_(language), server_(my_port), tcs_channel(hostname, tcs_port) {
	}

	void SRG(){
		_status_update("SRG", "SRR");
	}

	void SUN(){
		_status_update("SUN", "SUR");
	}

	void TRR(){

        TCPChannel user_channel = server_.Listen();
        std::string request = string_cast(user_channel.ReadLine(6))
        std::vector<uint_8> response;
        std::vector<std::string> initial_tokens = tokenize(request);

        if(tokens.at(0) != "TRQ") throw //TODO
        if(tokens.at(1) == "t"){
            int number_of_words = std::stoi(string_cast(tcs_channel.ReadUntil(' ')));

            std::vector<std::string> original_words = tokenize(string_cast(tcs_channel.ReadUntil('\n')));
            std::vector<std::string> translated_words;
            for(int i=0; i < number_of_words; i++)
                translated_words.push_back(TranslateWord(original_words.at(i)));

            response = byte_cast("TRR t" + std::to_string(translated_words.size()) + 
                                  " " + detokenize(translated_words) + "\n");
        }
        else if (tokens.at(1) == "f"){
            //TODO: TRANSLATE FILES
        }
        else{
            //TODO: ERRORS
        }
        user_channel.Write(response)
	}

    std::string TranslateWord(std::string word){
        //TODO: Actual translation
        return word;
    }
};


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
            trs_port = std::stoi(argv[i])
        } else if (!strcmp("-n", argv[i])) {
            i++;
            hostname = string(argv[i]);
        } else if (!strcmp("-e", argv[i])) {
            i++;
            tcs_port = std::stoi(argv[i]);
        }
        cerr << "Unknow option: " << argv[i] << endl;
    }


}
