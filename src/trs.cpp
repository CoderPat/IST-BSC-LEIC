#include <unistd.h>
#include <limits.h>

#include <vector>
#include <string>
#include <exception> 
#include <iostream>
#include <fstream>
#include <map>

#include "tcplib.hpp"
#include "udplib.hpp"
#include "utils.hpp"

class TRSInterface{
private:
    TCPServer server_;
    UDPChannel tcs_channel_;
    u_short port_;
    std::string language_;
    std::map<std::string, std::string> word_translator;
    std::map<std::string, std::string> file_translator;

    /** 
     * 	Send a arbitray request and interprets the response 
	 *	Compactation of the SRG and SUN, which are the same with different initial signals
     */
    void _status_update(const std::string& request, const std::string& expected_response){

    	char myhostname[HOST_NAME_MAX];
    	gethostname(myhostname, HOST_NAME_MAX); //TODO: Error Checking
    	std::string message = request + " " + language_ + " " + myhostname + " " + std::to_string(port_) + "\n";

		tcs_channel_.Write(byte_cast(message));
		std::string response = string_cast(tcs_channel_.Read());

		std::vector<std::string> tokens = tokenize(response);

        if(tokens.at(0) != expected_response) throw std::exception();
        if(tokens.at(1) == "NOK") throw std::exception();  //TODO: right exception
        if(tokens.at(1) == "ERR") throw std::exception();  //TODO: right exception   
    }


public:

	void LoadTranslators(const std::string& word_file_path, const std::string& img_file_path){
    	word_translator = std::map<std::string, std::string>();
    	file_translator = std::map<std::string, std::string>();

    	std::ifstream translation_file;
    	std::string original, translated;

    	//Assumes good format of the translation files
    	translation_file.open(word_file_path, std::ios::in);
    	while(translation_file >> original){
    		translation_file >> translated;
    		word_translator.insert(std::make_pair(original, translated));
    	}
    	translation_file.close();

    	translation_file.open(img_file_path, std::ios::in);
    	while(translation_file >> original){
    		translation_file >> translated;
    		file_translator.insert(std::make_pair(original, translated));
    	}
    	translation_file.close();
    }

	TRSInterface(std::string language,
			  	 u_short my_port, 
			  	 const std::string& tcs_hostname, 
			  	 u_short tcs_port) : port_(my_port), language_(language), server_(my_port), tcs_channel_(tcs_hostname, tcs_port) {
		LoadTranslators("text_translation.txt", "file_translation.txt"); //TODO: Constant file names out of the class
	}

	void SRG(){
		_status_update("SRG", "SRR");
	}

	void SUN(){
		_status_update("SUN", "SUR");
	}

	void TRR(){

        TCPChannel user_channel = server_.Listen();

        std::string request = string_cast(user_channel.ReadUntil(' '));
        if(request != "TRQ") throw std::exception(); //TODO: right exception

        request = string_cast(user_channel.ReadUntil(' '));
        if(request == "t"){
            int number_of_words = std::stoi(string_cast(user_channel.ReadUntil(' ')));

            std::vector<std::string> original_words = tokenize(string_cast(user_channel.ReadUntil('\n')));
            std::vector<std::string> translated_words;

            for(int i=0; i < number_of_words; i++)
                translated_words.push_back(TranslateWord(original_words.at(i)));

            user_channel.Write("TRR t ");
            user_channel.Write(std::to_string(translated_words.size()) + " " + detokenize(translated_words));
            user_channel.Write("\n");
        }
        else if (request == "f"){
            std::string filename = string_cast(user_channel.ReadUntil(' '));
            size_t byte_size = std::stol(string_cast(user_channel.ReadUntil(' '))); // TODO: do something with it
            std::vector<uint8_t> data = user_channel.ReadUntil('\n'); // TODO: dont know exactly what to do with it...

         	std::string translated_filepath = TranslateFile(filename);
         	std::ifstream translated_file;

         	user_channel.Write("TRR f ");
         	user_channel.Write(translated_filepath + " ");

            translated_file.open(translated_filepath, std::ios::binary | std::ios::ate);
            user_channel.Write(std::to_string(translated_file.tellg()) + " ");
            translated_file.close();

            translated_file.open(translated_filepath, std::ios::in);
            user_channel.Write(translated_file);
            translated_file.close();

            user_channel.Write("\n");
        }
        
        else throw std::exception(); //TODO: right exception
	}

    std::string TranslateWord(const std::string& word){
        return word_translator.at(word);
    }

    std::string TranslateFile(const std::string& file){
    	return file_translator.at(file);
    }
};


int main(int argc, char **argv) {
	
    u_short trs_port = 59000 + GN;
    u_short tcs_port = 58000 + GN; 
    std::string hostname = "localhost";

    if(argc < 2){
        std::cerr << "Usage <language> [-p <trs_port>] [-n <hostname>] [-e <tcs_port>]";
        exit(1);
    }
    std::string language = argv[1];
    for (int i = 2; i<argc-1; i++) {
        if (!strcmp("-p", argv[i])){
            i++;
            trs_port = std::stoi(argv[i]);
        } else if (!strcmp("-n", argv[i])) {
            i++;
            hostname = argv[i];
        } else if (!strcmp("-e", argv[i])) {
            i++;
            tcs_port = std::stoi(argv[i]);
        }
        std::cerr << "Unknow option: " << argv[i] << std::endl;
    }

    TRSInterface lang_server(language, trs_port, hostname, tcs_port);
    while(1)
    	lang_server.TRR();
}
