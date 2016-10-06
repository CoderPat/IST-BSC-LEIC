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

struct TRSException : public std::exception {
   std::string s;
   TRSException(std::string ss) : s(ss) {}
   ~TRSException() throw () {}
   const char* what() const throw() { return s.c_str(); }
};

struct initialization_error : public TRSException{
   initialization_error(std::string ss) : TRSException(ss) {}
   ~initialization_error() throw () {}
};

struct translation_not_available : public TRSException{
   translation_not_available(std::string ss) : TRSException(ss) {}
   ~translation_not_available() throw () {}
};

struct invalid_request : public TRSException{
   invalid_request(std::string ss) : TRSException(ss) {}
   ~invalid_request() throw () {}
};

struct invalid_response : public TRSException{
   invalid_response(std::string ss) : TRSException(ss) {}
   ~invalid_response() throw () {}
};

class TRSInterface{
private:
    TCPServer server_;
    UDPChannel tcs_channel_;
    u_short port_;
    std::string language_;
    std::map<std::string, std::string> word_translator;
    std::map<std::string, std::string> file_translator;

    /** 
     *  Send a arbitray request and interprets the response 
     *  Compactation of the SRG and SUN, which are the same with different initial signals
     */
    void _status_update(const std::string& request, const std::string& expected_response){

        char myhostname[HOST_NAME_MAX+1];
        if(gethostname(myhostname, HOST_NAME_MAX+1)==-1)
            throw initialization_error("Couldn't get self name");

        std::string message = request + " " + language_ + " " + myhostname + " " + std::to_string(port_) + "\n";
        tcs_channel_.Write(byte_cast(message));

        std::string response = string_cast(tcs_channel_.Read());

        std::vector<std::string> tokens = tokenize(response);

        if(tokens.at(0) != expected_response) throw invalid_response("Not the expected response");
        if(tokens.at(1) == "NOK") throw invalid_response("Request refused");  //TODO: right exception
        if(tokens.at(1) == "ERR") throw invalid_response("Something went wrong on the server side");  //TODO: right exception   
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

    TRSInterface(){} //TODO: Only allow other functions after proper initialization

    TRSInterface(const std::string& language,
                 const std::string& word_file_path,
                 const std::string& img_file_path,
                 u_short my_port, 
                 const std::string& tcs_hostname, 
                 u_short tcs_port) : port_(my_port), language_(language), server_(my_port), tcs_channel_(tcs_hostname, tcs_port) {
        LoadTranslators(word_file_path, img_file_path);
    }

    void SRG(){
        _status_update("SRG", "SRR");
    }

    void SUN(){
        _status_update("SUN", "SUR");
    }

    void TRR(fd_set set){
        TCPChannel user_channel = server_.Listen(set);
        try{
            std::string request = string_cast(user_channel.ReadUntil(' '));
            if(request != "TRQ") throw invalid_request("Unkown request"); 

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
                size_t byte_size = std::stol(string_cast(user_channel.ReadUntil(' ')));
                std::vector<uint8_t> data = user_channel.Read(byte_size);
                if(user_channel.Read(1)[0] != '\n') throw invalid_request("Error in protocol"); 

                std::string translated_filepath = TranslateFile(filename);
                std::ifstream translated_file;

                //Begining of the response and path
                user_channel.Write("TRR f ");
                user_channel.Write(translated_filepath + " ");

                //Write the size
                translated_file.open(translated_filepath, std::ios::binary | std::ios::ate);
                user_channel.Write(std::to_string(translated_file.tellg()) + " ");
                translated_file.close();

                //And the actual data
                translated_file.open(translated_filepath, std::ios::in);
                user_channel.Write(translated_file);
                translated_file.close();

                user_channel.Write("\n");
            }
            else throw invalid_request("Unkown TRQ request option"); 
        }
        catch(invalid_request& e){
            user_channel.Write("TRR ERR");
            user_channel.Write("\n");
        }
        catch(translation_not_available& e){
            user_channel.Write("TRR NTA");
            user_channel.Write("\n");
        }
       
    }

    std::string TranslateWord(const std::string& word){
        try{
            return word_translator.at(word);
        }
        catch(std::out_of_range& e){throw translation_not_available(word);} 
    }

    std::string TranslateFile(const std::string& file){
        try{
           return file_translator.at(file);
        }
        catch(std::out_of_range& e){throw translation_not_available(file);}
    }
};


int main(int argc, char **argv) {
    
    u_short trs_port = 59000 + GN;
    u_short tcs_port = 58000 + GN; 
    std::string hostname = "localhost";
    std::string text_translation_file = "auxiliary/text_translation.txt";
    std::string file_translation_file = "auxiliary/file_translation.txt";

    if(argc < 2 || argc%2==1){
        std::cerr << "Usage <language> [-p <trs_port>] [-n <hostname>] [-e <tcs_port>]";
        exit(1);
    }


    std::string language = argv[1];
    for (int i = 2; i<argc-1; i++) {
        try{
            if (!strcmp("-p", argv[i])){
                i++;
                long aux;
                aux = std::stol(argv[i]);
                if(aux > (1<<16) || aux <= 0) throw std::out_of_range("");
                trs_port = aux;
            } else if (!strcmp("-n", argv[i])) {
                i++;
                hostname = argv[i];
            } else if (!strcmp("-e", argv[i])) {
                i++;
                long aux;
                aux = std::stol(argv[i]);
                if(aux > (1<<16) || aux <= 0) throw std::out_of_range("");
                tcs_port = aux;
            } else 
                throw std::invalid_argument("Unknown option " + std::string(argv[i]));
        }
        catch(std::out_of_range& e){
            std::cerr << "Port numbers must be in the range 1-65535" << std::endl;
            exit(1); 
        }
        catch(std::invalid_argument& e){ 
            std::string message = (!strcmp(e.what(),"stol") ? "Ports must be a number" : e.what());
            std::cerr << message << std::endl;
            exit(1);
        }
    }

    try{
        TRSInterface lang_server(TRSInterface(language, text_translation_file, file_translation_file, trs_port, hostname, tcs_port));
        
        lang_server.SRG();

        fd_set input;
        FD_ZERO(&input);
        FD_SET(STDIN_FILENO, &input);
        while(1){
            try{
                lang_server.TRR(input);
            }
            //Check for exit command
            catch(other_inputs_available& e){
                std::string in;
                std::cin >> in;
                if(in == "exit"){
                    lang_server.SUN();
                    exit(0);
                }
            }
            //One user connection timed out (doesn kill the server)
            catch(tcp_socket_timeout& e){
                std::cerr << "One user connection timed out" << std::endl;
            }

        }
    }
    //Error connecting/disconnecting from the TCS
    catch(udp_socket_timeout& e){
        std::cerr << "TCS connection attempt timed out." << std::endl;
        exit(1);
    }
    //TCS refused to register our server
    catch(invalid_response& e){
        std::cerr << "TCS denied request to connect." << std::endl;
        exit(1);
    }
    catch(...){
        std::cerr << "Something went wrong with the server :/" << std::endl;
        exit(1);
     }


}
