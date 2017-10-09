#include "utils.hpp"

std::vector<uint8_t> byte_cast(const std::string& str){
    return std::vector<uint8_t>(str.begin(), str.end());
}

std::string string_cast(const std::vector<uint8_t>& bytes){
    return std::string(bytes.begin(), bytes.end());
}

std::vector<std::string> tokenize(const std::string& str){
    std::vector<std::string> tokens;
    std::string aux;
    std::stringstream ss(str);
    while (ss >> aux)
        tokens.push_back(aux);
    return tokens;
}

std::string detokenize(const std::vector<std::string>& tokens){
    std::string sum;
    for(auto& token : tokens)
        sum += token + " ";
    sum.pop_back();
    return sum;
}

void sigpipe_handler(int signum){
    signal(SIGPIPE, sigpipe_handler);
    //Does nothing else
}