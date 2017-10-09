#include <string>
#include <vector>
#include <sstream>
#include "signal.h"

#define GN 7

std::vector<uint8_t> byte_cast(const std::string& str);

std::string string_cast(const std::vector<uint8_t>& bytes);

std::vector<std::string> tokenize(const std::string& str);

std::string detokenize(const std::vector<std::string>& tokens);

void sigpipe_handler(int signum);