#pragma once
#include <unordered_map>

class Preprocessor
{
public:
    void process(std::string&);
protected:
private:
    std::string parse_word(std::string::const_iterator& it, std::string::const_iterator end);
    //std::unordered_map<std::string, std::string> defs;
};