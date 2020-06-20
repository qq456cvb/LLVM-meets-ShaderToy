#include "preprocess.h"

void replace_str(int pos, std::string& subject, const std::string& search, const std::string& replace) {
    while ((pos = subject.find(search, pos)) != std::string::npos) {
        subject.replace(pos, search.length(), replace);
        pos += replace.length();
    }
}

std::string Preprocessor::parse_word(std::string::const_iterator& it, std::string::const_iterator end) {
    auto ch = *(it++);
    if (!(ch >= 'A' && ch <= 'Z') && !(ch >= 'a' && ch <= 'z'))
    {
        throw std::exception();
    }
    std::string result;
    result += ch;
    while (it != end && *it != ' ')
    {
        ch = *it;
        if (!(ch >= 'A' && ch <= 'Z') && !(ch >= 'a' && ch <= 'z') && !(ch >= '0' && ch <= '9')) throw std::exception();
        result += ch;
        it++;
    }
    return result;
}
void Preprocessor::process(std::string& str) {
    auto it = str.begin();
    while (it != str.end())
    {
        auto ch = *it;
        if (ch == '#')
        {
            auto word = parse_word(++it, str.end());
            if (word != "define")
            {
                throw std::exception();
            }
            while (*(++it) == ' ');
            auto key = parse_word(it, str.end());
            while (*(++it) == ' ');
            std::string value = "";
            while (*it != '\n')
            {
                value += *(it++);
            }
            replace_str(it - str.begin(), str, key, value);
        }
        it++;
    }
}