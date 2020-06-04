#include "lexer.h"


Token::Token(const Token::Type& t, std::shared_ptr<void> ptr) : type(t), data(ptr)
{
}

Token::~Token()
{
}

std::ostream &operator<<(std::ostream& os, const Token& t) {
    std::string type_str;
    switch (t.type)
    {
#define SET_CASE(v) case (Token::Type::v): { type_str = "v"; break; }
        SET_CASE(Identifier)
#undef SET_CASE
    default:
        break;
    }
}

Lexer::Lexer()
{
}

Lexer::~Lexer()
{
}

void Lexer::prepare(const std::string& s) {
    str = s;
}


bool Lexer::parse_liter(std::string::iterator& it, std::string& buffer) {
    auto& first = *it;
    if ((first >= '0' && first <= '9') || first == '.') {
        buffer += first;
        return parse_liter(++it, buffer);
    }
    try
    {
        double value = std::stod(buffer);
        tokens.emplace_back(Token::Type::Literal, std::make_shared<double>(value));
        return parse_rec(++it);
    }
    catch (const std::exception&)
    {
        std::cout << "Could not convert string to double" << std::endl;
        return false;
    }
}

bool Lexer::parse_iden(std::string::iterator& it, std::string& buffer) {
    auto& first = *it;
    if ((first >= '0' && first <= '9') || first == '.') {
        buffer += first;
        return parse_iden(++it, buffer);
    }
    
    if (buffer == "const" || buffer == "float" || buffer == "int" || buffer == "vec2" || buffer == "vec3" || buffer == "vec4"
        || buffer == "in" || buffer == "out" || buffer == "return") {
        tokens.emplace_back(Token::Type::Keyword, std::make_shared<std::string>(buffer));
        return parse_rec(++it);
    }
    else {
        tokens.emplace_back(Token::Type::Identifier, std::make_shared<std::string>(buffer));
        return parse_rec(++it);
    }
}

bool Lexer::parse_rec(std::string::iterator& it) {
    if (it == str.end()) return true;
    auto& first = *it;
    if (first == ' ' || first == '\n' || first == '\r' || first == '\t') {
        return parse_rec(++it);
    }
    if (first == '*' || first == '/' || first == '+' || first == '-' || first == '=') {
        if (*it == '/' && it + 1 != str.end() && *(it + 1) == '/') {
            auto itt = it + 2;
            while (itt != str.end()) {
                if (*itt == '\n') return parse_rec(++itt);
                itt++;
            }
        }
        tokens.emplace_back(Token::Type::Operator, std::make_shared<char>(first));
        return parse_rec(++it);
    }
    if (first == '{' || first == '}' || first == '(' || first == ')' || first == ';') {
        tokens.emplace_back(Token::Type::Separator, std::make_shared<char>(first));
        return parse_rec(++it);
    }
    if ((first >= 'A' && first <= 'Z') || (first >= 'a' && first <= 'z')) {
        std::string buf{ "" };
        buf += first;
        return parse_iden(++it, buf);
    }
    if ((first >= '0' && first <= '9') || first == '.' ) {
        std::string buf{ "" };
        buf += first;
        return parse_liter(++it, buf);
    }
}

bool Lexer::parse() {
    auto it = str.begin();
    return parse_rec(it);
}

void Lexer::print() {

}

