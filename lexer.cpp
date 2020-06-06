#include "lexer.h"


template<typename T>
Token::Token(const Token::Type& t, std::shared_ptr<T> ptr) : type(t), data(ptr)
{
}

template<typename T>
const T& Token::get_data() const {
    return *std::static_pointer_cast<T>(data);
}

std::ostream &operator<<(std::ostream& os, const Token& t) {
    std::string type_str;
    switch (t.type)
    {
#define SET_CASE(v) case (Token::Type::v): { type_str = #v; break; }
        SET_CASE(Identifier)
        //SET_CASE(Comment)
        SET_CASE(Separator)
        SET_CASE(Operator)
        SET_CASE(Literal)
        SET_CASE(Keyword)
        SET_CASE(Qualifier)
#undef SET_CASE
    default:
        break;
    }
    if (t.type == Token::Type::Identifier || t.type == Token::Type::Keyword || t.type == Token::Type::Qualifier)    os << type_str << ": " << t.get_data<std::string>();
    else if (t.type == Token::Type::Separator || t.type == Token::Type::Operator)    os << type_str << ": " << t.get_data<char>();
    else if (t.type == Token::Type::Literal)    os << type_str << ": " << t.get_data<double>();
    return os;
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
        tokens.emplace_back(new Token(Token::Type::Literal, std::make_shared<double>(value)));
        return parse_rec(it);
    }
    catch (const std::exception&)
    {
        std::cout << "Could not convert string to double" << std::endl;
        return false;
    }
}

bool Lexer::parse_iden(std::string::iterator& it, std::string& buffer) {
    auto& first = *it;
    if ((first >= '0' && first <= '9') || (first >= 'A' && first <= 'Z') || (first >= 'a' && first <= 'z')) {
        buffer += first;
        return parse_iden(++it, buffer);
    }
    
    if (buffer == "const" || buffer == "in" || buffer == "out" || buffer == "return" || buffer == "if" || buffer == "else" || buffer == "while") {
        tokens.emplace_back(new Token(Token::Type::Keyword, std::make_shared<std::string>(buffer)));
        return parse_rec(it);
    }
    else if (buffer == "float" || buffer == "int" || buffer == "vec2" || buffer == "vec3" || buffer == "vec4") {
        tokens.emplace_back(new Token(Token::Type::Qualifier, std::make_shared<std::string>(buffer)));
        return parse_rec(it);
    }
    else {
        tokens.emplace_back(new Token(Token::Type::Identifier, std::make_shared<std::string>(buffer)));
        return parse_rec(it);
    }
}

bool Lexer::parse_rec(std::string::iterator& it) {
    if (it == str.end()) return true;
    auto& first = *it;
    if (first == ' ' || first == '\n' || first == '\r' || first == '\t') {
        return parse_rec(++it);
    }
    if (first == '*' || first == '/' || first == '+' || first == '-' || first == '=' || first == '<' || first == '>') {
        if (*it == '/' && it + 1 != str.end() && *(it + 1) == '/') {
            auto itt = it + 2;
            while (itt != str.end()) {
                if (*itt == '\n') return parse_rec(++itt);
                itt++;
            }
            return true;
        }
        tokens.emplace_back(new Token(Token::Type::Operator, std::make_shared<char>(first)));
        return parse_rec(++it);
    }
    if (first == '{' || first == '}' || first == '(' || first == ')' || first == ';' || first == ',' || first == '.') {
        tokens.emplace_back(new Token(Token::Type::Separator, std::make_shared<char>(first)));
        return parse_rec(++it);
    }
    if ((first >= 'A' && first <= 'Z') || (first >= 'a' && first <= 'z')) {
        std::string buf{ "" };
        buf += first;
        return parse_iden(++it, buf);
    }
    if ((first >= '0' && first <= '9')) {
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
    for (const auto& t : tokens) {
        std::cout << *t << " | ";
    }
}

