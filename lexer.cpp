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
    if (t.type == Token::Type::Identifier || t.type == Token::Type::Keyword || t.type == Token::Type::Qualifier || t.type == Token::Type::Operator)    os << type_str << ": " << t.get_data<std::string>();
    else if (t.type == Token::Type::Separator)    os << type_str << ": " << t.get_data<char>();
    else if (t.type == Token::Type::Literal)    os << type_str << ": " << t.get_data<double>();
    return os;
}

Lexer::Lexer()
{
}

Lexer::~Lexer()
{
}


bool Lexer::parse_liter(std::string::const_iterator& it, std::string& buffer) {
    auto& first = *it;
    if ((first >= '0' && first <= '9') || first == '.') {
        buffer += first;
        return parse_liter(++it, buffer);
    }
    try
    {
        double value = std::stod(buffer);
        tokens.emplace_back(new Token(Token::Type::Literal, std::make_shared<double>(value)));
        return true;
    }
    catch (const std::exception&)
    {
        std::cout << "Could not convert string to double" << std::endl;
        return false;
    }
}

bool Lexer::parse_iden(std::string::const_iterator& it, std::string& buffer) {
    auto& first = *it;
    if ((first >= '0' && first <= '9') || (first >= 'A' && first <= 'Z') || (first >= 'a' && first <= 'z')) {
        buffer += first;
        return parse_iden(++it, buffer);
    }
    
    if (buffer == "const" || buffer == "in" || buffer == "out" || buffer == "return" || buffer == "if" || buffer == "else" || buffer == "for" || buffer == "while") {
        tokens.emplace_back(new Token(Token::Type::Keyword, std::make_shared<std::string>(buffer)));
        return true;
    }
    else if (buffer == "float" || buffer == "int" || buffer == "vec2" || buffer == "vec3" || buffer == "vec4") {
        tokens.emplace_back(new Token(Token::Type::Qualifier, std::make_shared<std::string>(buffer)));
        return true;
    }
    else {
        tokens.emplace_back(new Token(Token::Type::Identifier, std::make_shared<std::string>(buffer)));
        return true;
    }
}

bool Lexer::parse(const std::string& str) {
    auto it = str.begin();
    end = str.end();
    while (it != end)
    {
        auto& first = *it;
        if (first == ' ' || first == '\n' || first == '\r' || first == '\t') {
            it++;
            continue;
        }
        if (first == '*' || first == '/' || first == '+' || first == '-' || first == '=' || first == '<' || first == '>') {
            std::string op(1, first);
            if (it + 1 != end) {
                if (*it == '/' && *(it + 1) == '/') {
                    auto itt = it + 2;
                    while (itt != end) {
                        if (*itt == '\n') {
                            it = ++itt;
                            break;
                        }
                        itt++;
                    }
                }
                else if ((*(it + 1) == '=') || (*it == '+' && *(it + 1) == '+') || (*it == '-' && *(it + 1) == '-'))
                {
                    op.push_back(*(++it));
                }
            }
            tokens.emplace_back(new Token(Token::Type::Operator, std::make_shared<std::string>(op)));
            it++;
            continue;
        }
        if (first == '{' || first == '}' || first == '(' || first == ')' || first == ';' || first == ',' || first == '.') {
            if (first == '.' && it + 1 != end && *(it + 1) >= '0' && *(it + 1) <= '9')
            {
                std::string buf{ "." };
                parse_liter(++it, buf);
                continue;
            }
            tokens.emplace_back(new Token(Token::Type::Separator, std::make_shared<char>(first)));
            it++;
            continue;
        }
        if ((first >= 'A' && first <= 'Z') || (first >= 'a' && first <= 'z')) {
            std::string buf{ "" };
            buf += first;
            parse_iden(++it, buf);
            continue;
        }
        if ((first >= '0' && first <= '9')) {
            std::string buf{ "" };
            buf += first;
            parse_liter(++it, buf);
            continue;
        }
    }
    return true;
}

void Lexer::print() {
    for (const auto& t : tokens) {
        std::cout << *t << " | ";
    }
}

