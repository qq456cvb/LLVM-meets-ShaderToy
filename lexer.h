#pragma once
#include <memory>
#include <string>
#include <vector>
#include <iostream>

class Token
{
private:
    
public:
    enum class Type {
        Identifier,
        //Comment,
        Separator,
        Operator,
        Literal,
        Keyword,
        Qualifier
    };

    Token::Type type;
    std::shared_ptr<void> data;

    template<typename T>
    Token(const Token::Type&, std::shared_ptr<T> = nullptr);

    template<typename T>
    const T& get_data() const;

    friend std::ostream &operator<<(std::ostream&, const Token&);
};



class Lexer
{
    std::vector<std::shared_ptr<Token>> tokens;
    std::string str;
public:
    Lexer();
    ~Lexer();

    void prepare(const std::string&);
    void print();
    bool parse_iden(std::string::iterator&, std::string& buffer);
    bool parse_liter(std::string::iterator&, std::string& buffer);
    bool parse_rec(std::string::iterator&);
    bool parse();
};

