#pragma once
#include <memory>
#include <vector>
#include <unordered_map>

class Token;


class ASTNode {
public:
    enum class Type
    {
        Global,
        Return,
        Factor,
        Expr,
        Proto,
        Call,
        Def,
        Compound,
        Ifelse,
        While,
        Basic
    };

    ASTNode::Type type;
    std::shared_ptr<Token> token;
    std::vector<std::shared_ptr<ASTNode>> children;
    ASTNode(ASTNode::Type, std::shared_ptr<Token> = nullptr);

    friend std::ostream &operator<<(std::ostream&, const ASTNode&);
};


class Parser
{
public:
    bool parse_program(const std::vector<std::shared_ptr<Token>>& tokens);
    void print();
    std::shared_ptr<ASTNode> root;
private:
    std::vector<std::shared_ptr<Token>>::const_iterator end;
    std::shared_ptr<Token> get_next(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<Token> peek_next(const std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_decl(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_func_decl(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_var_decl(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_var_decl_single(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_decl_single(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_param_decl(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_body(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_expr(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_expr_rec(std::vector<std::shared_ptr<Token>>::const_iterator&, std::shared_ptr<ASTNode>, int prec);
    std::shared_ptr<ASTNode> parse_factor(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_primary(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_primary_rest(std::vector<std::shared_ptr<Token>>::const_iterator&);  // remove left recursion
    std::shared_ptr<ASTNode> parse_func_call(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_statment(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_if_statment(std::vector<std::shared_ptr<Token>>::const_iterator&);
    std::shared_ptr<ASTNode> parse_while_statment(std::vector<std::shared_ptr<Token>>::const_iterator&);
    void print_rec(const std::shared_ptr<ASTNode>&, int indent);

    std::unordered_map<char, int> precedence_map{
        { '+', 10 },
        { '-', 10 },
        { '*', 20 },
        { '/', 20 },
        { '=', 100 },
    };
};

