#include "parser.h"
#include "lexer.h"
#include <iostream>


ASTNode::ASTNode(ASTNode::Type type, std::shared_ptr<Token> t) {
    this->token = t;
    this->type = type;
}

std::ostream &operator<<(std::ostream& os, const ASTNode& n) {
    std::string type_str;
    switch (n.type)
    {
#define SET_CASE(v) case (ASTNode::Type::v): type_str = #v; break;
        SET_CASE(Global)
        SET_CASE(Return)
        SET_CASE(Factor)
        SET_CASE(Expr)
        SET_CASE(Proto)
        SET_CASE(Call)
        SET_CASE(Def)
        SET_CASE(Compound)
        SET_CASE(Ifelse)
        SET_CASE(While)
        SET_CASE(Basic)
#undef SET_CASE
    default:
        break;
    }
    if (n.type == ASTNode::Type::Basic || n.type == ASTNode::Type::Call)
    {
        os << type_str << ", " << *n.token << std::endl;
    }
    else 
    {
        os << type_str << std::endl;
    }
    return os;
}

std::shared_ptr<Token> Parser::get_next(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    if (it == end) return nullptr;
    return *(it++);
}

std::shared_ptr<Token> Parser::peek_next(const std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    if (it == end) return nullptr;
    return *it;
}

bool Parser::parse_program(const std::vector<std::shared_ptr<Token>>& tokens) {
    end = tokens.end();
    root = std::make_shared<ASTNode>(ASTNode::Type::Global);
    auto it = tokens.begin();
    auto next = peek_next(it);
    while (next) {
        root->children.push_back(parse_decl(it));
        next = peek_next(it);
    }
    return true;
}

std::shared_ptr<ASTNode> Parser::parse_decl(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    if (peek_next(it)->type == Token::Type::Qualifier) {
        auto itt = it + 2;
        if (itt >= end) {
            throw std::exception("");
        }
        else {
            if ((*itt)->type == Token::Type::Separator) {
                if ((*itt)->get_data<char>() == '(') return parse_func_decl(it);
                else if ((*itt)->get_data<char>() == ';') return parse_var_decl(it);
                else throw std::exception("");
            }
            else if ((*itt)->type == Token::Type::Operator && (*itt)->get_data<char>() == '=') {
                return parse_var_decl(it);
            }
            else {
                throw std::exception("");
            }
        }
    }
    else if ((*it)->type == Token::Type::Keyword && (*it)->get_data<std::string>() == "const") {
        return parse_var_decl(it);
    }
    else {
        throw std::exception("");
    }
}

std::shared_ptr<ASTNode> Parser::parse_func_decl(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    auto type = get_next(it);
    if (!type) throw std::exception("");
    auto iden = get_next(it);
    if (!iden) throw std::exception("");
    auto left_paren = get_next(it);
    if (!left_paren || !(left_paren->get_data<char>() == '(')) throw std::exception("");  // left paren
    
    std::shared_ptr<ASTNode> node = std::make_shared<ASTNode>(ASTNode::Type::Proto);
    node->children.push_back(std::make_shared<ASTNode>(ASTNode::Type::Basic, type));
    node->children.push_back(std::make_shared<ASTNode>(ASTNode::Type::Basic, iden));
    node->children.push_back(parse_param_decl(it));
    auto right_paren = get_next(it);
    if (!right_paren || !(right_paren->get_data<char>() == ')')) throw std::exception("");  // right paren

    auto left_brack = get_next(it);
    if (!left_brack || !(left_brack->get_data<char>() == '{')) throw std::exception("");  // right paren
    node->children.push_back(parse_body(it));
    auto right_brack = get_next(it);
    if (!right_brack || !(right_brack->get_data<char>() == '}')) throw std::exception("");  // right paren
    return node;
}

std::shared_ptr<ASTNode> Parser::parse_var_decl(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    std::shared_ptr<ASTNode> var = std::make_shared<ASTNode>(ASTNode::Type::Def);
    auto next = get_next(it);
    while (next->type == Token::Type::Keyword)
    {
        var->children.push_back(std::make_shared<ASTNode>(ASTNode::Type::Basic, next));
        next = get_next(it);
    }
    if (next->type != Token::Type::Qualifier) throw std::exception();
    var->token = next;

    next = peek_next(it);
    if (!next) throw std::exception("");
    while (next->get_data<char>() != ';') {
        var->children.push_back(parse_var_decl_single(it));
        next = peek_next(it++);
    }
    return var;
}

std::shared_ptr<ASTNode> Parser::parse_var_decl_single(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    std::shared_ptr<ASTNode> node = std::make_shared<ASTNode>(ASTNode::Type::Basic);
    auto next = get_next(it);
    if (next->type != Token::Type::Identifier) throw std::exception("");
    node->children.push_back(std::make_shared<ASTNode>(ASTNode::Type::Basic, next));
    next = peek_next(it);
    if (next->type == Token::Type::Operator)
    {
        if (next->get_data<char>() != '=') throw std::exception();
        node->token = next;
        node->children.push_back(parse_expr(++it));
    }
    return node;
}

std::shared_ptr<ASTNode> Parser::parse_expr(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    auto expr = std::make_shared<ASTNode>(ASTNode::Type::Expr);
    auto left = parse_factor(it);
    return parse_expr_rec(it, left, 0);
}

std::shared_ptr<ASTNode> Parser::parse_expr_rec(std::vector<std::shared_ptr<Token>>::const_iterator& it, std::shared_ptr<ASTNode> left, int prec) {
    auto next_op = peek_next(it);
    if (next_op->type != Token::Type::Operator)
    {
        return left;
    }
    it++;
    auto expr = std::make_shared<ASTNode>(ASTNode::Type::Expr);
    auto op_prec = precedence_map[next_op->get_data<char>()];
    if (op_prec > prec)
    {
        expr->token = next_op;
        expr->children.push_back(left);
        auto right_left = parse_factor(it);
        expr->children.push_back(parse_expr_rec(it, right_left, op_prec));
        return expr;
    }
    else
    {
        expr->token = next_op;
        expr->children.push_back(left);
        expr->children.push_back(parse_factor(it));
        return parse_expr_rec(it, expr, prec);
    }
}

std::shared_ptr<ASTNode> Parser::parse_factor(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    auto next = peek_next(it);
    if (next->type == Token::Type::Operator)
    {
        if (next->get_data<char>() == '-' || next->get_data<char>() == '+')
        {
            std::shared_ptr<ASTNode> factor = std::make_shared<ASTNode>(ASTNode::Type::Factor);
            factor->token = next;
            factor->children.push_back(parse_primary(++it));
            return factor;
        } 
        else 
        {
            throw std::exception();
        }
    }
    else
    {
        return parse_primary(it);
    }
}


std::shared_ptr<ASTNode> Parser::parse_primary(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    auto next = peek_next(it);
    if (next->type == Token::Type::Separator)
    {
        return parse_expr(++it);
    } 
    else if (next->type == Token::Type::Literal || next->type == Token::Type::Identifier)
    {
        auto nextnext = peek_next(it + 1);
        if (nextnext && nextnext->type == Token::Type::Separator && nextnext->get_data<char>() == '(')
        {
            return parse_func_call(it);
        }
        it++;
        return std::make_shared<ASTNode>(ASTNode::Type::Basic, next);
    }
    else if (next->type == Token::Type::Qualifier) 
    {
        auto nextnext = peek_next(it + 1);
        if (nextnext && nextnext->type == Token::Type::Separator && nextnext->get_data<char>() == '(')
        {
            return parse_func_call(it);
        } 
        else 
        {
            throw std::exception();
        }
    }
    else {
        throw std::exception();
    }
}

std::shared_ptr<ASTNode> Parser::parse_func_call(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    std::shared_ptr<ASTNode> node = std::make_shared<ASTNode>(ASTNode::Type::Call);
    auto func_name = get_next(it);
    node->token = func_name;

    get_next(it);

    auto next = peek_next(it);
    if (!next) throw std::exception("");
    while (next->get_data<char>() != ')') {
        node->children.push_back(parse_expr(it));
        next = peek_next(it++);
    }
    return node;
}

std::shared_ptr<ASTNode> Parser::parse_statment(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    auto next = peek_next(it);
    if (next->type == Token::Type::Keyword)
    {
        auto word = next->get_data<std::string>();
        if (word == "if")
        {
            return parse_if_statment(it);
        }
        else if (word == "while")
        {
            return parse_while_statment(it);
        }
        else if (word == "return")
        {
            std::shared_ptr<ASTNode> node = std::make_shared<ASTNode>(ASTNode::Type::Return);
            node->children.push_back(parse_expr(++it));
            return node;
        }
        else
        {
            throw std::exception();
        }
    }
    else
    {
        if (next->type == Token::Type::Separator && next->get_data<char>() == '{')
        {
            auto node = parse_body(++it);
            auto right_bracket = get_next(it);
            if (right_bracket->type != Token::Type::Separator || right_bracket->get_data<char>() != '}')
            {
                throw std::exception();
            }
            return node;
        } 
        else 
        {
            auto node = parse_expr(it);
            auto right_semi = get_next(it);
            if (right_semi->type != Token::Type::Separator || right_semi->get_data<char>() != ';')
            {
                throw std::exception();
            }
            return node;
        }
    }
}

std::shared_ptr<ASTNode> Parser::parse_decl_single(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    std::shared_ptr<ASTNode> param = std::make_shared<ASTNode>(ASTNode::Type::Def);
    auto next = get_next(it);
    if (!next) throw std::exception("");
    while (next->type == Token::Type::Keyword) {
        param->children.push_back(std::make_shared<ASTNode>(ASTNode::Type::Basic, next));
        next = get_next(it);
    } 
    if (next->type != Token::Type::Qualifier) throw std::exception("");
    param->children.push_back(std::make_shared<ASTNode>(ASTNode::Type::Basic, next));
    next = get_next(it);
    if (next->type != Token::Type::Identifier) throw std::exception("");
    param->children.push_back(std::make_shared<ASTNode>(ASTNode::Type::Basic, next));
    return param;
}

std::shared_ptr<ASTNode> Parser::parse_param_decl(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    std::shared_ptr<ASTNode> params = std::make_shared<ASTNode>(ASTNode::Type::Compound);
    auto next = peek_next(it);
    if (!next) throw std::exception("");
    while (next->get_data<char>() != ')') {
        params->children.push_back(parse_decl_single(it));
        next = peek_next(it++);
    }
    it--;
    return params;
}

std::shared_ptr<ASTNode> Parser::parse_body(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    auto body = std::make_shared<ASTNode>(ASTNode::Type::Compound);
    auto next = peek_next(it);
    while (next && next->get_data<char>() != '}') {
        auto ifvar = peek_next(it);
        if ((ifvar->type == Token::Type::Keyword && ifvar->get_data<std::string>() == "const") || ifvar->type == Token::Type::Qualifier)
        {
            body->children.push_back(parse_var_decl(it));
        }
        else
        {
            body->children.push_back(parse_statment(it));
        }
        next = peek_next(it);
    }
    return body;
}

std::shared_ptr<ASTNode> Parser::parse_if_statment(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    auto node = std::make_shared<ASTNode>(ASTNode::Type::Ifelse);
    get_next(it);
    get_next(it);  // TODO: error check
    auto expr = parse_expr(it);
    get_next(it);
    get_next(it);
    auto statement = parse_body(it);
    get_next(it);
    auto next = peek_next(it);
    node->children.push_back(expr);
    node->children.push_back(statement);
    if (next && next->type == Token::Type::Keyword && next->get_data<std::string>() == "else")
    {
        auto else_statement = parse_body(++it);
        node->children.push_back(else_statement);
    }
    return node;
}

std::shared_ptr<ASTNode> Parser::parse_while_statment(std::vector<std::shared_ptr<Token>>::const_iterator& it) {
    auto node = std::make_shared<ASTNode>(ASTNode::Type::While);
    get_next(it);
    get_next(it);  // TODO: error check
    auto expr = parse_expr(it);
    get_next(it);
    get_next(it);
    auto statement = parse_body(it);
    get_next(it);
    node->children.push_back(expr);
    node->children.push_back(statement);
    return node;
}

void Parser::print() {
    print_rec(root, 0);
}

void Parser::print_rec(const std::shared_ptr<ASTNode>& node, int indent) {
    for (int i = 0; i < indent; i++) std::cout << " ";
    std::cout << *node;
    for (const auto& child : node->children)
    {
        print_rec(child, indent + 2);
    }
}