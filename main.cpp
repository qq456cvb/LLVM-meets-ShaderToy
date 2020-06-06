#include "parser.h"
#include "lexer.h"
#include <fstream>
#include <sstream>

int main() {
    std::ifstream t("src/test.txt");
    std::stringstream buffer;
    buffer << t.rdbuf();

    Lexer lexer;
    lexer.prepare(buffer.str());
    lexer.parse();
    lexer.print();

    Parser parser;
    parser.parse_program(lexer.tokens);
    parser.print();
    return 0;
}

