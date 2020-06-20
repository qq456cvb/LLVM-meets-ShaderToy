#include "parser.h"
#include "lexer.h"
#include "preprocess.h"
#include <fstream>
#include <sstream>

int main() {
    std::ifstream t("src/test.txt");
    std::stringstream buffer;
    buffer << t.rdbuf();

    auto content = buffer.str();
    Preprocessor processor;
    processor.process(content);

    std::cout << content << std::endl;

    //Lexer lexer;
    //lexer.parse(content);
    //lexer.print();

    //Parser parser;
    //parser.parse_program(lexer.tokens);
    //parser.print();
    return 0;
}

