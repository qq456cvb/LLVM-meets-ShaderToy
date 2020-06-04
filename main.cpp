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
    return 0;
}

