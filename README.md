# LLVM-meets-ShaderToy

A from-scratch compiler frontend for ShaderToy-style GLSL, written in C++ with no parser generator. The end goal was to translate ShaderToy fragment shaders into other targets (C++, LLVM IR) for CPU execution; what's implemented is the frontend pipeline up to an AST.

## Pipeline

- `preprocess.cpp` — a minimal preprocessor handling `#define` macro substitution (the only directive ShaderToy code typically needs).
- `lexer.cpp` — a hand-rolled tokenizer producing identifier, literal, operator, separator, keyword, and qualifier tokens.
- `parser.cpp` — a recursive-descent parser with operator-precedence climbing for expressions and explicit left-recursion elimination, building a generic `ASTNode` tree. It handles function/variable declarations (including GLSL parameter qualifiers like `in`), compound statements, `if`/`else`, `for`, `while`, function calls, and assignment, and can pretty-print the AST with indentation.
- `test.txt` — the test input: a real ShaderToy fragment shader (an FBM-noise fire effect using `iTime`/`iChannel0`) exercising macros, vector types, swizzles, and control flow.

## Usage

Compile the four `.cpp` files with any C++14 compiler and run from the directory containing `src/test.txt` (the input path is hardcoded in `main.cpp`). As committed, `main.cpp` runs only the preprocessor and prints the expanded source; uncomment the `Lexer`/`Parser` blocks to tokenize and dump the AST.

## Status

Frontend-only prototype, parked before code generation: despite the name, no LLVM (or C++) backend was ever wired up — the AST is where it ends.
