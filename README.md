# Compiler-Construction-24  

This repository contains the implementation of a **simple compiler** created as part of the **COMPILER CONSTRUCTION LAB - 2024**, instructed by **Sir Laeeq Khan Niazi**.  

## 🌟 What does this compiler do?  
This compiler takes a source code file (e.g., `code.txt`) as input and performs the following steps:  
1. **Tokenization:** Breaks the code into meaningful units called tokens.  
2. **Parsing:** Analyzes the tokens to check the syntax and ensures the code follows the grammar rules.  
3. **Symbol Table Management:** Keeps track of all variables, constants, and their attributes (e.g., type, value, scope).  
4. **Three-Address Code (TAC) Generation:** Converts the code into an intermediate representation using three-address code.  
5. **Intel x86 Assembly Code Generation:** Generates assembly code (e.g., `output.asm`) compatible with the Intel x86 architecture.  

This tool helps understand the process behind compiling a program, from high-level code to low-level machine instructions.  

## 💻 How to Compile and Run the Compiler  
### 1. Compile the Compiler  
Run the following command to compile the source code of the compiler:  
```bash  
g++ compiler.cpp -o compiler.exe  
```  

### 2. Run the Compiler  
Once compiled, you can use the compiler with the following command:  
```bash  
./compiler.exe code.txt output.asm
```  
In case the above command does not work, run this: 
```bash  
compiler.exe code.txt output.asm
```  

Here:  
- `code.txt` is the input file containing the source code to be compiled.  
- `output.asm` is the generated Intel x86 assembly file.  

## 🛠 Features  
- **Educational Purpose:** Simplified design to help students learn about compiler construction.  
- **Full Pipeline:** Covers key phases of compilation: Lexical Analysis, Syntax Analysis, Semantic Analysis, Intermediate Code Generation, and Assembly Code Generation.  
- **Output Assembly:** Generates Intel x86 assembly code as output, making it easier to see how high-level code maps to low-level instructions.  

Feel free to explore the code, test it with your own examples, and learn how a compiler works step by step!  
