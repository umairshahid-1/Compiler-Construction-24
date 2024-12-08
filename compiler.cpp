#include <iostream> // For cout, cerr
#include <fstream>  // For file I/O
#include <vector>   // For std::vector
#include <string>   // For std::string
#include <cctype>   // For isspace, isdigit, isalpha
#include <map>
#include <unordered_map> // For symbol table (unordered_map for O(1) lookup)
#include <sstream>       // For stringstream, used in reading files and error handling
#include <iomanip>       // For table formatting with setw
#include <stdexcept>     // For runtime_error
#include <algorithm>

using namespace std;

enum TokenType
{
    // Data types
    T_INT,
    T_FLOAT,
    T_DOUBLE,
    T_STRING,
    T_BOOL,
    T_CHAR,

    // Keywords
    T_IF,
    T_ELSE,
    T_RETURN,
    T_FOR,
    T_WHILE,
    T_DO,
    T_BREAK,

    // Identifiers and literals
    T_ID,
    T_NUM,
    T_STRING_LITERAL,
    T_CHAR_LITERAL,
    T_BOOL_LITERAL,

    // Operators
    T_ASSIGN,
    T_PLUS,
    T_MINUS,
    T_MUL,
    T_DIV,
    T_GT,
    T_LT,
    T_GE,
    T_LE,
    T_EQ,
    T_NEQ,
    T_AND,
    T_OR,
    T_NOT,

    // Delimiters
    T_LPAREN,
    T_RPAREN,
    T_LBRACE,
    T_RBRACE,
    T_SEMICOLON,
    T_COMMA,
    T_DOT,

    // Special tokens
    T_EOF
};

// Symbol table entry structure
struct SymbolEntry
{
    string name;
    TokenType type;
    string value;
    int line;
    bool initialized;
};

struct Token
{
    TokenType type;
    string value;
    int line;
    int column;
};

/* Symbol Table using unordered_map for O(1) lookup
This class manages a symbol table to store information about variables and their attributes
such as name, type, value, line number, and initialization status during compilation.*/

class SymbolTable
{
private:
    // Stores symbols as a mapping from variable name (string) to its attributes (SymbolEntry)
    unordered_map<string, SymbolEntry> symbols;

public:
    /* Insert a new symbol into the table
    Input:
    - name: The name of the variable
    - type: The type of the variable (e.g., T_INT, T_FLOAT)
    - line: The line number where the variable is declared
    Output:
    - Returns false if the symbol already exists; otherwise, true after successful insertion.*/
    bool insert(const string &name, TokenType type, int line)
    {
        if (symbols.find(name) != symbols.end())
        {
            return false; // Symbol already exists
        }
        symbols[name] = {name, type, "", line, false};
        return true;
    }

    /*Lookup a symbol in the table by name
    Input:
    - name: The name of the variable to find
    Output:
    - Returns a pointer to the SymbolEntry if found, or nullptr if not found.*/
    SymbolEntry *lookup(const string &name)
    {
        auto it = symbols.find(name);
        return (it != symbols.end()) ? &(it->second) : nullptr;
    }

    /* Update the value of an existing symbol
    Input:
    - name: The name of the variable to update
    - value: The new value to assign to the variable
    Updates the `value` and marks the symbol as initialized.*/
    void updateValue(const string &name, const string &value)
    {
        if (symbols.find(name) != symbols.end())
        {
            symbols[name].value = value;
            symbols[name].initialized = true;
        }
    }

    /* Get a string representation of a TokenType
    Input:
    - type: The TokenType to convert (e.g., T_INT, T_FLOAT)
    Output:
    - Returns a string name for the given type (e.g., "INT", "FLOAT").*/
    string getTypeName(TokenType type)
    {
        switch (type)
        {
        case T_INT:
            return "INT";
        case T_FLOAT:
            return "FLOAT";
        case T_DOUBLE:
            return "DOUBLE";
        case T_BOOL:
            return "BOOL";
        case T_STRING:
            return "STRING";
        case T_CHAR:
            return "CHAR";
        // Add other types here as needed
        default:
            return "UNKNOWN";
        }
    }

    /* Retrieve all symbols from the table
    Output:
        - Returns a vector of all SymbolEntry objects in the table.*/
    vector<SymbolEntry> getAllSymbols() const
    {
        vector<SymbolEntry> allSymbols;
        for (const auto &entry : symbols)
        {
            allSymbols.push_back(entry.second);
        }
        return allSymbols;
    }

    /* Print all entries in the symbol table
    Formats and displays the name, type, value, line number, and initialization status of each symbol in a tabular format.*/
    void printEntries()
    {
        cout << left << setw(15) << "Name" << setw(10) << "Type" << setw(10) << "Value" << setw(10) << "Line" << setw(15) << "Initialized" << endl;
        cout << string(56, '-') << endl;
        for (const auto &entry : symbols)
        {
            const SymbolEntry &symbol = entry.second;
            cout << left << setw(15) << symbol.name
                 << setw(10) << getTypeName(symbol.type)
                 << setw(10) << symbol.value
                 << setw(10) << symbol.line
                 << setw(15) << (symbol.initialized ? "Yes" : "No") << endl;
        }
    }
};

/*TACInstruction Class
This class represents a single Three Address Code (TAC) instruction, which is an intermediate
representation used during the compilation process. Each instruction specifies an operation,
its operands, and the result or target.

The class supports various operations like assignments, arithmetic, comparisons, and jumps.
It provides constructors for creating binary operations, unary operations, and jump statements,
along with a method to convert the instruction into a readable string format.
*/
class TACInstruction
{
public:
    /*
    OpType Enumeration
    Defines the type of operation the TAC instruction represents.
    Examples include:
    - ASSIGN: Assignment operation (x = y)
    - ADD, SUB, MUL, DIV: Arithmetic operations
    - LABEL: Represents a label in code
    - JUMP, JUMPIFTRUE, JUMPIFFALSE: Control flow operations
    - Comparisons like LESSTHAN, EQUALS, etc.
    */
    enum OpType
    {
        ASSIGN,           // x = y
        ADD,              // x = y + z
        SUB,              // x = y - z
        MUL,              // x = y * z
        DIV,              // x = y / z
        LABEL,            // Label for jumps
        JUMP,             // Unconditional jump
        JUMPIFTRUE,       // Conditional jump
        JUMPIFFALSE,      // Conditional jump
        LESSTHAN,         // x = y < z
        GREATERTHAN,      // x = y > z
        LESSTHANEQUAL,    // x = y <= z
        GREATERTHANEQUAL, // x = y >= z
        EQUALS,           // x = y == z
        RETURN            // return x
    };

    OpType op;     // The type of operation being represented
    string result; // The result or target variable for the operation
    string arg1;   // The first operand (source or input)
    string arg2;   // The second operand (used in binary operations)
    string label;  // A label name for jump or label operations

    /*
    Constructor for Binary Operations
    Creates a TAC instruction for operations requiring two operands.
    Input:
    - op: The operation type (e.g., ADD, SUB, etc.)
    - result: The variable where the result will be stored
    - arg1: The first operand
    - arg2: The second operand
    */
    TACInstruction(OpType op, string result, string arg1, string arg2)
        : op(op), result(result), arg1(arg1), arg2(arg2) {}

    /*
    Constructor for Unary Operations
    Creates a TAC instruction for operations requiring a single operand.
    Input:
    - op: The operation type (e.g., ASSIGN)
    - result: The variable where the result will be stored
    - arg1: The operand
    */
    TACInstruction(OpType op, string result, string arg1)
        : op(op), result(result), arg1(arg1) {}

    /*
    Constructor for Jump Operations
    Creates a TAC instruction for control flow operations like JUMP, JUMPIFTRUE, etc.
    Input:
    - op: The operation type (e.g., JUMP, LABEL)
    - label: The label or target of the jump
    */
    TACInstruction(OpType op, string label)
        : op(op), label(label) {}

    /*
    Converts the TAC instruction to a human-readable string format.
    Output:
    - Returns a string representation of the instruction (e.g., "x = y + z").
    */
    string toString() const
    {
        switch (op)
        {
        case ASSIGN:
            return result + " = " + arg1;
        case ADD:
            return result + " = " + arg1 + " + " + arg2;
        case SUB:
            return result + " = " + arg1 + " - " + arg2;
        case MUL:
            return result + " = " + arg1 + " * " + arg2;
        case DIV:
            return result + " = " + arg1 + " / " + arg2;
        case LABEL:
            return label + ":";
        case JUMP:
            return "goto " + label;
        case JUMPIFTRUE:
            return "if " + arg1 + " goto " + label;
        case JUMPIFFALSE:
            return "ifFalse " + arg1 + " goto " + label;
        case LESSTHAN:
            return result + " = " + arg1 + " < " + arg2;
        case GREATERTHAN:
            return result + " = " + arg1 + " > " + arg2;
        case LESSTHANEQUAL:
            return result + " = " + arg1 + " <= " + arg2;
        case GREATERTHANEQUAL:
            return result + " = " + arg1 + " >= " + arg2;
        case EQUALS:
            return result + " = " + arg1 + " == " + arg2;
        case RETURN:
            return "return " + result;
        default:
            return "unknown instruction";
        }
    }
};

/*X86AssemblyGenerator Class
This class is responsible for generating x86 assembly code from a given set of
Three Address Code (TAC) instructions. It handles variable allocation,
instruction translation, and stack management, ensuring a structured assembly output.

Key Features:
- Maintains mappings between variables and memory locations on the stack.
- Handles uninitialized variables by providing default values in the assembly.
- Generates function prologue and epilogue for stack setup and cleanup.
*/
class X86AssemblyGenerator
{
private:
    /*
    tacInstructions: Reference to a vector of TAC instructions.
    These instructions are translated into x86 assembly code.
    */
    vector<TACInstruction> &tacInstructions;

    /*
    assemblyCode: Stores the generated x86 assembly code as strings.
    This vector accumulates the translated instructions for output.
    */
    vector<string> assemblyCode;

    /*
    variableToMemory: Maps variable names to their corresponding memory locations.
    Example: A variable `x` might map to `[ebp-4]` in the stack.
    */
    unordered_map<string, string> variableToMemory;

    /*
    initializedVariables: Tracks whether variables are initialized before usage.
    If a variable is uninitialized, it is zeroed out in the assembly code.
    */
    unordered_map<string, bool> initializedVariables;

    /*
    stackSize: Keeps track of the total stack space allocated for variables.
    This value is used to adjust the stack pointer in the assembly code.
    */
    int stackSize = 0;

    /*
    allocateStackSpace
    Allocates space on the stack for a variable and maps it to a memory location.
    Input:
    - var: The name of the variable to allocate space for.
    - initialized: A boolean indicating whether the variable is initialized.
    */
    void allocateStackSpace(const string &var, bool initialized)
    {
        stackSize += 4; // 4 bytes for each variable
        variableToMemory[var] = "[ebp-" + to_string(stackSize) + "]";
        initializedVariables[var] = initialized; // Track initialization status
    }

    /*
    allocateUninitializedVariables
    Handles uninitialized variables by assigning them a default value of zero.
    This method ensures that all variables are properly initialized in the assembly code.
    */
    void allocateUninitializedVariables()
    {
        for (const auto &entry : initializedVariables)
        {
            if (!entry.second)
            {
                assemblyCode.push_back("; Warning: Variable " + entry.first + " is uninitialized.");
                assemblyCode.push_back("    mov dword " + variableToMemory[entry.first] + ", 0");
            }
        }
    }

public:
    /*
    Constructor
    Initializes the generator with a reference to a vector of TAC instructions.
    Input:
    - tac: Reference to the vector of TAC instructions to be converted to assembly.
    */
    X86AssemblyGenerator(vector<TACInstruction> &tac) : tacInstructions(tac) {}

    /*
    generateAssembly
    Converts the provided TAC instructions into x86 assembly code.
    Input:
    - symbolTable: Reference to the symbol table, used to retrieve variable information.
    Output:
    - Returns a vector of strings, each representing a line of x86 assembly code.
    */
    vector<string> generateAssembly(const SymbolTable &symbolTable)
    {
        // Data section
        assemblyCode.push_back("section .data");
        assemblyCode.push_back("; Data declarations go here (if any)");

        // BSS section for uninitialized global variables
        assemblyCode.push_back("section .bss");

        // Text section
        assemblyCode.push_back("section .text");
        assemblyCode.push_back("global _start");
        assemblyCode.push_back("_start:");

        // Function prologue
        assemblyCode.push_back("    push ebp");
        assemblyCode.push_back("    mov ebp, esp");

        // Allocate space for local variables
        auto symbols = symbolTable.getAllSymbols();
        for (const auto &entry : symbols)
        {
            allocateStackSpace(entry.name, entry.initialized);
        }
        assemblyCode.push_back("    sub esp, " + to_string(stackSize));

        // Zero out uninitialized variables
        allocateUninitializedVariables();

        // Generate assembly for each TAC instruction
        for (const auto &tac : tacInstructions)
        {
            generateInstructionAssembly(tac);
        }

        // Function epilogue
        assemblyCode.push_back("    mov esp, ebp");
        assemblyCode.push_back("    pop ebp");
        assemblyCode.push_back("    mov eax, 1");   // syscall: exit
        assemblyCode.push_back("    xor ebx, ebx"); // exit code 0
        assemblyCode.push_back("    int 0x80");

        return assemblyCode;
    }

    /*
    generateInstructionAssembly
    Translates a single TAC instruction into corresponding x86 assembly code.
    Input:
    - tac: A reference to the TAC instruction to be translated.
    */
    void generateInstructionAssembly(const TACInstruction &tac)
    {
        assemblyCode.push_back("; " + tac.toString()); // Comment with TAC instruction

        switch (tac.op)
        {
        case TACInstruction::ASSIGN:
            // Handle assignment instructions
            assemblyCode.push_back("    mov eax, " + getOperandLocation(tac.arg1));
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            initializedVariables[tac.result] = true; // Mark as initialized
            break;

            // Handle arithmetic, jumps, comparisons, etc.
        }
    }

    /*
    getOperandLocation
    Determines the memory location or immediate value for a given operand.
    Input:
    - operand: The operand whose location is to be determined.
    Output:
    - Returns the memory location for variables or the immediate value for constants.
    */
    string getOperandLocation(const string &operand)
    {
        if (variableToMemory.find(operand) != variableToMemory.end())
        {
            return variableToMemory[operand];
        }
        return operand; // Immediate values or labels
    }
};

/*Lexer Class
The Lexer class performs lexical analysis, breaking down the source code into tokens.
Tokens are the smallest units of the code, such as keywords, identifiers, operators,
literals, and delimiters.

Key Responsibilities:
- Tokenize the input source code into a sequence of tokens.
- Handle single-line and multi-line comments.
- Recognize and handle different types of tokens like keywords, numbers, strings, and operators.
- Provide detailed error messages for invalid characters or incomplete literals.
*/
class Lexer
{
private:
    // src: The source code to be tokenized, provided as a single string.
    string src;

    // pos: The current position in the source string during tokenization.
    size_t pos;

    // line: Tracks the current line number in the source code.
    int line;

    // column: Tracks the current column number within the current line.
    int column;

    /*
    keywords: A mapping of reserved keywords to their respective token types.
    This allows the lexer to distinguish between keywords and identifiers.
    */
    map<string, TokenType> keywords;

public:
    /*
    Constructor
    Initializes the Lexer with the source code, setting the starting position, line,
    and column. It also initializes the keyword mapping.
    Input:
    - src: The source code as a string.
    */
    Lexer(const string &src) : src(src), pos(0), line(1), column(1)
    {
        initializeKeywords();
    }

    /*
    initializeKeywords
    Populates the keywords map with reserved keywords and their corresponding token types.
    */
    void initializeKeywords()
    {
        keywords["int"] = T_INT;
        keywords["float"] = T_FLOAT;
        keywords["double"] = T_DOUBLE;
        keywords["string"] = T_STRING;
        keywords["bool"] = T_BOOL;
        keywords["char"] = T_CHAR;
        keywords["if"] = T_IF;
        keywords["else"] = T_ELSE;
        keywords["return"] = T_RETURN;
        keywords["for"] = T_FOR;
        keywords["while"] = T_WHILE;
        keywords["do"] = T_DO;
        keywords["break"] = T_BREAK;
        keywords["true"] = T_BOOL_LITERAL;
        keywords["false"] = T_BOOL_LITERAL;
    }

    /*
    tokenize
    Main method to convert the source code into a vector of tokens. It iterates through
    the source code character by character and identifies tokens based on their type.
    Output:
    - Returns a vector of tokens representing the lexical elements of the source code.
    */
    vector<Token> tokenize()
    {
        vector<Token> tokens;
        while (pos < src.size())
        {
            char current = src[pos];

            // Handle whitespace
            if (isspace(current))
            {
                if (current == '\n')
                {
                    line++;
                    column = 1;
                }
                else
                {
                    column++;
                }
                pos++;
                continue;
            }

            // Handle single-line and multi-line comments
            if (current == '/' && pos + 1 < src.size())
            {
                if (src[pos + 1] == '/')
                {
                    // Single-line comment
                    while (pos < src.size() && src[pos] != '\n')
                    {
                        pos++;
                    }
                    continue;
                }
                else if (src[pos + 1] == '*')
                {
                    // Multi-line comment
                    pos += 2;
                    while (pos + 1 < src.size() && !(src[pos] == '*' && src[pos + 1] == '/'))
                    {
                        if (src[pos] == '\n')
                        {
                            line++;
                            column = 1;
                        }
                        pos++;
                    }
                    pos += 2;
                    continue;
                }
            }

            // Handle numbers
            if (isdigit(current))
            {
                tokens.push_back(consumeNumber());
                continue;
            }

            // Handle identifiers and keywords
            if (isalpha(current) || current == '_')
            {
                Token token = consumeIdentifier();
                tokens.push_back(token);
                continue;
            }

            // Handle operators and other symbols
            Token token = consumeOperator();
            if (token.type != T_EOF)
            {
                tokens.push_back(token);
                continue;
            }

            // Handle unexpected characters
            stringstream error;
            error << "Unexpected character '" << current << "' at line " << line << ", column " << column;
            throw runtime_error(error.str());
        }

        tokens.push_back({T_EOF, "", line, column});
        return tokens;
    }

    /*
    consumeString
    Handles string literals enclosed in double quotes (").
    Detects errors for unterminated string literals.
    Output:
    - Returns a Token object representing the string literal.
    */
    Token consumeString()
    {
        int startLine = line;
        int startColumn = column;
        string value = "";
        pos++; // Skip opening quote
        column++;

        while (pos < src.size() && src[pos] != '"')
        {
            if (src[pos] == '\n')
            {
                throw runtime_error("Unterminated string literal at line " + to_string(startLine));
            }
            if (src[pos] == '\\' && pos + 1 < src.size())
            {
                pos++;
                column++;
                switch (src[pos])
                {
                case 'n':
                    value += '\n';
                    break;
                case 't':
                    value += '\t';
                    break;
                case '\\':
                    value += '\\';
                    break;
                case '"':
                    value += '"';
                    break;
                default:
                    value += src[pos];
                }
            }
            else
            {
                value += src[pos];
            }
            pos++;
            column++;
        }

        if (pos >= src.size())
        {
            throw runtime_error("Unterminated string literal at line " + to_string(startLine));
        }

        pos++; // Skip closing quote
        column++;

        return {T_STRING_LITERAL, value, startLine, startColumn};
    }

    /*
    consumeChar
    Handles character literals enclosed in single quotes (').
    Detects errors for unterminated or invalid character literals.
    Output:
    - Returns a Token object representing the character literal.
    */
    Token consumeChar()
    {
        int startLine = line;
        int startColumn = column;
        pos++; // Skip opening quote
        column++;

        string value = "";
        if (pos >= src.size())
        {
            throw runtime_error("Unterminated character literal at line " + to_string(startLine));
        }

        if (src[pos] == '\\')
        {
            pos++;
            column++;
            if (pos >= src.size())
            {
                throw runtime_error("Unterminated character literal at line " + to_string(startLine));
            }
            switch (src[pos])
            {
            case 'n':
                value = "\n";
                break;
            case 't':
                value = "\t";
                break;
            case '\\':
                value = "\\";
                break;
            case '\'':
                value = "'";
                break;
            default:
                value = src[pos];
            }
        }
        else
        {
            value = src[pos];
        }

        pos++;
        column++;

        if (pos >= src.size() || src[pos] != '\'')
        {
            throw runtime_error("Unterminated character literal at line " + to_string(startLine));
        }

        pos++; // Skip closing quote
        column++;

        return {T_CHAR_LITERAL, value, startLine, startColumn};
    }

    /*
    consumeNumber
    Handles numeric literals, including both integers and floating-point numbers.
    Detects errors for invalid number formats (e.g., multiple decimal points).
    Output:
    - Returns a Token object representing the numeric literal.
    */
    Token consumeNumber()
    {
        int startLine = line;
        int startColumn = column;
        string value;
        bool isFloat = false;

        while (pos < src.size() && (isdigit(src[pos]) || src[pos] == '.'))
        {
            if (src[pos] == '.')
            {
                if (isFloat)
                {
                    throw runtime_error("Invalid number format: multiple decimal points at line " + to_string(line));
                }
                isFloat = true;
            }
            value += src[pos];
            pos++;
            column++;
        }

        // Check for valid float format
        if (isFloat)
        {
            try
            {
                stof(value); // Validate that it's a valid float
            }
            catch (const std::invalid_argument &)
            {
                throw runtime_error("Invalid float literal at line " + to_string(startLine) + ": " + value);
            }
        }

        return {isFloat ? T_FLOAT : T_NUM, value, startLine, startColumn};
    }

    /*
    consumeIdentifier
    Handles identifiers and checks if the identifier is a reserved keyword.
    Output:
    - Returns a Token object representing the identifier or keyword.
    */
    Token consumeIdentifier()
    {
        int startLine = line;
        int startColumn = column;
        string value;

        while (pos < src.size() && (isalnum(src[pos]) || src[pos] == '_'))
        {
            value += src[pos];
            pos++;
            column++;
        }

        auto it = keywords.find(value);
        if (it != keywords.end())
        {
            return {it->second, value, startLine, startColumn};
        }

        return {T_ID, value, startLine, startColumn};
    }

    /*
    consumeOperator
    Handles operators and delimiters in the source code.
    Recognizes single and multi-character operators (e.g., ==, !=, <=).
    Output:
    - Returns a Token object representing the operator or delimiter.
    */
    Token consumeOperator()
    {
        int startLine = line;
        int startColumn = column;
        char current = src[pos];
        pos++;
        column++;

        // Two-character operators
        if (pos < src.size())
        {
            string op = string(1, current) + src[pos];
            if (op == "==")
            {
                pos++;
                column++;
                return {T_EQ, op, startLine, startColumn};
            }
            else if (op == "!=")
            {
                pos++;
                column++;
                return {T_NEQ, op, startLine, startColumn};
            }
            else if (op == ">=")
            {
                pos++;
                column++;
                return {T_GE, op, startLine, startColumn};
            }
            else if (op == "<=")
            {
                pos++;
                column++;
                return {T_LE, op, startLine, startColumn};
            }
            else if (op == "&&")
            {
                pos++;
                column++;
                return {T_AND, op, startLine, startColumn};
            }
            else if (op == "||")
            {
                pos++;
                column++;
                return {T_OR, op, startLine, startColumn};
            }
        }

        // Single-character operators
        switch (current)
        {
        case '=':
            return {T_ASSIGN, "=", startLine, startColumn};
        case '+':
            return {T_PLUS, "+", startLine, startColumn};
        case '-':
            return {T_MINUS, "-", startLine, startColumn};
        case '*':
            return {T_MUL, "*", startLine, startColumn};
        case '/':
            return {T_DIV, "/", startLine, startColumn};
        case '(':
            return {T_LPAREN, "(", startLine, startColumn};
        case ')':
            return {T_RPAREN, ")", startLine, startColumn};
        case '{':
            return {T_LBRACE, "{", startLine, startColumn};
        case '}':
            return {T_RBRACE, "}", startLine, startColumn};
        case ';':
            return {T_SEMICOLON, ";", startLine, startColumn};
        case '>':
            return {T_GT, ">", startLine, startColumn};
        case '<':
            return {T_LT, "<", startLine, startColumn};
        case '!':
            return {T_NOT, "!", startLine, startColumn};
        case ',':
            return {T_COMMA, ",", startLine, startColumn};
        case '.':
            return {T_DOT, ".", startLine, startColumn};
        }

        pos--;
        column--;
        return {T_EOF, "", startLine, startColumn};
    }
};

/* Parser Class
 * -----------------
 * This class is responsible for parsing a sequence of tokens, generating
 * a symbol table, three-address code (TAC), and x86 assembly code. It handles
 * various constructs like declarations, assignments, conditional statements,
 * loops, and expressions.
 *
 * Inputs:
 * - A vector of tokens (`tokens`), representing the program's lexical elements.
 *
 * Outputs:
 * - Generates a symbol table (`symbolTable`) to store variable information.
 * - Produces TAC instructions (`tacInstructions`) for intermediate representation.
 * - Converts TAC to x86 assembly using an external generator (`asmGenerator`).
 */
class Parser
{
private:
    vector<Token> tokens;                   // List of tokens to parse
    size_t pos;                             // Current position in the tokens vector
    SymbolTable symbolTable;                // Tracks declared variables and their properties
    vector<TACInstruction> tacInstructions; // Stores generated TAC instructions
    int tempVarCounter = 0;                 // Counter for creating temporary variables
    int labelCounter = 0;                   // Counter for creating unique labels

    // Pointer to assembly generator object
    X86AssemblyGenerator *asmGenerator;

    /*
     * Generates a new temporary variable name.
     * Example: t1, t2, ...
     * Output:
     * - A string representing the new temporary variable.
     */
    string newTemp()
    {
        return "t" + to_string(++tempVarCounter);
    }

    /*
     * Generates a new unique label name.
     * Example: L1, L2, ...
     * Output:
     * - A string representing the new label.
     */
    string newLabel()
    {
        return "L" + to_string(++labelCounter);
    }

    /*
     * Structure to hold the result of an expression.
     * Includes:
     * - `place`: Location where the result is stored.
     * - `code`: Corresponding generated TAC code.
     */
    struct ExprResult
    {
        string place; // Location of the result
        string code;  // TAC code for the expression

        // Constructor to initialize `place` and `code`.
        ExprResult(string p = "", string c = "") : place(p), code(c) {}
    };

    /*
     * Throws a syntax error with a detailed message.
     * Inputs:
     * - `message`: Description of the error.
     */
    void error(const string &message)
    {
        Token token = tokens[pos];
        stringstream error;
        error << "Syntax error at line " << token.line << ", column " << token.column
              << ": " << message << "\nFound '" << token.value << "'";
        throw runtime_error(error.str());
    }

    /*
     * Verifies the current token matches the expected type.
     * Inputs:
     * - `type`: The expected token type.
     * Throws an error if the token does not match.
     */
    void expect(TokenType type)
    {
        if (tokens[pos].type != type)
        {
            string expected = "expected token"; // Extendable to describe expected tokens
            error(expected);
        }
        pos++;
    }

    /*
     * Parses a general statement, determining its type based on the current token.
     * Handles constructs like declarations, assignments, conditionals, loops, and blocks.
     */
    void parseStatement()
    {
        if (tokens[pos].type == T_EOF)
            return; // Avoid processing if end of file

        Token current = tokens[pos];
        switch (current.type)
        {
        case T_INT:
        case T_FLOAT:
        case T_DOUBLE:
        case T_STRING:
        case T_BOOL:
        case T_CHAR:
            parseDeclaration();
            break;
        case T_ID:
            parseAssignment();
            break;
        case T_IF:
            parseIfStatement();
            break;
        case T_WHILE:
            parseWhileStatement();
            break;
        case T_FOR:
            parseForStatement();
            break;
        case T_RETURN:
            parseReturnStatement();
            break;
        case T_LBRACE:
            parseBlock();
            break;
        default:
            error("Unexpected token");
        }
    }

    /*
     * Parses a variable declaration and handles optional initialization.
     * Example: `int x = 5;`
     * Generates:
     * - Adds the variable to the symbol table.
     * - TAC for initialization if provided.
     */
    void parseDeclaration()
    {
        TokenType dataType = tokens[pos].type; // Store the type of the variable
        pos++;                                 // Move past the type

        // Ensure an identifier follows the type
        if (tokens[pos].type != T_ID)
        {
            error("Expected identifier after type declaration");
        }
        string varName = tokens[pos].value; // Store the variable name
        pos++;                              // Move past identifier

        // Add variable to symbol table
        if (!symbolTable.insert(varName, dataType, tokens[pos - 1].line))
        {
            error("Variable '" + varName + "' already declared");
        }

        // Handle optional initialization
        if (tokens[pos].type == T_ASSIGN)
        {
            pos++; // Move past '='
            symbolTable.updateValue(varName, tokens[pos].value);

            // Parse and generate TAC for initialization
            ExprResult expr = parseExpression();
            tacInstructions.push_back(TACInstruction(TACInstruction::ASSIGN, varName, expr.place));
        }

        expect(T_SEMICOLON); // Ensure the statement ends with a semicolon
    }

    /*
     * Parses an assignment statement.
     * Example: `x = 10;`
     * Generates:
     * - Updates the symbol table with the new value.
     * - TAC for the assignment operation.
     */
    void parseAssignment()
    {
        string varName = tokens[pos].value; // Extract the variable name
        pos++;                              // Move past identifier

        SymbolEntry *entry = symbolTable.lookup(varName);
        if (!entry)
        {
            error("Variable '" + varName + "' not declared");
        }

        expect(T_ASSIGN);                    // Ensure assignment operator '='
        ExprResult expr = parseExpression(); // Parse the assigned expression

        // Generate TAC for the assignment
        tacInstructions.push_back(TACInstruction(TACInstruction::ASSIGN, varName, expr.place));

        expect(T_SEMICOLON); // Ensure the statement ends with a semicolon
    }

    /*
    parseIfStatement
    Parses an 'if' statement, including the condition and body of both 'if' and 'else' clauses.
    It expects specific tokens like 'if', '(', ')', and the statement body.
    If an 'else' clause is present, it will be parsed and handled as well.
*/
    void parseIfStatement()
    {
        pos++; // Move past 'if'
        expect(T_LPAREN);
        parseExpression(); // Parse condition
        expect(T_RPAREN);

        parseStatement(); // Parse if body

        // Handle else part
        if (pos < tokens.size() && tokens[pos].type == T_ELSE)
        {
            pos++;            // Move past 'else'
            parseStatement(); // Parse else body
        }
    }

    /*
        parseWhileStatement
        Parses a 'while' loop, generating corresponding Three Address Code (TAC) for the loop structure.
        It handles the condition check, loop body, and ensures proper jumping between loop iterations.
    */
    void parseWhileStatement()
    {
        pos++; // Move past 'while'
        expect(T_LPAREN);
        string startLabel = newLabel();
        string endLabel = newLabel();

        // Add start label before condition evaluation
        tacInstructions.push_back(TACInstruction(TACInstruction::LABEL, startLabel));

        // Parse and generate condition TAC
        ExprResult condition = parseExpression();
        expect(T_RPAREN);

        // Generate jump based on condition
        tacInstructions.push_back(TACInstruction(TACInstruction::JUMPIFFALSE, condition.place, endLabel));

        // Parse and generate loop body TAC
        parseStatement();

        // Jump back to start of loop
        tacInstructions.push_back(TACInstruction(TACInstruction::JUMP, startLabel));

        // Add end label
        tacInstructions.push_back(TACInstruction(TACInstruction::LABEL, endLabel));
    }

    /*
        parseForStatement
        Parses a 'for' loop by handling initialization, condition, increment, and the body of the loop.
        It generates labels and corresponding jumps for the loop structure.
    */
    void parseForStatement()
    {
        pos++;            // Move past 'for'
        expect(T_LPAREN); // Ensure '(' is present

        parseDeclaration(); // Handle declarations (e.g., `int i = 7;`)

        // Generate start and end labels
        string startLabel = newLabel();
        string endLabel = newLabel();

        // Add start label for the loop
        tacInstructions.push_back(TACInstruction(TACInstruction::LABEL, startLabel));

        // Parse Condition
        ExprResult condition;
        if (tokens[pos].type != T_SEMICOLON) // Check if a condition exists
        {
            condition = parseExpression(); // Parse the condition (e.g., `i > 5`)
        }
        expect(T_SEMICOLON); // Ensure the semicolon after the condition

        // Generate TAC for jumping if condition is false
        if (!condition.place.empty())
        {
            tacInstructions.push_back(TACInstruction(TACInstruction::JUMPIFFALSE, condition.place, endLabel));
        }

        // Parse Increment
        if (tokens[pos].type != T_RPAREN) // Check if an increment exists
        {
            parseExpression(); // Parse the increment (e.g., `i = i + 1`)
        }
        expect(T_RPAREN); // Ensure the closing ')' is present

        // Parse the loop body
        parseBlock();

        // Add a jump back to the start label
        tacInstructions.push_back(TACInstruction(TACInstruction::JUMP, startLabel));

        // Add end label to mark the end of the loop
        tacInstructions.push_back(TACInstruction(TACInstruction::LABEL, endLabel));
    }

    /*
        parseReturnStatement
        Parses a 'return' statement and expects an optional return value followed by a semicolon.
        If no return value is specified, it expects only the semicolon.
    */
    void parseReturnStatement()
    {
        pos++; // Move past 'return'
        if (tokens[pos].type == T_NUM)
        {
            pos++; // Move past the number (e.g., 0)
        }
        expect(T_SEMICOLON); // Expect a semicolon to end the return statement
    }

    /*
        parseBlock
        Parses a block of statements enclosed in curly braces.
        It continues parsing statements until the closing brace is encountered.
    */
    void parseBlock()
    {
        expect(T_LBRACE);
        while (pos < tokens.size() && tokens[pos].type != T_RBRACE)
        {
            parseStatement();
        }
        expect(T_RBRACE); // Ensures each `{` has a matching `}`
    }

    /*
        parseExpression
        Parses an expression involving operators, handling terms and factors, and generating corresponding TAC.
        The method handles expressions with operators like addition, subtraction, comparison, etc.
    */
    ExprResult parseExpression()
    {
        ExprResult result = parseTerm();

        while (pos < tokens.size())
        {
            TokenType type = tokens[pos].type;
            if (type != T_PLUS && type != T_MINUS &&
                type != T_GT && type != T_LT &&
                type != T_GE && type != T_LE &&
                type != T_EQ && type != T_NEQ &&
                type != T_ASSIGN)
            {
                break;
            }

            TokenType op = tokens[pos].type;
            pos++; // Move past operator

            ExprResult right = parseTerm();
            string temp = newTemp();

            // Generate TAC instruction based on operator type
            switch (op)
            {
            case T_PLUS:
                tacInstructions.push_back(TACInstruction(
                    TACInstruction::ADD, temp, result.place, right.place));
                break;
            case T_MINUS:
                tacInstructions.push_back(TACInstruction(
                    TACInstruction::SUB, temp, result.place, right.place));
                break;
            case T_GT:
                tacInstructions.push_back(TACInstruction(
                    TACInstruction::GREATERTHAN, temp, result.place, right.place));
                break;
            case T_LT:
                tacInstructions.push_back(TACInstruction(
                    TACInstruction::LESSTHAN, temp, result.place, right.place));
                break;
            case T_GE:
                tacInstructions.push_back(TACInstruction(
                    TACInstruction::GREATERTHANEQUAL, temp, result.place, right.place));
                break;
            case T_LE:
                tacInstructions.push_back(TACInstruction(
                    TACInstruction::LESSTHANEQUAL, temp, result.place, right.place));
                break;
            case T_EQ:
                tacInstructions.push_back(TACInstruction(
                    TACInstruction::EQUALS, temp, result.place, right.place));
                break;
            }

            result.place = temp;
        }

        return result;
    }

    /*
        parseFactor
        Parses factors in an expression, handling variables, literals, and parenthesized expressions.
        It also handles unary operators like negation or logical NOT.
    */
    ExprResult parseFactor()
    {
        if (pos >= tokens.size())
        {
            error("Unexpected end of input");
        }

        TokenType type = tokens[pos].type;
        ExprResult result;

        // Handle variable references (identifiers)
        if (type == T_ID)
        {
            string varName = tokens[pos].value;
            SymbolEntry *entry = symbolTable.lookup(varName);
            if (!entry)
            {
                error("Variable '" + varName + "' used before declaration");
            }
            result.place = varName;
            pos++;
        }
        // Handle literals
        else if (type == T_NUM || type == T_FLOAT ||
                 type == T_STRING_LITERAL || type == T_CHAR_LITERAL ||
                 type == T_BOOL_LITERAL)
        {
            result.place = tokens[pos].value;
            pos++;
        }
        // Handle parenthesized expressions
        else if (type == T_LPAREN)
        {
            pos++; // Move past (
            result = parseExpression();
            expect(T_RPAREN);
        }
        // Handle unary operators
        else if (type == T_NOT || type == T_MINUS)
        {
            string op = tokens[pos].value;
            pos++; // Move past operator
            ExprResult operand = parseFactor();
            string temp = newTemp();
            result.place = temp;
        }
        else
        {
            error("Unexpected token in expression: '" + tokens[pos].value + "'");
        }

        return result;
    }

    /*
        parseTerm
        Parses a term in an expression, which may involve multiplication or division operators.
        It generates the corresponding TAC for these operators.
    */
    ExprResult parseTerm()
    {
        ExprResult result = parseFactor();

        while (pos < tokens.size() &&
               (tokens[pos].type == T_MUL || tokens[pos].type == T_DIV))
        {
            TokenType op = tokens[pos].type;
            pos++; // Move past operator

            ExprResult right = parseFactor();
            string temp = newTemp();

            // Generate TAC instruction
            if (op == T_MUL)
            {
                tacInstructions.push_back(TACInstruction(
                    TACInstruction::MUL, temp, result.place, right.place));
            }
            else
            { // T_DIV
                tacInstructions.push_back(TACInstruction(
                    TACInstruction::DIV, temp, result.place, right.place));
            }

            result.place = temp;
        }

        return result;
    }

    /*
        Parser
        The Parser class is responsible for parsing source code and generating Three Address Code (TAC)
        or Assembly instructions. It manages parsing various statements and expressions and handles errors.
    */
public:
    Parser(const vector<Token> &tokens) : tokens(tokens), pos(0) { asmGenerator = nullptr; }

    /*
        parse
        Starts the parsing process, handling all statements in the provided token stream.
        It continues until the end of the file (EOF) or an error occurs.
    */
    void parse()
    {
        try
        {
            while (pos < tokens.size() && tokens[pos].type != T_EOF)
            {
                parseStatement();
            }
            cout << "Parsing completed successfully without any ERROR!" << endl;
        }
        catch (const runtime_error &e)
        {
            cerr << "Error: " << e.what() << endl;
            // You could add suggestion for fixing common errors here
            suggestFix(e.what());
        }
    }

    /*
        suggestFix
        Provides suggestions for fixing common parsing errors based on the error message.
    */
    void suggestFix(const string &error)
    {
        // Add code to suggest fixes for common errors
        if (error.find("missing semicolon") != string::npos)
        {
            cout << "Suggestion: Add a semicolon (;) at the end of the statement" << endl;
        }
        // Add more suggestions for other common errors
    }

    /*
        generateAssembly
        Generates x86 assembly code from the parsed TAC and writes it to the specified output file.
    */
    void generateAssembly(const string &outputFile)
    {
        X86AssemblyGenerator generator(tacInstructions);
        vector<string> assembly = generator.generateAssembly(symbolTable);

        ofstream outFile(outputFile);
        if (outFile.is_open())
        {
            for (const auto &line : assembly)
            {
                outFile << line << endl;
            }
            outFile.close();
            cout << "x86 assembly code generated successfully in " << outputFile << endl;
        }
        else
        {
            cerr << "Error: Unable to open output file for assembly code" << endl;
        }
    }

    /* Destructor to clean up resources like asmGenerator */
    ~Parser()
    {
        if (asmGenerator)
        {
            delete asmGenerator;
        }
    }

    /*
        printTAC
        Prints the generated Three Address Code (TAC) instructions to the console.
    */
    void printTAC()
    {
        cout << "\n--- Three Address Code ---\n";
        for (const auto &instr : tacInstructions)
        {
            cout << instr.toString() << endl;
        }
    }

    /*
        getSymbolTable
        Returns the symbol table associated with this parser instance.
    */
    SymbolTable &getSymbolTable()
    {
        return symbolTable;
    }
};

int main(int argc, char *argv[])
{
    try
    {
        // Check command line arguments
        if (argc != 3)
        {
            cerr << "Usage: " << argv[0] << " <input_file> <output_file>" << endl;
            cerr << "Example: " << argv[0] << " myprogram.txt output.asm" << endl;
            return 1;
        }

        // Open and read the input file
        ifstream inputFile(argv[1]);
        if (!inputFile.is_open())
        {
            cerr << "Error: Cannot open file '" << argv[1] << "'" << endl;
            cerr << "Please check if the file exists and you have read permissions." << endl;
            return 1;
        }

        // Read file content into string
        stringstream buffer;
        buffer << inputFile.rdbuf();
        string input = buffer.str();
        inputFile.close();

        cout << "\n=== Compiler Started ===" << endl;
        cout << "Processing file: " << argv[1] << endl;
        cout << "File size: " << input.length() << " bytes" << endl;

        // Create lexer and generate tokens
        cout << "\n--- Lexical Analysis ---" << endl;
        Lexer lexer(input);
        vector<Token> tokens;
        try
        {
            tokens = lexer.tokenize();
            cout << "Tokenization successful!" << endl;

            // Print tokens (with better formatting)
            cout << "\nTokens found:" << endl;
            cout << string(56, '-') << endl;
            cout << left << setw(20) << "TYPE" << setw(20) << "VALUE"
                 << setw(10) << "LINE"
                 << "COLUMN" << endl;
            cout << string(56, '-') << endl;

            for (const auto &token : tokens)
            {
                cout << left << setw(20) << static_cast<int>(token.type)
                     << setw(20) << token.value
                     << setw(10) << token.line
                     << token.column << endl;
            }
        }
        catch (const runtime_error &e)
        {
            cerr << "\nLexical Error: " << e.what() << endl;
            return 1;
        }

        // Parse the tokens
        cout << "\n--- Parsing ---" << endl;
        try
        {
            Parser parser(tokens);
            parser.parse();
            cout << "Parsing completed successfully!" << endl;

            cout << "\n--- Symbol Table ---" << endl;
            SymbolTable &symbolTable = parser.getSymbolTable();
            symbolTable.printEntries();

            // Print the Three Address Code after symbol table
            parser.printTAC();

            // Generate assembly code
            parser.generateAssembly(argv[2]);
        }
        catch (const runtime_error &e)
        {
            cerr << "\nParsing Error: " << e.what() << endl;
            return 1;
        }

        cout << "\n=== Compilation Completed Successfully ===" << endl;
        return 0;
    }
    catch (const exception &e)
    {
        cerr << "\nUnexpected error occurred: " << e.what() << endl;
        return 1;
    }
}