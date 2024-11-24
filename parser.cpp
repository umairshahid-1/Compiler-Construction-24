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

// Symbol Table using unordered_map for O(1) lookup
class SymbolTable
{
private:
    unordered_map<string, SymbolEntry> symbols;

public:
    bool insert(const string &name, TokenType type, int line)
    {
        if (symbols.find(name) != symbols.end())
        {
            return false; // Symbol already exists
        }
        symbols[name] = {name, type, "", line, false};
        return true;
    }

    SymbolEntry *lookup(const string &name)
    {
        auto it = symbols.find(name);
        return (it != symbols.end()) ? &(it->second) : nullptr;
    }

    void updateValue(const string &name, const string &value)
    {
        if (symbols.find(name) != symbols.end())
        {
            symbols[name].value = value;
            symbols[name].initialized = true;
        }
    }

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
        // ... other cases
        default:
            return "UNKNOWN";
        }
    }
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

class TACInstruction
{
public:
    enum OpType
    {
        ASSIGN,      // x = y
        ADD,         // x = y + z
        SUB,         // x = y - z
        MUL,         // x = y * z
        DIV,         // x = y / z
        LABEL,       // Label for jumps
        JUMP,        // Unconditional jump
        JUMPIFTRUE,  // Conditional jump
        JUMPIFFALSE, // Conditional jump
        LESSTHAN,    // x = y < z
        GREATERTHAN, // x = y > z
        EQUALS,      // x = y == z
        RETURN       // return x
    };

    OpType op;
    string result; // Target of operation
    string arg1;   // First operand
    string arg2;   // Second operand
    string label;  // For jumps and labels

    // Constructor for binary operations
    TACInstruction(OpType op, string result, string arg1, string arg2)
        : op(op), result(result), arg1(arg1), arg2(arg2) {}

    // Constructor for unary operations
    TACInstruction(OpType op, string result, string arg1)
        : op(op), result(result), arg1(arg1) {}

    // Constructor for jumps
    TACInstruction(OpType op, string label)
        : op(op), label(label) {}

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
        case EQUALS:
            return result + " = " + arg1 + " == " + arg2;
        case RETURN:
            return "return " + result;
        default:
            return "unknown instruction";
        }
    }
};

class Lexer
{
private:
    string src;
    size_t pos;
    int line;
    int column;
    map<string, TokenType> keywords;

public:
    Lexer(const string &src) : src(src), pos(0), line(1), column(1)
    {
        initializeKeywords();
    }

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

            // Handle sigle and multi line comments
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

    // Additional helper methods
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

class Parser
{
private:
    vector<Token> tokens;
    size_t pos;
    SymbolTable symbolTable;
    vector<TACInstruction> tacInstructions;
    int tempVarCounter = 0;
    int labelCounter = 0;

    // Generate new temporary variable
    string newTemp()
    {
        return "t" + to_string(++tempVarCounter);
    }

    // Generate new label
    string newLabel()
    {
        return "L" + to_string(++labelCounter);
    }

    // Structure to hold expression result info
    struct ExprResult
    {
        string place; // Where result is stored
        string code;  // Generated TAC code

        // Add a constructor to make it easier to create ExprResult
        ExprResult(string p = "", string c = "") : place(p), code(c) {}
    };

    void error(const string &message)
    {
        Token token = tokens[pos];
        stringstream error;
        error << "Syntax error at line " << token.line << ", column " << token.column
              << ": " << message << "\nFound '" << token.value << "'";
        throw runtime_error(error.str());
    }

    void expect(TokenType type)
    {
        if (tokens[pos].type != type)
        {
            string expected = "expected token"; // You could add a toString method for TokenType
            error(expected);
        }
        pos++;
    }

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

    void parseDeclaration()
    {
        // Handle variable declarations
        TokenType dataType = tokens[pos].type;
        pos++; // Move past the data type

        // Expect an identifier
        if (tokens[pos].type != T_ID)
        {
            error("Expected identifier after type declaration");
        }
        string varName = tokens[pos].value;
        pos++; // Move past identifier

        // Add to symbol table
        if (!symbolTable.insert(varName, dataType, tokens[pos - 1].line))
        {
            error("Variable '" + varName + "' already declared");
        }

        // Check for initialization
        if (tokens[pos].type == T_ASSIGN)
        {
            pos++; // Move past =
            symbolTable.updateValue(varName, tokens[pos].value);
            parseExpression();
        }

        expect(T_SEMICOLON);
    }

    void parseAssignment()
    {
        string varName = tokens[pos].value;
        pos++; // Move past identifier

        SymbolEntry *entry = symbolTable.lookup(varName);
        if (!entry)
        {
            error("Variable '" + varName + "' not declared");
        }

        expect(T_ASSIGN);
        ExprResult expr = parseExpression();
        tacInstructions.push_back(TACInstruction(
            TACInstruction::ASSIGN, varName, expr.place));
        expect(T_SEMICOLON);
    }

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

    void parseWhileStatement()
    {
        pos++; // Move past 'while'
        expect(T_LPAREN);
        parseExpression(); // Parse condition
        expect(T_RPAREN);
        parseStatement(); // Parse loop body
    }

    void parseForStatement()
    {
        pos++; // Move past 'for'
        expect(T_LPAREN);

        // Initialization part
        parseStatement();

        // Condition part
        parseExpression();
        expect(T_SEMICOLON);

        // Increment/Decrement part - handle as an assignment
        if (tokens[pos].type == T_ID)
        {
            string varName = tokens[pos].value;
            pos++; // Move past identifier

            if (tokens[pos].type == T_ASSIGN)
            {
                pos++; // Move past =
                parseExpression();
            }
            else
            {
                error("Expected assignment operator in for loop increment");
            }
        }
        else
        {
            parseExpression(); // Handle other types of expressions
        }

        expect(T_RPAREN);

        // Body
        parseStatement();
    }

    void parseReturnStatement()
    {
        pos++; // Move past 'return'
        if (tokens[pos].type == T_NUM)
        {
            pos++; // Move past the number (e.g., 0)
        }
        expect(T_SEMICOLON); // Expect a semicolon to end the return statement
    }

    void parseBlock()
    {
        expect(T_LBRACE);
        while (pos < tokens.size() && tokens[pos].type != T_RBRACE)
        {
            parseStatement();
        }
        expect(T_RBRACE); // Ensures each `{` has a matching `}`
    }

    // Modified parseExpression to generate TAC
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
                type != T_AND && type != T_OR)
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
                // Add other cases for remaining operators
            }

            result.place = temp;
        }

        return result;
    }

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
            // Generate appropriate TAC instruction for unary operator
            // For NOT:
            // tacInstructions.push_back(TACInstruction(TACInstruction::NOT, temp, operand.place));
            result.place = temp;
        }
        else
        {
            error("Unexpected token in expression: '" + tokens[pos].value + "'");
        }

        return result;
    }

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

public:
    Parser(const vector<Token> &tokens) : tokens(tokens), pos(0) {}

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

    void suggestFix(const string &error)
    {
        // Add code to suggest fixes for common errors
        if (error.find("missing semicolon") != string::npos)
        {
            cout << "Suggestion: Add a semicolon (;) at the end of the statement" << endl;
        }
        // Add more suggestions for other common errors
    }

    // Method to print generated TAC
    void printTAC()
    {
        cout << "\n--- Three Address Code ---\n";
        for (const auto &instr : tacInstructions)
        {
            cout << instr.toString() << endl;
        }
    }

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
        if (argc != 2)
        {
            cerr << "Usage: " << argv[0] << " <filename>" << endl;
            cerr << "Example: " << argv[0] << " myprogram.txt" << endl;
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