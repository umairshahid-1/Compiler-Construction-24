#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cctype>
#include <map>
#include <unordered_map>
#include <sstream>
#include <iomanip>
#include <stdexcept>
#include <algorithm>

using namespace std;

enum TokenType
{
    T_INT,
    T_FLOAT,
    T_DOUBLE,
    T_STRING,
    T_BOOL,
    T_CHAR,
    T_VOID,
    T_IF,
    T_ELSE,
    T_RETURN,
    T_FOR,
    T_WHILE,
    T_DO,
    T_BREAK,
    T_ID,
    T_NUM,
    T_STRING_LITERAL,
    T_CHAR_LITERAL,
    T_BOOL_LITERAL,
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
    T_LPAREN,
    T_RPAREN,
    T_LBRACE,
    T_RBRACE,
    T_SEMICOLON,
    T_COMMA,
    T_DOT,
    T_EOF
};

string getTokenTypeName(TokenType type)
{
    switch (type)
    {
    case T_INT:
        return "INT";
    case T_FLOAT:
        return "FLOAT";
    case T_DOUBLE:
        return "DOUBLE";
    case T_STRING:
        return "STRING";
    case T_BOOL:
        return "BOOL";
    case T_CHAR:
        return "CHAR";
    case T_VOID:
        return "VOID";
    case T_IF:
        return "IF";
    case T_ELSE:
        return "ELSE";
    case T_RETURN:
        return "RETURN";
    case T_FOR:
        return "FOR";
    case T_WHILE:
        return "WHILE";
    case T_DO:
        return "DO";
    case T_BREAK:
        return "BREAK";
    case T_ID:
        return "ID";
    case T_NUM:
        return "NUM";
    case T_STRING_LITERAL:
        return "STRING_LITERAL";
    case T_CHAR_LITERAL:
        return "CHAR_LITERAL";
    case T_BOOL_LITERAL:
        return "BOOL_LITERAL";
    case T_ASSIGN:
        return "ASSIGN";
    case T_PLUS:
        return "PLUS";
    case T_MINUS:
        return "MINUS";
    case T_MUL:
        return "MUL";
    case T_DIV:
        return "DIV";
    case T_GT:
        return "GREATER_THAN";
    case T_LT:
        return "LESS_THAN";
    case T_GE:
        return "GREATER_EQUAL";
    case T_LE:
        return "LESS_EQUAL";
    case T_EQ:
        return "EQUAL_EQUAL";
    case T_NEQ:
        return "NOT_EQUAL";
    case T_AND:
        return "AND";
    case T_OR:
        return "OR";
    case T_NOT:
        return "NOT";
    case T_LPAREN:
        return "LPAREN";
    case T_RPAREN:
        return "RPAREN";
    case T_LBRACE:
        return "LBRACE";
    case T_RBRACE:
        return "RBRACE";
    case T_SEMICOLON:
        return "SEMICOLON";
    case T_COMMA:
        return "COMMA";
    case T_DOT:
        return "DOT";
    case T_EOF:
        return "EOF";
    default:
        return "UNKNOWN";
    }
}

struct SymbolEntry
{
    string name;
    TokenType type;
    string value;
    int line;
    bool initialized;
};

struct FunctionEntry
{
    string name;
    TokenType returnType;
    vector<string> paramNames;
    vector<TokenType> paramTypes;
};

struct Token
{
    TokenType type;
    string value;
    int line;
    int column;
};

class SymbolTable
{
private:
    unordered_map<string, SymbolEntry> symbols;
    unordered_map<string, FunctionEntry> functions;
    vector<string> insertionOrder;

public:
    bool insert(const string &name, TokenType type, int line)
    {
        if (symbols.find(name) != symbols.end())
            return false;
        symbols[name] = {name, type, "", line, false};
        insertionOrder.push_back(name);
        return true;
    }

    SymbolEntry *lookup(const string &name)
    {
        auto it = symbols.find(name);
        return (it != symbols.end()) ? &(it->second) : nullptr;
    }

    void updateValue(const string &name, const string &value)
    {
        auto it = symbols.find(name);
        if (it != symbols.end())
        {
            it->second.value = value;
            it->second.initialized = true;
        }
    }

    bool insertFunction(const string &name, TokenType returnType,
                        const vector<string> &paramNames,
                        const vector<TokenType> &paramTypes)
    {
        if (functions.find(name) != functions.end())
            return false;
        functions[name] = {name, returnType, paramNames, paramTypes};
        return true;
    }

    FunctionEntry *lookupFunction(const string &name)
    {
        auto it = functions.find(name);
        return (it != functions.end()) ? &it->second : nullptr;
    }

    vector<SymbolEntry> getAllSymbols() const
    {
        vector<SymbolEntry> allSymbols;
        for (const auto &symbolName : insertionOrder)
        {
            allSymbols.push_back(symbols.at(symbolName));
        }
        return allSymbols;
    }

    void printFunctions()
    {
        if (functions.empty())
        {
            cout << "\nNo functions declared." << endl;
            return;
        }
        cout << "\n--- Functions ---" << endl;
        cout << left << setw(15) << "Name"
             << setw(10) << "Type"
             << setw(20) << "Parameters" << endl;
        cout << string(45, '-') << endl;
        for (const auto &pair : functions)
        {
            const FunctionEntry &func = pair.second;
            cout << left << setw(15) << func.name
                 << setw(10) << getTokenTypeName(func.returnType);
            string params;
            for (size_t i = 0; i < func.paramNames.size(); ++i)
            {
                params += getTokenTypeName(func.paramTypes[i]) + " " + func.paramNames[i];
                if (i != func.paramNames.size() - 1)
                    params += ", ";
            }
            cout << setw(20) << params << endl;
        }
    }

    void printEntries()
    {
        cout << left << setw(15) << "Name"
             << setw(10) << "Type"
             << setw(10) << "Value"
             << setw(10) << "Line"
             << setw(15) << "Initialized" << endl;
        cout << string(56, '-') << endl;
        for (auto &symbolName : insertionOrder)
        {
            const SymbolEntry &symbol = symbols.at(symbolName);
            cout << left << setw(15) << symbol.name
                 << setw(10) << getTokenTypeName(symbol.type)
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
        ASSIGN,
        ADD,
        SUB,
        MUL,
        DIV,
        LABEL,
        JUMP,
        JUMPIFTRUE,
        JUMPIFFALSE,
        LESSTHAN,
        GREATERTHAN,
        LESSTHANEQUAL,
        GREATERTHANEQUAL,
        EQUALS,
        RETURN,
        CALL
    };

    OpType op;
    string result;
    string arg1;
    string arg2;
    string label;

    TACInstruction(OpType op, string result, string arg1, string arg2)
        : op(op), result(result), arg1(arg1), arg2(arg2) {}

    TACInstruction(OpType op, string result, string arg1)
        : op(op), result(result), arg1(arg1) {}

    TACInstruction(OpType op, string label)
        : op(op), label(label) {}

    TACInstruction(OpType op, string result, string functionName, const vector<string> &args)
        : op(op), result(result), label(functionName)
    {
        arg1 = "";
        for (size_t i = 0; i < args.size(); ++i)
        {
            arg1 += args[i];
            if (i != args.size() - 1)
                arg1 += ", ";
        }
    }

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
        case CALL:
            return result + " = call " + label + "(" + arg1 + ")";
        default:
            return "unknown instruction";
        }
    }
};

class X86AssemblyGenerator
{
private:
    vector<TACInstruction> &tacInstructions;
    vector<string> assemblyCode;
    unordered_map<string, string> variableToMemory;
    unordered_map<string, bool> initializedVariables;
    int stackSize = 0;

    void allocateStackSpace(const string &var, bool initialized)
    {
        stackSize += 4;
        variableToMemory[var] = "[ebp-" + to_string(stackSize) + "]";
        initializedVariables[var] = initialized;
    }

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
    X86AssemblyGenerator(vector<TACInstruction> &tac) : tacInstructions(tac) {}

    vector<string> generateAssembly(const SymbolTable &symbolTable)
    {
        assemblyCode.push_back("section .data");
        assemblyCode.push_back("section .bss");
        assemblyCode.push_back("section .text");
        assemblyCode.push_back("global _start");
        assemblyCode.push_back("_start:");
        assemblyCode.push_back("    push ebp");
        assemblyCode.push_back("    mov ebp, esp");
        auto symbols = symbolTable.getAllSymbols();
        for (const auto &entry : symbols)
        {
            allocateStackSpace(entry.name, entry.initialized);
        }
        assemblyCode.push_back("    sub esp, " + to_string(stackSize));
        allocateUninitializedVariables();

        for (const auto &tac : tacInstructions)
        {
            generateInstructionAssembly(tac);
        }

        assemblyCode.push_back("    mov esp, ebp");
        assemblyCode.push_back("    pop ebp");
        assemblyCode.push_back("    mov eax, 1");
        assemblyCode.push_back("    xor ebx, ebx");
        assemblyCode.push_back("    int 0x80");

        return assemblyCode;
    }

    string trim(const string &s)
    {
        size_t start = s.find_first_not_of(" \t\n\r");
        size_t end = s.find_last_not_of(" \t\n\r");
        return (start == string::npos) ? "" : s.substr(start, end - start + 1);
    }

    void generateInstructionAssembly(const TACInstruction &tac)
    {
        assemblyCode.push_back("; " + tac.toString());

        // Helper lambda to move a variable/constant into eax
        auto moveToEAX = [&](const string &operand)
        {
            assemblyCode.push_back("    mov eax, " + getOperandLocation(operand));
        };

        // Helper lambda to compare arg1 and arg2 and set a boolean result in eax (0 or 1)
        auto generateComparison = [&](const string &arg1, const string &arg2, const string &setInstr)
        {
            // Move arg1 into eax
            moveToEAX(arg1);
            // Compare eax with arg2
            assemblyCode.push_back("    cmp eax, " + getOperandLocation(arg2));
            // Set AL based on condition
            assemblyCode.push_back("    " + setInstr + " al");
            // Zero-extend AL into EAX
            assemblyCode.push_back("    movzx eax, al");
        };

        switch (tac.op)
        {
        case TACInstruction::ASSIGN:
            // result = arg1
            moveToEAX(tac.arg1);
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            initializedVariables[tac.result] = true;
            break;

        case TACInstruction::ADD:
            // result = arg1 + arg2
            moveToEAX(tac.arg1);
            assemblyCode.push_back("    add eax, " + getOperandLocation(tac.arg2));
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            break;

        case TACInstruction::SUB:
            // result = arg1 - arg2
            moveToEAX(tac.arg1);
            assemblyCode.push_back("    sub eax, " + getOperandLocation(tac.arg2));
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            break;

        case TACInstruction::MUL:
            // result = arg1 * arg2
            // We'll use imul for signed multiply
            moveToEAX(tac.arg1);
            assemblyCode.push_back("    imul " + getOperandLocation(tac.arg2));
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            break;

        case TACInstruction::DIV:
            // result = arg1 / arg2
            // EAX holds dividend; EDX must be sign-extended for idiv
            moveToEAX(tac.arg1);
            assemblyCode.push_back("    cdq");
            assemblyCode.push_back("    idiv " + getOperandLocation(tac.arg2));
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            break;

        case TACInstruction::LESSTHAN:
            // result = (arg1 < arg2) ? 1 : 0
            generateComparison(tac.arg1, tac.arg2, "setl");
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            break;

        case TACInstruction::GREATERTHAN:
            // result = (arg1 > arg2) ? 1 : 0
            generateComparison(tac.arg1, tac.arg2, "setg");
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            break;

        case TACInstruction::LESSTHANEQUAL:
            // result = (arg1 <= arg2) ? 1 : 0
            generateComparison(tac.arg1, tac.arg2, "setle");
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            break;

        case TACInstruction::GREATERTHANEQUAL:
            // result = (arg1 >= arg2) ? 1 : 0
            generateComparison(tac.arg1, tac.arg2, "setge");
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            break;

        case TACInstruction::EQUALS:
            // result = (arg1 == arg2) ? 1 : 0
            generateComparison(tac.arg1, tac.arg2, "sete");
            assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            break;

        case TACInstruction::LABEL:
            // label:
            assemblyCode.push_back(tac.label + ":");
            break;

        case TACInstruction::JUMP:
            // goto label
            assemblyCode.push_back("    jmp " + tac.label);
            break;

        case TACInstruction::JUMPIFTRUE:
            // if arg1 != 0 goto label
            // Check if arg1 is true (non-zero)
            moveToEAX(tac.arg1);
            assemblyCode.push_back("    cmp eax, 0");
            assemblyCode.push_back("    jne " + tac.label);
            break;

        case TACInstruction::JUMPIFFALSE:
            // if arg1 == 0 goto label
            moveToEAX(tac.arg1);
            assemblyCode.push_back("    cmp eax, 0");
            assemblyCode.push_back("    je " + tac.label);
            break;

        case TACInstruction::RETURN:
            // return result
            // According to System V ABI: return value in EAX
            // Just move our result into EAX
            if (!tac.result.empty())
            {
                assemblyCode.push_back("    mov eax, " + getOperandLocation(tac.result));
            }
            // After this, we expect end of function cleanup (handled after all TAC instructions)
            break;

        case TACInstruction::CALL:
        {
            // result = call function(args)
            // Push args right-to-left
            vector<string> args;
            {
                string argStr = tac.arg1;
                stringstream ss(argStr);
                string arg;
                vector<string> reversedArgs;
                while (getline(ss, arg, ','))
                {
                    arg = trim(arg);
                    reversedArgs.push_back(arg);
                }
                // The original code pushes args in the order they appear.
                // The System V ABI for Linux x86 prefers registers for first arguments,
                // but for simplicity, we do stack-based calls.
                // Just ensure arguments are pushed in reverse order if needed.
                for (auto it = reversedArgs.rbegin(); it != reversedArgs.rend(); ++it)
                {
                    assemblyCode.push_back("    push " + getOperandLocation(*it));
                }
            }
            assemblyCode.push_back("    call " + tac.label);

            // Remove arguments from stack
            if (!tac.arg1.empty())
            {
                int argCount = 1;
                // Count arguments by splitting by comma
                {
                    stringstream countSS(tac.arg1);
                    string tempArg;
                    argCount = 0;
                    while (getline(countSS, tempArg, ','))
                        argCount++;
                }
                assemblyCode.push_back("    add esp, " + to_string(argCount * 4));
            }

            // Move return value from EAX to result
            if (!tac.result.empty())
            {
                assemblyCode.push_back("    mov " + getOperandLocation(tac.result) + ", eax");
            }
        }
        break;

        default:
            // Unknown or unimplemented instruction
            assemblyCode.push_back("; Unhandled TAC instruction");
            break;
        }
    }

    string getOperandLocation(const string &operand)
    {
        if (variableToMemory.find(operand) != variableToMemory.end())
        {
            return variableToMemory[operand];
        }
        return operand;
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
        keywords["void"] = T_VOID;
        keywords["char"] = T_CHAR;
        keywords["agr"] = T_IF;
        keywords["wrna"] = T_ELSE;
        keywords["waps"] = T_RETURN;
        keywords["chliyaG"] = T_FOR;
        keywords["jidotak"] = T_WHILE;
        keywords["kroG"] = T_DO;
        keywords["rukja"] = T_BREAK;
        keywords["true"] = T_BOOL_LITERAL;
        keywords["false"] = T_BOOL_LITERAL;
    }

    vector<Token> tokenize()
    {
        vector<Token> tokens;
        while (pos < src.size())
        {
            char current = src[pos];
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

            if (current == '/' && pos + 1 < src.size())
            {
                if (src[pos + 1] == '/')
                {
                    while (pos < src.size() && src[pos] != '\n')
                        pos++;
                    continue;
                }
                else if (src[pos + 1] == '*')
                {
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

            if (current == '"')
            {
                tokens.push_back(consumeString());
                continue;
            }

            if (current == '\'')
            {
                tokens.push_back(consumeChar());
                continue;
            }

            if (isdigit(current))
            {
                tokens.push_back(consumeNumber());
                continue;
            }

            if (isalpha(current) || current == '_')
            {
                tokens.push_back(consumeIdentifier());
                continue;
            }

            Token token = consumeOperator();
            if (token.type != T_EOF)
            {
                tokens.push_back(token);
                continue;
            }

            stringstream error;
            error << "Unexpected character '" << current << "' at line " << line << ", column " << column;
            throw runtime_error(error.str());
        }

        tokens.push_back({T_EOF, "", line, column});
        return tokens;
    }

    Token consumeString()
    {
        int startLine = line;
        int startColumn = column;
        string value = "";
        pos++;
        column++;
        while (pos < src.size() && src[pos] != '"')
        {
            if (src[pos] == '\n')
                throw runtime_error("Unterminated string literal");
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
                    break;
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
            throw runtime_error("Unterminated string literal");
        pos++;
        column++;
        return {T_STRING_LITERAL, value, startLine, startColumn};
    }

    Token consumeChar()
    {
        int startLine = line;
        int startColumn = column;
        pos++;
        column++;
        string value = "";
        if (pos >= src.size())
            throw runtime_error("Unterminated character literal");
        if (src[pos] == '\\')
        {
            pos++;
            column++;
            if (pos >= src.size())
                throw runtime_error("Unterminated character literal");
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
                break;
            }
        }
        else
        {
            value = src[pos];
        }
        pos++;
        column++;
        if (pos >= src.size() || src[pos] != '\'')
            throw runtime_error("Unterminated character literal");
        pos++;
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
                    throw runtime_error("Invalid number format");
                isFloat = true;
            }
            value += src[pos];
            pos++;
            column++;
        }
        if (isFloat)
        {
            try
            {
                stof(value);
            }
            catch (...)
            {
                throw runtime_error("Invalid float literal");
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
            return {it->second, value, startLine, startColumn};
        return {T_ID, value, startLine, startColumn};
    }

    Token consumeOperator()
    {
        int startLine = line;
        int startColumn = column;
        char current = src[pos];
        pos++;
        column++;
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

    struct ExprResult
    {
        string place;
        string code;
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
            error("Expected token");
        }
        pos++;
    }

    void parseStatement()
    {
        if (tokens[pos].type == T_EOF)
            return;
        Token current = tokens[pos];
        switch (current.type)
        {
        case T_INT:
        case T_FLOAT:
        case T_DOUBLE:
        case T_STRING:
        case T_BOOL:
        case T_CHAR:
        case T_VOID:
            if (pos + 2 < tokens.size() && tokens[pos + 1].type == T_ID && tokens[pos + 2].type == T_LPAREN)
            {
                parseFunction();
            }
            else
            {
                parseDeclaration();
            }
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
        TokenType dataType = tokens[pos].type;
        pos++;
        if (tokens[pos].type != T_ID)
            error("Expected identifier after type declaration");
        string varName = tokens[pos].value;
        pos++;
        if (!symbolTable.insert(varName, dataType, tokens[pos - 1].line))
            error("Variable '" + varName + "' already declared");
        if (tokens[pos].type == T_ASSIGN)
        {
            pos++;
            ExprResult expr = parseExpression();
            symbolTable.updateValue(varName, expr.place);
            tacInstructions.push_back(TACInstruction(TACInstruction::ASSIGN, varName, expr.place));
        }
        expect(T_SEMICOLON);
    }

    void parseAssignment()
    {
        string varName = tokens[pos].value;
        pos++;
        SymbolEntry *entry = symbolTable.lookup(varName);
        if (!entry)
            error("Variable '" + varName + "' not declared");
        expect(T_ASSIGN);
        ExprResult expr = parseExpression();
        tacInstructions.push_back(TACInstruction(TACInstruction::ASSIGN, varName, expr.place));
        symbolTable.updateValue(varName, expr.place);
        expect(T_SEMICOLON);
    }

    void parseFunction()
    {
        TokenType returnType = tokens[pos].type;
        pos++;
        string funcName = tokens[pos].value;
        expect(T_ID);
        expect(T_LPAREN);
        vector<string> paramNames;
        vector<TokenType> paramTypes;
        while (tokens[pos].type != T_RPAREN)
        {
            TokenType pType = tokens[pos].type;
            if (!(pType == T_INT || pType == T_FLOAT || pType == T_DOUBLE ||
                  pType == T_BOOL || pType == T_CHAR || pType == T_STRING))
            {
                error("Expected parameter type");
            }
            pos++;
            if (tokens[pos].type != T_ID)
                error("Expected parameter name after type");
            string pName = tokens[pos].value;
            pos++;
            symbolTable.insert(pName, pType, tokens[pos - 1].line);
            paramNames.push_back(pName);
            paramTypes.push_back(pType);
            if (tokens[pos].type == T_COMMA)
            {
                pos++;
            }
            else
                break;
        }
        expect(T_RPAREN);
        if (!symbolTable.insertFunction(funcName, returnType, paramNames, paramTypes))
            error("Function '" + funcName + "' is already declared");
        expect(T_LBRACE);
        while (tokens[pos].type != T_RBRACE && tokens[pos].type != T_EOF)
            parseStatement();
        expect(T_RBRACE);
    }

    void parseIfStatement()
    {
        pos++;
        expect(T_LPAREN);
        parseExpression();
        expect(T_RPAREN);
        parseStatement();
        if (pos < tokens.size() && tokens[pos].type == T_ELSE)
        {
            pos++;
            parseStatement();
        }
    }

    void parseWhileStatement()
    {
        pos++;
        expect(T_LPAREN);
        string startLabel = newLabel();
        string endLabel = newLabel();
        tacInstructions.push_back(TACInstruction(TACInstruction::LABEL, startLabel));
        ExprResult condition = parseExpression();
        expect(T_RPAREN);
        tacInstructions.push_back(TACInstruction(TACInstruction::JUMPIFFALSE, condition.place, endLabel));
        parseStatement();
        tacInstructions.push_back(TACInstruction(TACInstruction::JUMP, startLabel));
        tacInstructions.push_back(TACInstruction(TACInstruction::LABEL, endLabel));
    }

    void parseForStatement()
    {
        pos++;
        expect(T_LPAREN);
        parseDeclaration();
        string startLabel = newLabel();
        string endLabel = newLabel();
        tacInstructions.push_back(TACInstruction(TACInstruction::LABEL, startLabel));
        ExprResult condition;
        if (tokens[pos].type != T_SEMICOLON)
            condition = parseExpression();
        expect(T_SEMICOLON);
        if (!condition.place.empty())
            tacInstructions.push_back(TACInstruction(TACInstruction::JUMPIFFALSE, condition.place, endLabel));
        if (tokens[pos].type != T_RPAREN)
            parseExpression();
        expect(T_RPAREN);
        parseBlock();
        tacInstructions.push_back(TACInstruction(TACInstruction::JUMP, startLabel));
        tacInstructions.push_back(TACInstruction(TACInstruction::LABEL, endLabel));
    }

    void parseReturnStatement()
    {
        pos++;
        if (tokens[pos].type != T_SEMICOLON)
        {
            ExprResult expr = parseExpression();
            tacInstructions.push_back(TACInstruction(TACInstruction::RETURN, expr.place));
        }
        expect(T_SEMICOLON);
    }

    void parseBlock()
    {
        expect(T_LBRACE);
        while (pos < tokens.size() && tokens[pos].type != T_RBRACE)
            parseStatement();
        expect(T_RBRACE);
    }

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
            pos++;
            ExprResult right = parseTerm();
            string temp = newTemp();
            switch (op)
            {
            case T_PLUS:
                tacInstructions.push_back(TACInstruction(TACInstruction::ADD, temp, result.place, right.place));
                break;
            case T_MINUS:
                tacInstructions.push_back(TACInstruction(TACInstruction::SUB, temp, result.place, right.place));
                break;
            case T_GT:
                tacInstructions.push_back(TACInstruction(TACInstruction::GREATERTHAN, temp, result.place, right.place));
                break;
            case T_LT:
                tacInstructions.push_back(TACInstruction(TACInstruction::LESSTHAN, temp, result.place, right.place));
                break;
            case T_GE:
                tacInstructions.push_back(TACInstruction(TACInstruction::GREATERTHANEQUAL, temp, result.place, right.place));
                break;
            case T_LE:
                tacInstructions.push_back(TACInstruction(TACInstruction::LESSTHANEQUAL, temp, result.place, right.place));
                break;
            case T_EQ:
                tacInstructions.push_back(TACInstruction(TACInstruction::EQUALS, temp, result.place, right.place));
                break;
            default:
                break;
            }
            result.place = temp;
        }
        return result;
    }

    ExprResult parseFactor()
    {
        if (pos >= tokens.size())
            error("Unexpected end of input");
        TokenType type = tokens[pos].type;
        ExprResult result;
        if (type == T_ID)
        {
            string name = tokens[pos].value;
            pos++;
            if (pos < tokens.size() && tokens[pos].type == T_LPAREN)
            {
                if (symbolTable.lookupFunction(name))
                {
                    expect(T_LPAREN);
                    vector<string> args;
                    while (tokens[pos].type != T_RPAREN)
                    {
                        ExprResult arg = parseExpression();
                        args.push_back(arg.place);
                        if (tokens[pos].type == T_COMMA)
                            pos++;
                    }
                    expect(T_RPAREN);
                    string temp = newTemp();
                    tacInstructions.push_back(TACInstruction(TACInstruction::CALL, temp, name));
                    result.place = temp;
                }
                else
                {
                    error("Function '" + name + "' not declared");
                }
            }
            else
            {
                SymbolEntry *entry = symbolTable.lookup(name);
                if (!entry)
                    error("Variable '" + name + "' used before declaration");
                result.place = name;
            }
        }
        else if (type == T_NUM || type == T_FLOAT ||
                 type == T_STRING_LITERAL || type == T_CHAR_LITERAL ||
                 type == T_BOOL_LITERAL)
        {
            result.place = tokens[pos].value;
            pos++;
        }
        else if (type == T_LPAREN)
        {
            pos++;
            result = parseExpression();
            expect(T_RPAREN);
        }
        else if (type == T_NOT || type == T_MINUS)
        {
            string op = tokens[pos].value;
            pos++;
            ExprResult operand = parseFactor();
            string temp = newTemp();
            result.place = temp;
        }
        else
        {
            error("Unexpected token in expression");
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
            pos++;
            ExprResult right = parseFactor();
            string temp = newTemp();
            if (op == T_MUL)
            {
                tacInstructions.push_back(TACInstruction(TACInstruction::MUL, temp, result.place, right.place));
            }
            else
            {
                tacInstructions.push_back(TACInstruction(TACInstruction::DIV, temp, result.place, right.place));
            }
            result.place = temp;
        }
        return result;
    }

    string newTemp() { return "t" + to_string(++tempVarCounter); }
    string newLabel() { return "L" + to_string(++labelCounter); }

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
            suggestFix(e.what());
        }
    }

    void suggestFix(const string &error)
    {
        if (error.find("missing semicolon") != string::npos)
        {
            cout << "Suggestion: Add a semicolon (;) at the end of the statement" << endl;
        }
    }

    void generateAssembly(const string &outputFile)
    {
        X86AssemblyGenerator generator(tacInstructions);
        vector<string> assembly = generator.generateAssembly(symbolTable);
        ofstream outFile(outputFile);
        if (outFile.is_open())
        {
            for (const auto &line : assembly)
                outFile << line << endl;
            outFile.close();
            cout << "x86 assembly code generated successfully in " << outputFile << endl;
        }
        else
        {
            cerr << "Error: Unable to open output file" << endl;
        }
    }

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
        if (argc != 3)
        {
            cerr << "Usage: " << argv[0] << " <input_file> <output_file>" << endl;
            return 1;
        }

        ifstream inputFile(argv[1]);
        if (!inputFile.is_open())
        {
            cerr << "Error: Cannot open file '" << argv[1] << "'" << endl;
            return 1;
        }

        stringstream buffer;
        buffer << inputFile.rdbuf();
        string input = buffer.str();
        inputFile.close();

        cout << "\n=== Compiler Started ===" << endl;
        cout << "Processing file: " << argv[1] << endl;
        cout << "File size: " << input.length() << " bytes" << endl;

        cout << "\n--- Lexical Analysis ---" << endl;
        Lexer lexer(input);
        vector<Token> tokens;
        try
        {
            tokens = lexer.tokenize();
            cout << "Tokenization successful!" << endl;
            cout << "\nTokens found:" << endl;
            cout << string(56, '-') << endl;
            cout << left << setw(20) << "TYPE" << setw(20) << "VALUE"
                 << setw(10) << "LINE" << "COLUMN" << endl;
            cout << string(56, '-') << endl;
            // printing tokens:
            for (const auto &token : tokens)
            {
                cout << left << setw(20) << getTokenTypeName(token.type)
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

        cout << "\n--- Parsing ---" << endl;
        try
        {
            Parser parser(tokens);
            parser.parse();
            cout << "Parsing completed successfully!" << endl;

            cout << "\n--- Symbol Table ---" << endl;
            SymbolTable &symbolTable = parser.getSymbolTable();
            symbolTable.printEntries();
            symbolTable.printFunctions();
            parser.printTAC();
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
        cerr << "\nUnexpected error: " << e.what() << endl;
        return 1;
    }
}