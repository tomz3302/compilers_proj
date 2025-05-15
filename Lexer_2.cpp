#include <iostream>
#include <vector>
#include <algorithm>
#include <regex>
#include <string>
#include <iomanip> 
#include <map>
#include <cctype>
#include <fstream>
#include <functional>
using namespace std;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////LEXICAL ANALYZER/////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


struct lexeme {
    string value;
    int lineNumber;
};

struct SymbolInfo {
    string name;
    string category;
    string dataType;
    int index;
    string scope;
};

struct LexicalError {
    string message;
    int line;
};

bool isDelimiter(char ch) {
    return ch == ' ' || ch == '\t' || ch == '\n';
}

bool isSymbol(char ch) {
    string symbols = "():{}[]<>=+-*/%!&|,.";
    return symbols.find(ch) != string::npos;
}

string commentsremover(const string& code) {
    string result;
    bool inSingleQuote = false;
    bool inDoubleQuote = false;

    for (size_t i = 0; i < code.length(); ++i) {
        char c = code[i];

        // Always preserve newlines to maintain line numbers
        if (c == '\n') {
            result += c;
            inSingleQuote = false;
            inDoubleQuote = false;
            continue;
        }

        if (c == '\'' && !inDoubleQuote) {
            inSingleQuote = !inSingleQuote;
        }
        else if (c == '\"' && !inSingleQuote) {
            inDoubleQuote = !inDoubleQuote;
        }

        if (c == '#' && !inSingleQuote && !inDoubleQuote) {
            // When we find a comment, add spaces until the newline
            while (i < code.length() && code[i] != '\n') {
                result += ' ';  // Replace comment with spaces to preserve line length
                i++;
            }
            i--;  // Back up so the loop will handle the newline
            continue;
        }

        result += c;
    }

    return result;
}

vector<LexicalError> errors;

// TOKENIZATION
vector<lexeme> tokenize(const string& input) {

    //set up and initialization
    vector<lexeme> tokens; //stores extracted tokens
    string current;
    bool insideString = false;
    bool unterminatedString = false; //flags a string error

    vector<int> indentStack = { 0 }; //stores indentation levels
    size_t pos = 0; //current position in input 
    int currentIndent = 0;
    bool newLine = true; //detects if it started a new parse line
    int lineNumber = 1;

    while (pos < input.length()) {
        char ch = input[pos];

        if (newLine) {
            currentIndent = 0; //if starting a new line reset currentIndent 

            //count spaces and tabs +4
            while (pos < input.length() && (input[pos] == ' ' || input[pos] == '\t')) {
                if (input[pos] == ' ')
                    currentIndent += 1;
                else if (input[pos] == '\t')
                    currentIndent += 4 - (currentIndent % 4);
                pos++;
            }

            //if line is empty then skip it to next line
            if (pos < input.length() && input[pos] == '\n') {
                pos++;
                lineNumber++;
                newLine = true;
                continue;
            }

            //emit IDENT tokens if indent increased  
            if (currentIndent > indentStack.back()) {
                while (currentIndent > indentStack.back()) {
                    indentStack.push_back(indentStack.back() + 4);
                    tokens.push_back({ "INDENT" ,lineNumber });
                }
            }

            //emit DEDENT tokens if indent decreased
            else {
                while (currentIndent < indentStack.back()) {
                    indentStack.pop_back();
                    tokens.push_back({ "DEDENT",lineNumber });
                }
            }
            newLine = false;
            continue;
        }


        if (insideString) {
            current += ch;
            if ((ch == '"' && current.front() == '"') || (ch == '\'' && current.front() == '\'')) {
                //this will allow singles quotes inside double quote strings and the opposite
                tokens.push_back({ current ,lineNumber });
                current.clear();
                insideString = false;
                unterminatedString = false;
            }

            //error handling: undetermined string literal
            else if (ch == '\n') {
                // maslan ("Hello)
                errors.push_back({ "Unterminated string literal", lineNumber });
                insideString = false;
                unterminatedString = true;
                current.clear();
                newLine = true;
                lineNumber++;
            }

        }

        //detects if its inside a string " or '
        else if (ch == '"' || ch == '\'') {
            if (!current.empty()) {
                tokens.push_back({ current ,lineNumber });
                current.clear();
            }
            insideString = true;
            current += ch;
        }

        else if (isDelimiter(ch)) {
            if (!current.empty()) {
                tokens.push_back({ current,lineNumber });
                current.clear();
            }
            if (ch == '\n') {
                newLine = true;
                lineNumber++;
            }
        }
        else if (isSymbol(ch)) {
            if (!current.empty()) {
                tokens.push_back({ current,lineNumber });
                current.clear();
            }
            tokens.push_back({ string(1, ch), lineNumber });
        }


        //handling floats
        else if (isdigit(ch)) {
            current += ch;
            pos++;
            bool isFloat = false;
            bool isValid = true;

            while (pos < input.length() && (isdigit(input[pos]) || input[pos] == '.')) {
                if (input[pos] == '.') {
                    if (isFloat) {
                        isValid = false;
                    }
                    isFloat = true;
                }
                current += input[pos];
                pos++;
            }

            // check if identifier valid
            if (pos < input.length() && (isalpha(input[pos]) || input[pos] == '_')) {
                string invalid = current;

                while (pos < input.length() && (isalnum(input[pos]) || input[pos] == '_')) {
                    invalid += input[pos];
                    pos++;
                }

                //error handling: invalid identifier
                errors.push_back({ "Invalid identifier: '" + invalid + "'", lineNumber });
                current.clear();
                continue;
            }


            if (isValid) {
                if (isFloat && regex_match(current, regex("^[0-9]+\\.[0-9]+$")))
                    tokens.push_back({ current, lineNumber });

                else if (!isFloat && regex_match(current, regex("^[0-9]+$")))
                    tokens.push_back({ current, lineNumber });

                else
                    errors.push_back({ "Invalid number format: '" + current + "'", lineNumber });
            }

            else {
                errors.push_back({ "Invalid float format: '" + current + "'", lineNumber });
            }

            current.clear();
            continue;
        }


        else if (isalpha(ch) || ch == '_') {
            current += ch;
            pos++;
            while (pos < input.length() && (isalnum(input[pos]) || input[pos] == '_')) {
                current += input[pos];
                pos++;
            }
            tokens.push_back({ current, lineNumber });
            current.clear();
            continue;
        }
        else {
            // invalid character
            errors.push_back({ string("Invalid character: '") + ch + "'", lineNumber });
        }

        pos++;
    }

    if (!current.empty() && !unterminatedString) {
        tokens.push_back({ current ,lineNumber });
    }

    while (indentStack.back() > 0) {
        indentStack.pop_back();
        tokens.push_back({ "DEDENT",lineNumber });
    }

    return tokens;
}




// TOKEN TYPE
string getTokenType(const string& lexeme) {
    vector<string> keywords = { "def", "return", "if", "else", "elif", "while", "for", "in", "True", "False", "None", "import", "from", "as", "input" };
    if (find(keywords.begin(), keywords.end(), lexeme) != keywords.end()) return "KEYWORD";

    vector<string> operators = { "+", "-", "*", "/", "%", "**", "//", "==", "!=", "<", "<=", ">", ">=", "and", "or", "not" };
    if (find(operators.begin(), operators.end(), lexeme) != operators.end()) return "OPERATOR";

    if (lexeme == "=") return "ASSIGNMENT";
    if (lexeme == "INDENT" || lexeme == "DEDENT") return lexeme;

    string symbols = "():{}[]<>=+-*/%!&|,.";
    if (lexeme.length() == 1 && symbols.find(lexeme) != string::npos) return "SYMBOL";

    if (regex_match(lexeme, regex("^[0-9]+$"))) return "INTEGER";
    if (regex_match(lexeme, regex("^[0-9]+\\.[0-9]+$"))) return "FLOAT";
    //omar edited the line under this comment 
    if (regex_match(lexeme, regex(R"(^(['\"])(.*)\1$)"))) return "STRING";
    if (regex_match(lexeme, regex("^[a-zA-Z_][a-zA-Z0-9_]*$"))) return "IDENTIFIER";

    return "UNKNOWN";
}





// SYMBOL TABLE
map<string, SymbolInfo> buildSymbolTable(const vector<lexeme>& tokens) {
    map<string, SymbolInfo> symbolTable;
    int symbolIndex = 1;

    for (size_t i = 0; i < tokens.size(); ++i) {
        const string& lexVal = tokens[i].value;
        string type = getTokenType(lexVal);

        if (type == "IDENTIFIER") {
            string category = "variable";
            string dataType = "0";

            if (i + 1 < tokens.size() && tokens[i + 1].value == "=") {
                string value = tokens[i + 2].value;
                if (regex_match(value, regex("^[0-9]+$"))) dataType = "int";
                else if (regex_match(value, regex("^[0-9]+\\.[0-9]+$"))) dataType = "float";
                else if (regex_match(value, regex("^\".*\"$"))) dataType = "str";
            }

            if (i + 1 < tokens.size() && tokens[i + 1].value == "(") category = "function";
            if (i > 0) {
                string prev = tokens[i - 1].value;
                if (prev == "struct") category = "struct";
                else if (prev == "class") category = "class";
            }

            if (symbolTable.find(lexVal) == symbolTable.end()) {
                SymbolInfo info;
                info.name = lexVal;
                info.category = category;
                info.dataType = dataType;
                info.index = symbolIndex;
                symbolTable[lexVal] = info;
                symbolIndex++;
            }
        }
    }

    return symbolTable;
}



// PRINT SYMBOL TABLE
void printSymbolTable(const map<string, SymbolInfo>& table) {
    std::cout << "\nSymbol Table:\n";
    std::cout << left
        << setw(15) << "Index:"
        << setw(15) << "Name:"
        << setw(15) << "Category:"
        << setw(15) << "DataType:"
        << endl;
    std::cout << string(60, '-') << endl;

    vector<SymbolInfo> sortedSymbols;
    for (const auto& info : table) {
        sortedSymbols.push_back(info.second);
    }

    sort(sortedSymbols.begin(), sortedSymbols.end(), [](const SymbolInfo& a, const SymbolInfo& b) {
        return a.index < b.index;
        });

    for (const auto& info : sortedSymbols) {
        std::cout << left
            << setw(15) << info.index
            << setw(15) << info.name
            << setw(15) << info.category
            << setw(15) << info.dataType
            << endl;
    }
}




// PRINT ERRORS
void printErrors() {
    if (errors.empty()) return;
    std::cout << "\nLexical Errors:\n";
    for (const auto& err : errors) {
        std::cout << "Line " << err.line << ": " << err.message << endl;
    }
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////SYNTAX ANALYZER/////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

enum NodeType {
    NODE_PROGRAM,
    NODE_STATEMENT,
    NODE_ASSIGN,
    NODE_EXP,
    NODE_TERM,
    NODE_FACTOR,
    NODE_FUNC_CALL,
    NODE_RETURN,
    NODE_IMPORT,
    NODE_PASS,
    NODE_IF,
    NODE_WHILE,
    NODE_FOR,
    NODE_FUNC_DEF,
    NODE_CLASS,
    NODE_TRY,
    NODE_IDENTIFIER,
    NODE_OPERATOR,
    NODE_NUMBER,
    NODE_COMPARISON,
    NODE_EXCEPT,
    NODE_STRING,
    NODE_SUITE
};

struct ParseNode {
    NodeType type;
    string value;
    int lineNumber;
    vector<ParseNode*> children; //stored sub nodes
};

struct ParserState {
    vector<lexeme> tokens;    // Input tokens from lexer
    size_t currentToken;      // Current position in token stream
    vector<string> errors;    // Syntax errors encountered
};

//HELPER FUNCTIONS
ParseNode* createNode(NodeType type, const string& value = "", int lineNumber = 0) {
    ParseNode* node = new ParseNode();
    node->type = type;
    node->value = value;
    node->lineNumber = lineNumber;
    return node;
}

void addChild(ParseNode* parent, ParseNode* child) {
    if (parent && child) {
        parent->children.push_back(child);
    }
}

bool match(ParserState& state, const string& expected) {
    if (state.currentToken >= state.tokens.size())
        return false;
    return state.tokens[state.currentToken].value == expected;
}

void advance(ParserState& state) {
    if (state.currentToken < state.tokens.size()) {
        state.currentToken++;
    }
}

void addError(ParserState& state, const string& message) {
    if (state.currentToken < state.tokens.size()) {
        state.errors.push_back("Syntax Error at line " +
            to_string(state.tokens[state.currentToken].lineNumber) + ": " + message);
    }
}

ParseNode* parseExpression(ParserState& state);
ParseNode* parseStatement(ParserState& state);

// Forward declarations
ParseNode* parseFunctionCall(ParserState& state);
ParseNode* parseSuite(ParserState& state);

//GRAMMAR RULES
ParseNode* parseAssignment(ParserState& state) {
    ParseNode* node = createNode(NODE_ASSIGN);
    addChild(node, createNode(NODE_IDENTIFIER, state.tokens[state.currentToken].value));
    advance(state);

    if (!match(state, "=")) {
        addError(state, "Expected '=' in assignment");
        return node;
    }
    advance(state);
    addChild(node, parseExpression(state));
    return node;
}

// Add this helper function for error recovery
void skipToNextStatement(ParserState& state) {
    while (state.currentToken < state.tokens.size()) {
        string currentValue = state.tokens[state.currentToken].value;
        string currentType = getTokenType(currentValue);
        
        // Check for compound statement keywords
        if (match(state, "INDENT") || match(state, "DEDENT") || 
            match(state, "def") || match(state, "class") || 
            match(state, "if") || match(state, "while") || 
            match(state, "for") || match(state, "try")) {
            break;
        }
        
        // Check for simple statement keywords
        if (match(state, "return") || match(state, "pass") || 
            match(state, "import") || match(state, "break") || 
            match(state, "continue")) {
            break;
        }
        
        // Check for potential assignment or expression start
        if (currentType == "IDENTIFIER") {
            // Look ahead for assignment
            if (state.currentToken + 1 < state.tokens.size() && 
                state.tokens[state.currentToken + 1].value == "=") {
                break;  // Found assignment statement
            }
            // Look ahead for function call
            if (state.currentToken + 1 < state.tokens.size() && 
                state.tokens[state.currentToken + 1].value == "(") {
                break;  // Found function call expression
            }
            break;  // Found potential expression starting with identifier
        }
        
        // Check for expressions starting with literals
        if (currentType == "INTEGER" || currentType == "FLOAT" || 
            currentType == "STRING" || match(state, "(")) {
            break;  // Found expression starting with literal or parenthesis
        }

        advance(state);
    }
}

ParseNode* parseFactor(ParserState& state) {
    if (state.currentToken >= state.tokens.size()) {
        addError(state, "Unexpected end of input");
        return nullptr;
    }

    ParseNode* node = createNode(NODE_FACTOR);
    string tokenType = getTokenType(state.tokens[state.currentToken].value);
    size_t nextToken = state.currentToken + 1;

    try {
        if (tokenType == "INTEGER" || tokenType == "FLOAT") {
            addChild(node, createNode(NODE_NUMBER, state.tokens[state.currentToken].value));
            advance(state);
        }
        else if ((tokenType == "IDENTIFIER") &&
            nextToken < state.tokens.size() &&
            state.tokens[nextToken].value == "(") {
            addChild(node, parseFunctionCall(state));
        }
        else if (tokenType == "IDENTIFIER") {
            addChild(node, createNode(NODE_IDENTIFIER, state.tokens[state.currentToken].value));
            advance(state);
        }
        else if (match(state, "(")) {
            advance(state);  // consume '('
            int parenCount = 1;
            size_t startToken = state.currentToken;
            
            ParseNode* expr = parseExpression(state);
            if (!expr) {
                addError(state, "Invalid expression inside parentheses");
                skipToNextStatement(state);
                return nullptr;
            }
            addChild(node, expr);
            
            if (!match(state, ")")) {
                // If we hit end of input or a newline before finding closing paren
                if (state.currentToken >= state.tokens.size() || 
                    state.tokens[state.currentToken].value == "\n") {
                    addError(state, "Unclosed parenthesis in expression");
                    // Don't advance - let error recovery handle it
                    return nullptr;
                }
                addError(state, "Expected ')'");
                return nullptr;
            }
            advance(state);  // consume ')'
        }
        else if (tokenType == "STRING") {
            if ((state.tokens[state.currentToken].value.front() == '"' && 
                 state.tokens[state.currentToken].value.back() != '"') ||
                (state.tokens[state.currentToken].value.front() == '\'' && 
                 state.tokens[state.currentToken].value.back() != '\'')) {
                addError(state, "Unclosed string literal");
                advance(state);
                return nullptr;
            }
            addChild(node, createNode(NODE_STRING, state.tokens[state.currentToken].value));
            advance(state);
        }
        else if (match(state, ">>>") || match(state, "<<<") || match(state, ">>") || match(state, "<<")) {
            addError(state, "Invalid operator '" + state.tokens[state.currentToken].value + "'");
            advance(state);
            return nullptr;
        }
        else {
            if (state.errors.empty() || 
                state.errors.back().find("Expected") == string::npos) {
                addError(state, "Invalid expression");
            }
            skipToNextStatement(state);
            return nullptr;
        }
    }
    catch (...) {
        addError(state, "Invalid expression");
        skipToNextStatement(state);
        return nullptr;
    }

    return node;
}

ParseNode* parseTerm(ParserState& state) {
    ParseNode* node = createNode(NODE_TERM);
    ParseNode* factor = parseFactor(state);
    if (!factor) {
        return nullptr;  // Propagate the error up
    }
    addChild(node, factor);

    while (state.currentToken < state.tokens.size()) {
        // Only accept valid multiplicative operators
        if (match(state, "*") || match(state, "/")) {
            string op = state.tokens[state.currentToken].value;
            advance(state);
            ParseNode* opNode = createNode(NODE_OPERATOR, op);
            addChild(node, opNode);
            
            ParseNode* nextFactor = parseFactor(state);
            if (!nextFactor) {
                addError(state, "Invalid or incomplete expression after operator '" + op + "'");
                return node;
            }
            addChild(node, nextFactor);
        } else {
            break;
        }
    }

    return node;
}

ParseNode* parseExpression(ParserState& state) {
    ParseNode* node = createNode(NODE_EXP);
    ParseNode* term = parseTerm(state);
    if (!term) {
        return nullptr;  // Propagate the error up
    }
    addChild(node, term);

    while (state.currentToken < state.tokens.size()) {
        // Only accept valid additive operators
        if (match(state, "+") || match(state, "-")) {
            string op = state.tokens[state.currentToken].value;
            advance(state);
            ParseNode* opNode = createNode(NODE_OPERATOR, op);
            addChild(node, opNode);
            
            ParseNode* nextTerm = parseTerm(state);
            if (!nextTerm) {
                addError(state, "Invalid or incomplete expression after operator '" + op + "'");
                return node;
            }
            addChild(node, nextTerm);
        } else if (match(state, ">>>") || match(state, "<<<") || match(state, ">>") || match(state, "<<")) {
            // Handle invalid operators explicitly
            addError(state, "Invalid operator '" + state.tokens[state.currentToken].value + "' in expression");
            advance(state);
            return node;
        } else {
            break;
        }
    }

    return node;
}


ParseNode* parseComparison(ParserState& state) {
    ParseNode* node = createNode(NODE_COMPARISON);

    addChild(node, parseExpression(state));

    if (match(state, ">") || match(state, "<") || match(state, ">=") ||
        match(state, "<=") || match(state, "==") || match(state, "!=")) {
        string op = state.tokens[state.currentToken].value;
        advance(state);
        ParseNode* opNode = createNode(NODE_OPERATOR, op);
        addChild(node, opNode);
        addChild(node, parseExpression(state));
    }

    return node;
}

ParseNode* parseIfStatement(ParserState& state) {
    ParseNode* node = createNode(NODE_IF);
    
    advance(state); // consume 'if'
    addChild(node, parseComparison(state));
    
    if (!match(state, ":")) {
        addError(state, "Expected ':' after if condition");
        return node;
    }
    advance(state);

    // Parse the if block
    ParseNode* ifBlock = parseSuite(state);
    if (ifBlock) {
        addChild(node, ifBlock);
    }

    // Handle optional else block
    if (match(state, "else")) {
        advance(state);
        if (!match(state, ":")) {
            addError(state, "Expected ':' after else");
            return node;
        }
        advance(state);
        ParseNode* elseBlock = parseSuite(state);
        if (elseBlock) {
            addChild(node, elseBlock);
        }
    }

    return node;
}

ParseNode* parseWhileStatement(ParserState& state) {
    ParseNode* node = createNode(NODE_WHILE);

    advance(state); // consume 'while'
    addChild(node, parseExpression(state));

    if (!match(state, ":")) {
        addError(state, "Expected ':' after while condition");
        return node;
    }
    advance(state);

    ParseNode* suite = parseSuite(state);
    if (suite != nullptr) {
        addChild(node, suite);
    }

    return node;
}

ParseNode* parseForStatement(ParserState& state) {
    ParseNode* node = createNode(NODE_FOR);
    
    advance(state); // consume 'for'
    
    if (getTokenType(state.tokens[state.currentToken].value) != "IDENTIFIER") {
        addError(state, "Expected identifier after 'for'");
        return node;
    }
    addChild(node, createNode(NODE_IDENTIFIER, state.tokens[state.currentToken].value));
    advance(state);

    if (!match(state, "in")) {
        addError(state, "Expected 'in' after identifier in for loop");
        skipToNextStatement(state);
        return node;
    }
    advance(state);

    ParseNode* iterExpr = parseExpression(state);
    if (iterExpr) {
        addChild(node, iterExpr);
    }

    if (!match(state, ":")) {
        addError(state, "Expected ':' after for loop expression");
        return node;
    }
    advance(state);

    addChild(node, parseSuite(state));
    return node;
}


ParseNode* parseFunctionDef(ParserState& state) {
    ParseNode* node = createNode(NODE_FUNC_DEF);

    if (!match(state, "def")) {
        addError(state, "Expected 'def' keyword");
        return node;
    }
    advance(state);

    if (state.currentToken >= state.tokens.size() || getTokenType(state.tokens[state.currentToken].value) != "IDENTIFIER") {
        addError(state, "Expected function name");
        return node;
    }

    addChild(node, createNode(NODE_IDENTIFIER, state.tokens[state.currentToken].value));
    advance(state);

    if (!match(state, "(")) {
        addError(state, "Expected '(' after function name");
        return node;
    }

    advance(state);

    // parse the parameters
    while (state.currentToken < state.tokens.size() && !match(state, ")")) {
        if (getTokenType(state.tokens[state.currentToken].value) == "IDENTIFIER") {
            addChild(node, createNode(NODE_IDENTIFIER, state.tokens[state.currentToken].value));
            advance(state);
            if (match(state, ","))
                advance(state);
        }
        else {
            addError(state, "Expected parameter name");
            break;
        }
    }

    if (!match(state, ")")) {
        addError(state, "Expected ')'");
        return node;
    }
    advance(state);

    if (!match(state, ":")) {
        addError(state, "Expected ':' after function parameters");
        return node;
    }
    advance(state);

    // Parse function body as a block
    addChild(node, parseSuite(state));

    return node;
}

ParseNode* parseClassDef(ParserState& state) {
    ParseNode* node = createNode(NODE_CLASS);

    advance(state); // consume 'class'

    if (getTokenType(state.tokens[state.currentToken].value) != "IDENTIFIER") {
        addError(state, "Expected class name after 'class'");
        return node;
    }
    addChild(node, createNode(NODE_IDENTIFIER, state.tokens[state.currentToken].value));
    advance(state);

    if (!match(state, ":")) {
        addError(state, "Expected ':' after class declaration");
        return node;
    }
    advance(state);

    addChild(node, parseSuite(state));

    return node;
}

ParseNode* parseTryStatement(ParserState& state) {
    ParseNode* node = createNode(NODE_TRY);

    advance(state); // consume 'try'

    if (!match(state, ":")) {
        addError(state, "Expected ':' after 'try'");
        return node;
    }
    advance(state);

    addChild(node, parseSuite(state));

    // Parse 'except' clauses
    while (match(state, "except")) {
        ParseNode* exceptNode = createNode(NODE_EXCEPT);
        advance(state); // consume 'except'

        if (!match(state, ":")) {
            addError(state, "Expected ':' after 'except'");
            return node;
        }
        advance(state);

        addChild(exceptNode, parseSuite(state));
        addChild(node, exceptNode);
    }

    return node;
}

// Modify parseCompoundStatement
ParseNode* parseCompoundStatement(ParserState& state) {
    ParseNode* node = nullptr;
    
    try {
        if (match(state, "if")) {
            node = parseIfStatement(state);
        }
        else if (match(state, "while")) {
            node = parseWhileStatement(state);
        }
        else if (match(state, "for")) {
            node = parseForStatement(state);
        }
        else if (match(state, "def")) {
            node = parseFunctionDef(state);
        }
        else if (match(state, "class")) {
            node = parseClassDef(state);
        }
        else if (match(state, "try")) {
            node = parseTryStatement(state);
        }
        else {
            addError(state, "Invalid compound statement");
            skipToNextStatement(state);
        }
    }
    catch (...) {
        skipToNextStatement(state);
    }

    return node;
}


ParseNode* parseSimpleStatement(ParserState& state) {
    if (state.currentToken >= state.tokens.size()) {
        addError(state, "Unexpected end of input");
        return nullptr;
    }

    if (match(state, "return")) {
        ParseNode* node = createNode(NODE_RETURN);
        advance(state);
        if (!match(state, "DEDENT") && !match(state, "\n")) {
            addChild(node, parseExpression(state));
        }
        return node;
    }

    else if (match(state, "import")) {
        ParseNode* node = createNode(NODE_IMPORT, "import");
        advance(state);
        if (getTokenType(state.tokens[state.currentToken].value) == "IDENTIFIER") {
            addChild(node, createNode(NODE_IDENTIFIER, state.tokens[state.currentToken].value));
            advance(state);
        }
        else {
            addError(state, "Expected identifier after 'import'");
        }
        return node;
    }

    else if (match(state, "pass")) {
        ParseNode* node = createNode(NODE_STATEMENT, "pass");
        advance(state);
        return node;
    }

    else if (getTokenType(state.tokens[state.currentToken].value) == "IDENTIFIER") {
        size_t nextToken = state.currentToken + 1;
        if (nextToken < state.tokens.size() && state.tokens[nextToken].value == "=") {
            return parseAssignment(state);
        }
        else if (nextToken < state.tokens.size() && state.tokens[nextToken].value == "(") {
            return parseFunctionCall(state);
        }
        else {
            return parseExpression(state);
        }
    }
    else {
        return parseExpression(state);
    }
}

// Modify parseStatement to handle errors better
ParseNode* parseStatement(ParserState& state) {
    if (state.currentToken >= state.tokens.size()) {
        addError(state, "Unexpected end of input");
        return nullptr;
    }

    try {
        if (match(state, "INDENT") || match(state, "DEDENT")) {
            advance(state);
            return nullptr;
        }

        if (match(state, "if") || match(state, "while") || match(state, "for") ||
            match(state, "def") || match(state, "class") || match(state, "try")) {
            return parseCompoundStatement(state);
        }
        else {
            ParseNode* node = parseSimpleStatement(state);
            if (!node) {
                skipToNextStatement(state);
            }
            return node;
        }
    }
    catch (...) {
        skipToNextStatement(state);
        return nullptr;
    }
}

ParseNode* parseProgram(ParserState& state) {
    ParseNode* node = createNode(NODE_PROGRAM);

    while (state.currentToken < state.tokens.size()) {
        if (match(state, "def")) {
            addChild(node, parseFunctionDef(state));
        }
        else {
            addChild(node, parseStatement(state));
        }
    }

    return node;
}

ParseNode* parseFunctionCall(ParserState& state) {
    ParseNode* node = createNode(NODE_FUNC_CALL);

    // Save function name
    addChild(node, createNode(NODE_IDENTIFIER, state.tokens[state.currentToken].value));
    advance(state);

    if (!match(state, "(")) {
        addError(state, "Expected '(' after function name");
        return node;
    }
    advance(state);

    // Parse arguments
    while (!match(state, ")")) {
        addChild(node, parseExpression(state));
        if (match(state, ",")) {
            advance(state);
        }
        else if (!match(state, ")")) {
            addError(state, "Expected ',' or ')'");
            return node;
        }
    }
    advance(state); // consume ')'

    return node;
}

ParseNode* parseSuite(ParserState& state) {
    ParseNode* node = createNode(NODE_SUITE);
    
    // Expect INDENT token
    if (!match(state, "INDENT")) {
        addError(state, "Expected indented block");
        return node;
    }
    advance(state);
    
    // Parse statements until we hit DEDENT
    while (!match(state, "DEDENT") && state.currentToken < state.tokens.size()) {
        ParseNode* stmt = parseStatement(state);
        if (stmt) {
            addChild(node, stmt);
        }
    }
    
    if (match(state, "DEDENT")) {
        advance(state);
    }
    
    return node;
}

string nodeTypeToString(NodeType type) {
    switch (type) {
    case NODE_PROGRAM: return "PROGRAM";
    case NODE_FUNC_DEF: return "FUNCTION_DEF";
    case NODE_STATEMENT: return "STATEMENT";
    case NODE_EXP: return "EXPRESSION";
    case NODE_TERM: return "TERM";
    case NODE_FACTOR: return "FACTOR";
    case NODE_IF: return "IF_STATEMENT";
    case NODE_WHILE: return "WHILE_STATEMENT";
    case NODE_FOR: return "FOR_STATEMENT";
    case NODE_ASSIGN: return "ASSIGNMENT";
    case NODE_IDENTIFIER: return "IDENTIFIER";
    case NODE_NUMBER: return "NUMBER";
    case NODE_STRING: return "STRING";
    case NODE_OPERATOR: return "OPERATOR";
    case NODE_COMPARISON: return "COMPARISON";
    case NODE_FUNC_CALL: return "FUNCTION_CALL";
    case NODE_RETURN: return "RETURN";
    case NODE_IMPORT: return "IMPORT";
    case NODE_CLASS: return "CLASS_DEF";
    case NODE_TRY: return "TRY_BLOCK";
    case NODE_EXCEPT: return "EXCEPT_BLOCK";
    case NODE_SUITE: return "SUITE";
    default: return "UNKNOWN";
    }
}

void printParseTree(ParseNode* node, string prefix = "", bool isLast = true) {
    if (!node) return;

    cout << prefix;
    cout << (isLast ? "└── " : "├── ");
    
    // Print the node information
    cout << nodeTypeToString(node->type);
    if (!node->value.empty()) {
        cout << " [" << node->value << "]";
    }
    cout << endl;

    // Prepare prefix for children
    string childPrefix = prefix + (isLast ? "    " : "│   ");

    // Print children
    for (size_t i = 0; i < node->children.size(); i++) {
        bool lastChild = (i == node->children.size() - 1);
        printParseTree(node->children[i], childPrefix, lastChild);
    }
}

void freeParseTree(ParseNode* node) {
    if (!node) return;

    for (ParseNode* child : node->children) {
        freeParseTree(child);
    }
    delete node;
}



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////MAIN////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main() {

    string inputFile;
    cout <<"LOML   " << "Enter path to .py file: ";
    cin >> inputFile;

    ifstream file(inputFile);
    if (!file) {
        cerr << "Error: Could not open file." << endl;
        return 1;
    }

    string line, input;
    while (getline(file, line)) {
        input += line + "\n";
    }

    file.close();

    vector<lexeme> tokens = tokenize(commentsremover(input));

    // Print lexical analysis results
    cout << "\n=== Lexical Analysis Results ===\n";
    cout << left << setw(20) << "Lexeme:" << setw(20) << "Token Type:" << "Tokens:" << endl;
    cout << string(50, '-') << endl;

    map<string, SymbolInfo> symbolTable = buildSymbolTable(tokens);

    for (const auto& lex : tokens) {
        string type = getTokenType(lex.value);
        string tokenRep;

        if (type == "IDENTIFIER") {
            int idIndex = symbolTable.count(lex.value) ? symbolTable[lex.value].index : 0;
            tokenRep = "< id , " + to_string(idIndex) + " >";
        }

        else if (type == "INTEGER") {
            tokenRep = "< number , integer value " + lex.value + " >";
        }

        else if (type == "FLOAT") {
            tokenRep = "< number , float value " + lex.value + " >";
        }

        else if (type == "STRING") {
            tokenRep = "< string , " + lex.value + " >";
        }

        else {
            tokenRep = "< " + lex.value + " >";
        }

        cout << left << setw(20) << ("[" + lex.value + "]")
            << setw(20) << type
            << tokenRep << endl;
    }

    printSymbolTable(symbolTable);
    printErrors();

    // syntax analysis
    cout << "\n=== Syntax Analysis Results ===\n";
    ParserState state = { tokens, 0, vector<string>() };
    ParseNode* parseTree = parseProgram(state);

    cout << "\nParse Tree:\n";
    printParseTree(parseTree);  // Using the single print function

    if (!state.errors.empty()) {
        cout << "\nSyntax Errors:\n";
        for (const auto& error : state.errors) {
            cout << error << endl;
        }
    }

    // Clean up the parse tree
    freeParseTree(parseTree);

    return 0;
}
