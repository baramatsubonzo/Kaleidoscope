#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

// If the lexer reads a character that is not a number, not a keword, and not an identifier,
// it simply returns the ASCII value of that character (0-255).
// If the lexer reads something meaningful,
// it returns one of the special token values defined here.
enum Token {
  tok_eof = -1, // end of file

  tok_def = -2,
  tok_extern = -3,

  tok_identifier = -4, // variable names
  tok_number = -5, // numeric literals
};

static std::string IdentifierStr;
static double NumVal;

// Returns the next token from standard input.
static int gettok() {
  static int LastChar = ' ';
  // Skip any whitespace.
  while (isspace(LastChar))
    LastChar = getchar();

  if (isalpha(LastChar)) { // The initial character must be a letter(A-Z, a-z). e.g., "1def" is prohibited.a
    IdentifierStr = LastChar;
    while (isalnum((LastChar = getchar()))) // Subsequent characters can be letters or numbers.
      IdentifierStr += LastChar;

    if (IdentifierStr == "def")
      return tok_def;
    if (IdentifierStr == "extern")
      return tok_extern;
    return tok_identifier;
  }

  if (isdigit(LastChar) || LastChar == '.') { // e.g., `.14` is equivalent to `0.14`
    std::string NumStr;
    do {
      NumStr += LastChar;
      LastChar = getchar();
    } while (isdigit(LastChar) || LastChar == '.');

    NumVal = strtod(NumStr.c_str(), 0); // Convert string to double
    return tok_number;
  }

  if (LastChar == '#') {
    do
      LastChar = getchar();
    while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

    if (LastChar != EOF)
      return gettok();
  }

  if (LastChar == EOF)
    return tok_eof;

  int ThisChar = LastChar;
  LastChar = getchar();
  return ThisChar;
}
// Base class for all expression nodes.
class ExprAST {
  public:
  virtual ~ExprAST() = default; // virtual destructor for base class
};

class NumberExprAST : public ExprAST {
  double Val;
public:
  NumberExprAST(double Val) : Val(Val) {} // constructor
};

class VariableExprAST : public ExprAST {
  std::string Name;
public:
  VariableExprAST(const std::string &Name) : Name(Name) {}
};

class BinaryExprAST : public ExprAST {
  char Op; // operator
  std::unique_ptr<ExprAST> LHS, RHS; // smart pointers can only have one owner
  public:
  BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS)
    : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {} // std::move allows transfer of ownership
};

class CallExprAST : public ExprAST {
  std::string Callee;
  std::vector<std::unique_ptr<ExprAST>> Args;
public:
  CallExprAST(const std::string &Callee, std::vector<std::unique_ptr<ExprAST>> Args)
    : Callee(Callee), Args(std::move(Args)) {}
};

class PrototypeAST {
  std::string Name;
  std::vector<std::string> Args;
public:
  PrototypeAST(const std::string &Name, std::vector<std::string> Args)
    : Name(Name), Args(std::move(Args)) {}
  const std::string &getName() const { return Name; }
};

class FunctionAST {
  std::unique_ptr<PrototypeAST> Proto;
  std::unique_ptr<ExprAST> Body;
public:
  FunctionAST(std::unique_ptr<PrototypeAST> Proto, std::unique_ptr<ExprAST> Body)
    : Proto(std::move(Proto)), Body(std::move(Body)) {}
};

// Provide a simple token buffer.
// CurTok is the current token the parser is looking at.
// getNextToken reads another token from the lexer and updates CurTok with its results.
static int CurTok;
static int getNextToken() {
  return CurTok = gettok();
}

std::unique_ptr<ExprAST> LogError(const char *Str) {
  fprintf(stderr, "Error: %s\n", Str);
  return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
  LogError(Str);
  return nullptr;
}

int main() {
  return 0;
}