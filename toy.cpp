#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <string>

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
  while (isspace(Lastchar))
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