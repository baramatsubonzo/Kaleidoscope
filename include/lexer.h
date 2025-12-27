#ifndef LEXER_H
#define LEXER_H

#include <string>
#include "token.h"

extern std::string IdentifierStr;
extern double NumVal;

int gettok();

#endif
