#ifndef TRANSLATOR_H
#define TRANSLATOR_H

#include <string>
#include <vector>

std::string convertExpression(std::string expr);
void replaceAll(std::string& str, const std::string& from, const std::string& to);
size_t findMatchingBrace(const std::string& str, size_t openBracePos);
size_t findMatchingBracket(const std::string& str, size_t openBracketPos) ;
size_t findMatchingParen(const std::string& str, size_t openParenPos);
void processLatexEquations(const std::string& inputFile, const std::string& outputFile);
extern int recursion_depth;

#endif // TRANSLATOR_H