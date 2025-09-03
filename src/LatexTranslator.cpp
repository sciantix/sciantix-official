#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <vector>
#include <stack> // For brace and parenthesis management
#include <cmath> // For sin, cos, sqrt, pow, exp, log, log10
#include <map>   // For std::map
#include "LatexTranslator.h"

int recursion_depth = 0;

bool isInteger(const std::string& s) {
    if (s.empty()) return false;
    for (size_t i = (s[0] == '-' ? 1 : 0); i < s.length(); ++i) { 
        if (!isdigit(s[i])) {
            return false;
        }
    }
    return true;
}

// Utility function to find the end of a block delimited by curly braces {}
size_t findMatchingBrace(const std::string& str, size_t openBracePos) {
    if (openBracePos >= str.length() || str[openBracePos] != '{') {
        return std::string::npos;
    }
    int braceCount = 1;
    for (size_t i = openBracePos + 1; i < str.length(); ++i) {
        if (str[i] == '{') {
            braceCount++;
        } else if (str[i] == '}') {
            braceCount--;
        }
        if (braceCount == 0) {
            return i;
        }
    }
    return std::string::npos; // Matching closing brace not found
}

// Utility function to find the end of a block delimited by square brackets []
size_t findMatchingBracket(const std::string& str, size_t openBracketPos) {
    if (openBracketPos >= str.length() || str[openBracketPos] != '[') {
        return std::string::npos;
    }
    int bracketCount = 1;
    for (size_t i = openBracketPos + 1; i < str.length(); ++i) {
        if (str[i] == '[') {
            bracketCount++;
        } else if (str[i] == ']') {
            bracketCount--;
        }
        if (bracketCount == 0) {
            return i;
        }
    }
    return std::string::npos; // Matching closing bracket not found
}

// Utility function to find the end of a block delimited by parentheses ()
size_t findMatchingParen(const std::string& str, size_t openParenPos) {
    if (openParenPos >= str.length() || str[openParenPos] != '(') {
        return std::string::npos;
    }
    int parenCount = 1;
    for (size_t i = openParenPos + 1; i < str.length(); ++i) {
        if (str[i] == '(') {
            parenCount++;
        } else if (str[i] == ')') {
            parenCount--;
        }
        if (parenCount == 0) {
            return i;
        }
    }
    return std::string::npos; // Matching closing parenthesis not found
}

// Recursive function to convert a LaTeX sub-expression to C++
std::string convertExpression(std::string expr) {
    std::string original_expr = expr;
    const int MAX_RECURSION_DEPTH = 500;
    recursion_depth++;
    if (recursion_depth > MAX_RECURSION_DEPTH) {
        recursion_depth--;
        throw std::runtime_error("Maximum recursion depth exceeded");
    }

    int debug_loop_counter = 0;
    const int MAX_DEBUG_LOOP_ITERATIONS = 100; // Max iterations per pass

    bool changed_in_this_pass = true;

    while (changed_in_this_pass) {
        changed_in_this_pass = false; // Reset for this pass

        if (debug_loop_counter > MAX_DEBUG_LOOP_ITERATIONS) {
             std::cerr << "WARNING: Conversion loop too long for expression: " << original_expr << std::endl;
             recursion_depth--; 
             return expr; // Return expression as is
        }
        debug_loop_counter++;

        // --- LaTeX symbol replacements with valid C++ names and cleanup ---
        if (expr.find("\\theta") != std::string::npos) { replaceAll(expr, "\\theta", "theta"); changed_in_this_pass = true; }
        if (expr.find("\\alpha") != std::string::npos) { replaceAll(expr, "\\alpha", "alpha"); changed_in_this_pass = true; }
        if (expr.find("\\beta") != std::string::npos) { replaceAll(expr, "\\beta", "beta"); changed_in_this_pass = true; }
        if (expr.find("\\gamma") != std::string::npos) { replaceAll(expr, "\\gamma", "gamma"); changed_in_this_pass = true; } 
        if (expr.find("\\delta") != std::string::npos) { replaceAll(expr, "\\delta", "delta"); changed_in_this_pass = true; } 
        if (expr.find("\\epsilon") != std::string::npos) { replaceAll(expr, "\\epsilon", "epsilon"); changed_in_this_pass = true; } 
        if (expr.find("\\phi") != std::string::npos) { replaceAll(expr, "\\phi", "phi"); changed_in_this_pass = true; } 
        if (expr.find("\\psi") != std::string::npos) { replaceAll(expr, "\\psi", "psi"); changed_in_this_pass = true; } 
        if (expr.find("\\chi") != std::string::npos) { replaceAll(expr, "\\chi", "chi"); changed_in_this_pass = true; } 

        if (expr.find("\\cdot") != std::string::npos) { replaceAll(expr, "\\cdot", "*"); changed_in_this_pass = true; }
        if (expr.find("\\left(") != std::string::npos) { replaceAll(expr, "\\left(", "("); changed_in_this_pass = true; }
        if (expr.find("\\right)") != std::string::npos) { replaceAll(expr, "\\right)", ")"); changed_in_this_pass = true; }
        if (expr.find("\\pow") != std::string::npos) { replaceAll(expr, "\\pow", "pow"); changed_in_this_pass = true; }

        // --- Trigonométriques ---
        size_t pos;
        if ((pos = expr.find("\\sin{")) != std::string::npos) {
            size_t openBrace = pos + 4;
            size_t closeBrace = findMatchingBrace(expr, openBrace);
            if (closeBrace != std::string::npos) {
                std::string arg = convertExpression(expr.substr(openBrace + 1, closeBrace - (openBrace + 1)));
                expr.replace(pos, closeBrace - pos + 1, "sin(" + arg + ")");
                changed_in_this_pass = true;
                continue;
            }
        }
        if ((pos = expr.find("\\sin(")) != std::string::npos) { expr.replace(pos, 4, "sin"); changed_in_this_pass = true; }
        else if ((pos = expr.find("\\sin")) != std::string::npos && (pos + 4 >= expr.length() || !isalnum(expr[pos + 4]))) { expr.replace(pos, 4, "sin"); changed_in_this_pass = true; }

        if ((pos = expr.find("\\cos{")) != std::string::npos) {
            size_t openBrace = pos + 4;
            size_t closeBrace = findMatchingBrace(expr, openBrace);
            if (closeBrace != std::string::npos) {
                std::string arg = convertExpression(expr.substr(openBrace + 1, closeBrace - (openBrace + 1)));
                expr.replace(pos, closeBrace - pos + 1, "cos(" + arg + ")");
                changed_in_this_pass = true;
                continue;
            }
        }
        if ((pos = expr.find("\\cos(")) != std::string::npos) { expr.replace(pos, 4, "cos"); changed_in_this_pass = true; }
        else if ((pos = expr.find("\\cos")) != std::string::npos && (pos + 4 >= expr.length() || !isalnum(expr[pos + 4]))) { expr.replace(pos, 4, "cos"); changed_in_this_pass = true; }

        if ((pos = expr.find("\\tan{")) != std::string::npos) {
            size_t openBrace = pos + 4;
            size_t closeBrace = findMatchingBrace(expr, openBrace);
            if (closeBrace != std::string::npos) {
                std::string arg = convertExpression(expr.substr(openBrace + 1, closeBrace - (openBrace + 1)));
                expr.replace(pos, closeBrace - pos + 1, "tan(" + arg + ")");
                changed_in_this_pass = true;
                continue;
            }
        }
        if ((pos = expr.find("\\tan(")) != std::string::npos) { expr.replace(pos, 4, "tan"); changed_in_this_pass = true; }
        else if ((pos = expr.find("\\tan")) != std::string::npos && (pos + 4 >= expr.length() || !isalnum(expr[pos + 4]))) { expr.replace(pos, 4, "tan"); changed_in_this_pass = true; }

        // --- LOGS & LN ---
        if (expr.length() >= 3 && expr.substr(0, 3) == "\\ln") {
            size_t pos = 3;
            std::string power_str;
            bool has_power = false;

            if (pos < expr.length() && expr[pos] == '^') {
                pos++;
                if (expr[pos] == '{') {
                    size_t close = findMatchingBrace(expr, pos);
                    if (close == std::string::npos) { recursion_depth--; return "ERROR_LN_POWER_BRACE"; }
                    power_str = convertExpression(expr.substr(pos + 1, close - pos - 1));
                    pos = close + 1;
                } else {
                    power_str = expr.substr(pos, 1);
                    pos++;
                }
                has_power = true;
            }

            std::string arg_str;
            if (pos < expr.length() && expr[pos] == '{') {
                size_t close = findMatchingBrace(expr, pos);
                if (close == std::string::npos) { recursion_depth--; return "ERROR_LN_ARG_BRACE"; }
                arg_str = convertExpression(expr.substr(pos + 1, close - pos - 1));
                pos = close + 1;
            } else if (pos < expr.length() && expr[pos] == '(') {
                size_t close = findMatchingParen(expr, pos);
                if (close == std::string::npos) { recursion_depth--; return "ERROR_LN_ARG_PAREN"; }
                arg_str = convertExpression(expr.substr(pos + 1, close - pos - 1));
                pos = close + 1;
            } else {
                size_t end = pos;
                while (end < expr.length() && (isalnum(expr[end]) || expr[end] == '_')) end++;
                arg_str = expr.substr(pos, end - pos);
                pos = end;
            }

            std::string ln_result = "log(" + arg_str + ")";
            if (has_power) {
                ln_result = "pow(" + ln_result + "," + power_str + ")";
            }
            std::string rest = expr.substr(pos);
            recursion_depth--;
            return ln_result + convertExpression(rest);
        }

        if (expr.length() >= 4 && expr.substr(0, 4) == "\\log") {
            size_t pos = 4;
            std::string func_digits;
            while (pos < expr.length() && isdigit(expr[pos])) {
                func_digits += expr[pos];
                pos++;
            }
            std::string base_str = func_digits.empty() ? "10" : func_digits;
            std::string power_str;
            bool has_power = false;
            bool has_base = !func_digits.empty();
            for (int i = 0; i < 2; ++i) {
                if (pos < expr.length() && (expr[pos] == '^' || expr[pos] == '_')) {
                    char type = expr[pos];
                    pos++;
                    if (type == '^') {
                        has_power = true;
                        if (pos < expr.length() && expr[pos] == '{') {
                            size_t close = findMatchingBrace(expr, pos);
                            if (close == std::string::npos) { recursion_depth--; return "ERROR_LOG_POWER_BRACE"; }
                            power_str = convertExpression(expr.substr(pos + 1, close - pos - 1));
                            pos = close + 1;
                        } else {
                            power_str = expr.substr(pos, 1);
                            pos++;
                        }
                    } else if (type == '_') {
                        has_base = true;
                        if (pos < expr.length() && expr[pos] == '{') {
                            size_t close = findMatchingBrace(expr, pos);
                            if (close == std::string::npos) { recursion_depth--; return "ERROR_LOG_BASE_BRACE"; }
                            base_str = convertExpression(expr.substr(pos + 1, close - pos - 1));
                            pos = close + 1;
                        } else {
                            base_str = expr.substr(pos, 1);
                            pos++;
                        }
                    }
                }
            }
            std::string arg_str;
            if (pos < expr.length() && expr[pos] == '{') {
                size_t close = findMatchingBrace(expr, pos);
                if (close == std::string::npos) { recursion_depth--; return "ERROR_LOG_ARG_BRACE"; }
                arg_str = convertExpression(expr.substr(pos + 1, close - pos - 1));
                pos = close + 1;
            } else if (pos < expr.length() && expr[pos] == '(') {
                size_t close = findMatchingParen(expr, pos);
                if (close == std::string::npos) { recursion_depth--; return "ERROR_LOG_ARG_PAREN"; }
                arg_str = convertExpression(expr.substr(pos + 1, close - pos - 1));
                pos = close + 1;
            } else {
                size_t end = pos;
                while (end < expr.length() && (isalnum(expr[end]) || expr[end] == '_')) end++;
                arg_str = expr.substr(pos, end - pos);
                pos = end;
            }
            std::string log_result;
            if (base_str == "e") {
                log_result = "log(" + arg_str + ")";
            } else {
                log_result = "(log(" + arg_str + ")/log(" + base_str + "))";
            }
            if (has_power) {
                log_result = "pow(" + log_result + "," + power_str + ")";
            }
            std::string rest = expr.substr(pos);
            recursion_depth--;
            return log_result + convertExpression(rest);
        }

        
        size_t exp_brace_pos = expr.find("\\exp{");
        if (exp_brace_pos != std::string::npos) {
            size_t openBrace = exp_brace_pos + 4;
            size_t closeBrace = findMatchingBrace(expr, openBrace);
            if (closeBrace == std::string::npos) { recursion_depth--; return "ERROR_EXP_BRACE"; }
            std::string arg = convertExpression(expr.substr(openBrace + 1, closeBrace - (openBrace + 1)));
            expr.replace(exp_brace_pos, closeBrace - exp_brace_pos + 1, "exp(" + arg + ")");
            changed_in_this_pass = true;
            continue;
        }
        if ((pos = expr.find("\\exp(")) != std::string::npos) { expr.replace(pos, 4, "exp"); changed_in_this_pass = true; }

        
        for (size_t i = 0; i + 2 < expr.length(); ++i) {
            if (isalpha(expr[i]) && expr[i+1] == '_' && expr[i+2] == '{') {
                size_t open_brace = i + 2;
                size_t close_brace = findMatchingBrace(expr, open_brace);
                if (close_brace != std::string::npos) {
                    std::string inner_content = expr.substr(open_brace + 1, close_brace - (open_brace + 1));
                    if (inner_content.find_first_of("+-*/^(){}[].") == std::string::npos &&
                        inner_content.find("\\") == std::string::npos &&
                        !inner_content.empty())
                    {
                        std::string var_name = expr.substr(i, 1);
                        std::string converted_part = var_name + "_" + inner_content;
                        expr.replace(i, close_brace - i + 1, converted_part);
                        changed_in_this_pass = true;
                        break;
                    }
                }
            }
        }

        
        size_t sqrt_n_pos = expr.find("\\sqrt[");
        if (sqrt_n_pos != std::string::npos) {
            size_t openBracket = sqrt_n_pos + 5;
            size_t closeBracket = findMatchingBracket(expr, openBracket);
            if (closeBracket == std::string::npos) { recursion_depth--; return "ERROR_SQRT_N_BRACKET_MISSING"; }
            std::string n_str = convertExpression(expr.substr(openBracket + 1, closeBracket - openBracket - 1));
            size_t openBrace = closeBracket + 1;
            if (openBrace >= expr.length() || expr[openBrace] != '{') { recursion_depth--; return "ERROR_SQRT_N_BRACE_MISSING"; }
            size_t closeBrace = findMatchingBrace(expr, openBrace);
            if (closeBrace == std::string::npos) { recursion_depth--; return "ERROR_SQRT_N_CLOSE_BRACE_MISSING"; }
            std::string arg = convertExpression(expr.substr(openBrace + 1, closeBrace - openBrace - 1));
            std::string converted_sqrt_n = "pow(" + arg + ",1.0/" + n_str + ")";
            expr.replace(sqrt_n_pos, closeBrace - sqrt_n_pos + 1, converted_sqrt_n);
            changed_in_this_pass = true;
            continue;
        }

        
        size_t sqrt_pos = expr.find("\\sqrt{");
        if (sqrt_pos != std::string::npos) {
            size_t openBrace = sqrt_pos + 5;
            size_t closeBrace = findMatchingBrace(expr, openBrace);
            if (openBrace == std::string::npos || closeBrace == std::string::npos) { recursion_depth--; return "ERROR_SQRT_BRACE_MISSING"; }
            std::string arg = convertExpression(expr.substr(openBrace + 1, closeBrace - openBrace - 1));
            expr.replace(sqrt_pos, closeBrace - sqrt_pos + 1, "sqrt(" + arg + ")");
            changed_in_this_pass = true;
            continue;
        }

        
        size_t e_power_pos = expr.find("e^");
        if (e_power_pos != std::string::npos) {
            size_t exponent_start = e_power_pos + 2;
            if (exponent_start < expr.length()) {
                if (expr[exponent_start] == '{') {
                    size_t openBrace = exponent_start;
                    size_t closeBrace = findMatchingBrace(expr, openBrace);
                    if (closeBrace == std::string::npos) { recursion_depth--; return "ERROR_E_POWER_BRACE"; }
                    std::string arg = convertExpression(expr.substr(openBrace + 1, closeBrace - openBrace - 1));
                    expr.replace(e_power_pos, closeBrace - e_power_pos + 1, "exp(" + arg + ")");
                    changed_in_this_pass = true;
                    continue;
                } else if (isalnum(expr[exponent_start])) {
                    size_t i = exponent_start;
                    while (i < expr.length() && (isalnum(expr[i]) || expr[i] == '+' || expr[i] == '-' || expr[i] == '*' || expr[i] == '/')) {
                        i++;
                    }
                    if (expr.substr(exponent_start, i - exponent_start) == "exp") {
                        
                    } else {
                        std::string arg = expr.substr(exponent_start, i - exponent_start);
                        arg = convertExpression(arg);
                        expr.replace(e_power_pos, i - e_power_pos, "exp(" + arg + ")");
                        changed_in_this_pass = true;
                        continue;
                    }
                }
            }
        }

        
        size_t frac_pos = expr.find("\\frac{");
        if (frac_pos != std::string::npos) {
            size_t num_open = frac_pos + 5;
            size_t num_close = findMatchingBrace(expr, num_open);
            if (num_close == std::string::npos) { recursion_depth--; return "ERROR_FRAC_NUM"; }
            size_t den_open = num_close + 1;
            if (den_open >= expr.length() || expr[den_open] != '{') { recursion_depth--; return "ERROR_FRAC_DEN_MISSING_BRACE"; }
            size_t den_close = findMatchingBrace(expr, den_open);
            if (den_close == std::string::npos) { recursion_depth--; return "ERROR_FRAC_DEN"; }
            std::string numerator = convertExpression(expr.substr(num_open + 1, num_close - (num_open + 1)));
            std::string denominator = convertExpression(expr.substr(den_open + 1, den_close - (den_open + 1)));
            if (isInteger(numerator) && numerator.find('.') == std::string::npos) { numerator += ".0"; }
            if (isInteger(denominator) && denominator.find('.') == std::string::npos) { denominator += ".0"; }
            auto requiresParentheses = [](const std::string& s) {
                if (s.empty()) return false;
                if (s.front() == '(' && s.back() == ')') {
                    size_t matchingParen = findMatchingParen(s, 0);
                    if (matchingParen == s.length() -1) return false;
                }
                return s.find('+') != std::string::npos || s.find('-') != std::string::npos ||
                       s.find('*') != std::string::npos || s.find('/') != std::string::npos;
            };
            if (requiresParentheses(numerator)) { numerator = "(" + numerator + ")"; }
            if (requiresParentheses(denominator)) { denominator = "(" + denominator + ")"; }
            std::string converted_frac = "(" + numerator + "/" + denominator + ")";
            expr.replace(frac_pos, den_close - frac_pos + 1, converted_frac);
            changed_in_this_pass = true;
            continue;
        }

        
        std::vector<std::string> functions_with_power = {"sin", "cos", "tan", "log", "exp", "log10"};
        bool applied_func_power = false;
        for (const auto& func_name : functions_with_power) {
            size_t func_pos = expr.find(func_name + "^");
            if (func_pos != std::string::npos) {
                size_t power_caret_pos = func_pos + func_name.length();
                std::string exponent_str;
                size_t exponent_start_pos = power_caret_pos + 1;
                size_t exponent_full_end_pos;
                if (exponent_start_pos < expr.length() && expr[exponent_start_pos] == '{') {
                    size_t exp_open = exponent_start_pos;
                    size_t exp_close = findMatchingBrace(expr, exp_open);
                    if (exp_close == std::string::npos) { recursion_depth--; return "ERROR_FUNC_POWER_BRACE"; }
                    exponent_str = convertExpression(expr.substr(exp_open + 1, exp_close - (exp_open + 1)));
                    exponent_full_end_pos = exp_close;
                } else if (exponent_start_pos < expr.length() && isalnum(expr[exponent_start_pos])) {
                    size_t i = exponent_start_pos;
                    while (i < expr.length() && isalnum(expr[i])) { i++; }
                    exponent_str = expr.substr(exponent_start_pos, i - exponent_start_pos);
                    exponent_full_end_pos = i - 1;
                } else { continue; }
                size_t arg_open_pos = exponent_full_end_pos + 1;
                if (arg_open_pos >= expr.length() || expr[arg_open_pos] != '(') { continue; }
                size_t arg_close_pos = findMatchingParen(expr, arg_open_pos);
                if (arg_close_pos == std::string::npos) { recursion_depth--; return "ERROR_FUNC_POWER_ARG_PAREN"; }
                std::string func_arg_content = convertExpression(expr.substr(arg_open_pos + 1, arg_close_pos - arg_open_pos - 1));
                std::string base_func_call = func_name + "(" + func_arg_content + ")";
                std::string converted_power_func = "pow(" + base_func_call + "," + exponent_str + ")";
                expr.replace(func_pos, arg_close_pos - func_pos + 1, converted_power_func);
                changed_in_this_pass = true;
                applied_func_power = true;
                break;
            }
        }
        if (applied_func_power) { continue; }

        
        size_t power_pos = expr.find("^");
        if (power_pos != std::string::npos) {
            std::string base_str;
            size_t base_start_idx;
            size_t base_end_idx = power_pos - 1;
            if (base_end_idx >= expr.length()) { recursion_depth--; return "ERROR_POWER_BASE_PARSE"; }
            if (expr[base_end_idx] == '}') {
                size_t openBrace = base_end_idx;
                int braceCount = 0;
                while(openBrace > 0) {
                    if (expr[openBrace] == '}') braceCount++;
                    else if (expr[openBrace] == '{') braceCount--;
                    if (braceCount == 0) break;
                    openBrace--;
                }
                if (openBrace == std::string::npos || expr[openBrace] != '{') {
                    size_t current_base_char_pos = base_end_idx;
                    while (current_base_char_pos > 0 && (isalnum(expr[current_base_char_pos - 1]) || expr[current_base_char_pos - 1] == '_')) {
                        current_base_char_pos--;
                    }
                    base_str = expr.substr(current_base_char_pos, power_pos - current_base_char_pos);
                    base_start_idx = current_base_char_pos;
                } else {
                    base_str = convertExpression(expr.substr(openBrace + 1, base_end_idx - (openBrace + 1)));
                    base_str = "(" + base_str + ")";
                    base_start_idx = openBrace;
                }
            } else if (expr[base_end_idx] == ')') {
                size_t openParen = base_end_idx;
                int parenCount = 0;
                while(openParen > 0) {
                    if (expr[openParen] == ')') parenCount++;
                    else if (expr[openParen] == '(') parenCount--;
                    if (parenCount == 0) break;
                    openParen--;
                }
                if (openParen == std::string::npos || expr[openParen] != '(') { recursion_depth--; return "ERROR_POWER_PAREN_BASE_NO_MATCH"; }
                base_str = expr.substr(openParen, base_end_idx - openParen + 1);
                base_start_idx = openParen;
                base_str = convertExpression(base_str);
            } else {
                size_t current_base_char_pos = base_end_idx;
                while (current_base_char_pos > 0 && (isalnum(expr[current_base_char_pos - 1]) || expr[current_base_char_pos - 1] == '_')) {
                    current_base_char_pos--;
                }
                base_str = expr.substr(current_base_char_pos, power_pos - current_base_char_pos);
                base_start_idx = current_base_char_pos;
            }
            std::string exponent_str;
            size_t exponent_start_pos = power_pos + 1;
            size_t exponent_full_end_pos;
            if (exponent_start_pos < expr.length()) {
                if (expr[exponent_start_pos] == '{') {
                    size_t exp_open = exponent_start_pos;
                    size_t exp_close = findMatchingBrace(expr, exp_open);
                    if (exp_close == std::string::npos) { recursion_depth--; return "ERROR_POWER_EXP_BRACE"; }
                    exponent_str = convertExpression(expr.substr(exp_open + 1, exp_close - (exp_open + 1)));
                    exponent_full_end_pos = exp_close;
                } else if (expr[exponent_start_pos] == '(') {
                    size_t exp_open = exponent_start_pos;
                    size_t exp_close = findMatchingParen(expr, exp_open);
                    if (exp_close == std::string::npos) { recursion_depth--; return "ERROR_POWER_EXP_PAREN"; }
                    exponent_str = convertExpression(expr.substr(exp_open + 1, exp_close - (exp_open + 1)));
                    exponent_full_end_pos = exp_close;
                } else {
                    size_t i = exponent_start_pos;
                    while (i < expr.length() && (isalnum(expr[i]))) { i++; }
                    exponent_str = expr.substr(exponent_start_pos, i - exponent_start_pos);
                    exponent_full_end_pos = i - 1;
                }
                std::string converted_power = "pow(" + base_str + "," + exponent_str + ")";
                expr.replace(base_start_idx, exponent_full_end_pos - base_start_idx + 1, converted_power);
                changed_in_this_pass = true;
                continue;
            }
        }

        
        size_t underscore_pos = expr.find("_{");
        if (underscore_pos != std::string::npos && underscore_pos > 0) {
            size_t base_start_idx = underscore_pos - 1;
            if (expr[base_start_idx] == ')') {
                size_t openParen = base_start_idx;
                int parenCount = 0;
                while(openParen > 0) {
                    if (expr[openParen] == ')') parenCount++;
                    else if (expr[openParen] == '(') parenCount--;
                    if (parenCount == 0) break;
                    openParen--;
                }
                if (openParen == std::string::npos || expr[openParen] != '(') { recursion_depth--; return "ERROR_INDEX_PAREN_BASE_NO_MATCH"; }
                base_start_idx = openParen;
            } else {
                while (base_start_idx > 0 && (isalnum(expr[base_start_idx - 1]) || expr[base_start_idx - 1] == '_')) {
                    base_start_idx--;
                }
            }
            std::string index_str;
            size_t index_start_pos = underscore_pos + 1;
            size_t index_end_pos;
            if (index_start_pos < expr.length() && expr[index_start_pos] == '{') {
                size_t idx_open = index_start_pos;
                size_t idx_close = findMatchingBrace(expr, idx_open);
                if (idx_close == std::string::npos) { recursion_depth--; return "ERROR_INDEX_BRACE"; }
                index_str = convertExpression(expr.substr(idx_open + 1, idx_close - (idx_open + 1)));
                index_end_pos = idx_close;
                std::string actual_base_part = expr.substr(base_start_idx, underscore_pos - base_start_idx);
                std::string indexed_var = actual_base_part + "_" + index_str;
                expr.replace(base_start_idx, index_end_pos - base_start_idx + 1, indexed_var);
                changed_in_this_pass = true;
                continue;
            }
        }

        
        auto trim = [](std::string s) {
            s.erase(0, s.find_first_not_of(" \t\n\r\f\v"));
            s.erase(s.find_last_not_of(" \t\n\r\f\v") + 1);
            return s;
        };
        std::string trimmed_expr = trim(expr);
        if (trimmed_expr != expr) {
            expr = trimmed_expr;
            changed_in_this_pass = true;
        }
    }

    // --- Final whitespace cleanup ---
    replaceAll(expr, " + ", "+");
    replaceAll(expr, " - ", "-");
    replaceAll(expr, " * ", "*");
    replaceAll(expr, " / ", "/");
    replaceAll(expr, " = ", "=");
    replaceAll(expr, "+ ", "+");
    replaceAll(expr, " +", "+");
    replaceAll(expr, "- ", "-");
    replaceAll(expr, " -", "-");
    replaceAll(expr, "* ", "*");
    replaceAll(expr, " *", "*");
    replaceAll(expr, "/ ", "/");
    replaceAll(expr, " /", "/");
    replaceAll(expr, "= ", "=");
    replaceAll(expr, " =", "=");
    replaceAll(expr, " ,", ",");
    replaceAll(expr, " )", ")");
    replaceAll(expr, "( ", "(");
    auto trim_final = [](std::string s) {
        s.erase(0, s.find_first_not_of(" \t\n\r\f\v"));
        s.erase(s.find_last_not_of(" \t\n\r\f\v") + 1);
        return s;
    };
    expr = trim_final(expr);

    recursion_depth--;
    return expr;
}

// Function to replace all occurrences of a substring
void replaceAll(std::string& str, const std::string& from, const std::string& to) {
    if (from.empty())
        return;
    size_t start_pos = 0;
    while ((start_pos = str.find(from, start_pos)) != std::string::npos) {
        str.replace(start_pos, from.length(), to);
        start_pos += to.length();
    }
}

void    processLatexEquations(const std::string& inputFile, const std::string& outputFile) {
    std::ifstream inFile(inputFile);
    std::ofstream outFile(outputFile);

    if (!inFile.is_open()) {
        std::cerr << "Error: Unable to open input file " << inputFile << std::endl;
        return;
    }
    if (!outFile.is_open()) {
        std::cerr << "Error: Unable to open output file " << outputFile << std::endl;
        return;
    }

    std::string line;
    while (std::getline(inFile, line)) {
        if (line.empty() || line.find_first_not_of(" \t\n\r\f\v") == std::string::npos) {
            continue;
        }

        std::string cppEquivalent = convertExpression(line);
        outFile << cppEquivalent << ";" << std::endl;

    }

    inFile.close();
    outFile.close();
    //std::cout << "Conversion completed. C++ equations have been written to " << outputFile << std::endl;
}

