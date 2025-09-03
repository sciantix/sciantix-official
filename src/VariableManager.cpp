#include "VariableManager.h"
#include <regex>
#include <algorithm>
#include <set>
#include <iostream>

std::string uniformizeVariable(const std::string& equation, const std::string& property) {
    size_t eq_pos = equation.find('=');
    if (eq_pos != std::string::npos) {
        return property + " =" + equation.substr(eq_pos + 1);
    }
    return equation;
}



std::vector<std::string> getInputVariables(const std::string& equation) {
    std::smatch match;
    std::regex param_re(R"((?:[a-zA-Z_][a-zA-Z0-9_]*)\s*[\(\[]([^\]\)]*)[\)\]])"); 
    
    if (std::regex_search(equation, match, param_re) && match.size() >= 2) {
        std::vector<std::string> vars;
        std::string params = match[1]; 
        std::regex var_re(R"([a-zA-Z_][a-zA-Z0-9_]*)"); 
        auto words_begin = std::sregex_iterator(params.begin(), params.end(), var_re);
        auto words_end = std::sregex_iterator();
        for (auto it = words_begin; it != words_end; ++it)
            vars.push_back(it->str());
        return vars;
    }
    return {};
}


std::set<std::string> getAllSymbols(const std::string& equation) {
    std::set<std::string> result;
    size_t eq_pos = equation.find('=');
    if (eq_pos == std::string::npos) return result;
    std::string expr = equation.substr(eq_pos + 1);
    static const std::set<std::string> keywords = { "pow", "sqrt", "exp", "log", "sin", "cos", "tan", "abs", "return" };
    std::regex var_re(R"([a-zA-Z_][a-zA-Z0-9_]*)");
    auto words_begin = std::sregex_iterator(expr.begin(), expr.end(), var_re);
    auto words_end = std::sregex_iterator();
    for (auto it = words_begin; it != words_end; ++it) {
        std::string var = it->str();
        if (!keywords.count(var))
            result.insert(var);
    }
    return result;
}


std::set<std::string> detectVariables(const std::string& equation, const std::string& property) {

    std::set<std::string> result = getAllSymbols(equation);
    result.erase(property);
    return result;
}


std::vector<std::string> makeInitializations(const std::set<std::string>& variables) {
    std::vector<std::string> lines;
    for (const auto& v : variables) {
        lines.push_back("double " + v + " = 0.0;");
    }
    return lines;
}


void detectVariablesAndCoefficients(const std::string& equation, std::vector<std::string>& variables, std::vector<std::string>& coefficients) {

    variables = getInputVariables(equation);

    std::set<std::string> all_syms = getAllSymbols(equation);

    for (const auto& var : variables)
        all_syms.erase(var);
    coefficients.assign(all_syms.begin(), all_syms.end());
}