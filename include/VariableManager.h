#pragma once
#include <string>
#include <vector>
#include <set>


std::string uniformizeVariable(const std::string& equation, const std::string& property);
std::set<std::string> detectVariables(const std::string& equation, const std::string& property);
std::vector<std::string> makeInitializations(const std::set<std::string>& variables);
std::vector<std::string> getInputVariables(const std::string& equation);
std::set<std::string> getAllSymbols(const std::string& equation);

void detectVariablesAndCoefficients(
    const std::string& equation,
    std::vector<std::string>& variables,
    std::vector<std::string>& coefficients
);