#include "CorrelationSelector.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <string>
#include <map>
#include <vector>


static std::string trim(const std::string& s) {
    size_t start = s.find_first_not_of(" \t\r\n#");
    size_t end = s.find_last_not_of(" \t\r\n#");
    if (start == std::string::npos || end == std::string::npos)
        return "";
    return s.substr(start, end - start + 1);
}

std::map<std::string, int> parseSettingsFile(const std::string& filename) {
    std::map<std::string, int> settings;
    std::ifstream infile(filename);
    if (!infile) {
        std::cerr << "Unable to open " << filename << std::endl;
        exit(1);
    }
    std::string line;
    while (std::getline(infile, line)) {
        std::istringstream iss(line);
        int idx;
        std::string hash, property;
        if (iss >> idx >> hash && hash == "#") {
            std::getline(iss, property);
            property = trim(property);
            if (!property.empty()) settings[property] = idx;
        }
    }
    return settings;
}

void writeSelectedCorrelations(
    const std::string& allCorrsFile,
    const std::map<std::string, int>& settings,
    const std::string& outputSelectionFile
) {
    auto allCorrs = loadAllCorrelations(allCorrsFile);



    std::ofstream selection_out(outputSelectionFile, std::ios::trunc);
    if (!selection_out) {
        std::cerr << "Error: Unable to create selection file: " << outputSelectionFile << std::endl;
        return;
    }

    for (const auto& [property, idx] : settings) {
        auto it = allCorrs.find(property);
        if (it != allCorrs.end() && idx >= 0 && idx < (int)it->second.size()) {
            const auto& corr = it->second[idx];

            selection_out << "Property: " << corr.property << "\n";
            selection_out << "Module: " << corr.moduleName << "\n";
            selection_out << "Title: " << corr.title << "\n";
            selection_out << "Variables: ";
            for (size_t i = 0; i < corr.variables.size(); ++i) {
                if (i > 0) selection_out << ",";
                selection_out << corr.variables[i];
            }
            selection_out << "\n";
            selection_out << "Coefficients: ";
            for (size_t i = 0; i < corr.coefficients.size(); ++i) {
                if (i > 0) selection_out << ",";
                selection_out << corr.coefficients[i];
            }
            selection_out << "\n";
            selection_out << "Equation: ";
            for (size_t i = 0; i < corr.eqs.size(); ++i) {
                if (i > 0) selection_out << "|";
                selection_out << corr.eqs[i];
            }
            selection_out << "\n";
            selection_out << "Units: ";
            for (const auto& [k, v] : corr.units) {
                selection_out << k << ":" << v << ",";
            }
            selection_out << "\n";
            selection_out << "---\n";
        } else {
            std::cerr << "Warning: Property '" << property << "' not found or index " << idx << " out of bounds." << std::endl;
        }
    }
    std::cout << " Selected correlation writing in " << outputSelectionFile << std::endl;
}

