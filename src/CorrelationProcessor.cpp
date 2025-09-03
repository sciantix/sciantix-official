#include "CorrelationProcessor.h"
#include "VariableManager.h"
#include "TableManager.h"
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <algorithm>
#include <map>
#include <numeric>
#include <cctype>
#include <filesystem>
#include <vector>


namespace fs = std::filesystem;



static std::string trim(const std::string& s) {
    size_t start = s.find_first_not_of(" \t\r\n#");
    size_t end = s.find_last_not_of(" \t\r\n#");
    if (start == std::string::npos || end == std::string::npos)
        return "";
    return s.substr(start, end - start + 1);
}



std::map<std::string, std::vector<Correlation>> getAllCorrelations(
    const std::string& filename,
    const std::string& out_filename
) {
    std::ifstream input(filename);
    if (!input) {
        std::cerr << "Error opening file " << filename << std::endl;
        exit(1);
    }
    std::ofstream output(out_filename);
    if (!output) {
        std::cerr << "File write error " << out_filename << std::endl;
        exit(1);
    }

    std::map<std::string, std::vector<Correlation>> property_to_corrs;
    std::string line;
    bool in_table_block = false;

    std::map<std::string, std::vector<Correlation>> temp_correlations_by_file;
    std::string current_temp_file_path;

    std::ifstream input_pass1(filename);
    std::string line_pass1;
    Correlation temp_corr_pass1;
    bool collecting_corr = false;
    bool expecting_reference_title_content = false;

    while (std::getline(input_pass1, line_pass1)) {
        if (line_pass1.rfind("File :", 0) == 0) {
            if (collecting_corr) {
                temp_correlations_by_file[current_temp_file_path].push_back(temp_corr_pass1);
            }

            current_temp_file_path = line_pass1.substr(9);
            current_temp_file_path = trim(current_temp_file_path);

            fs::path p(current_temp_file_path);
            std::string property_for_new_corr = p.parent_path().filename().string();

            std::string module_name = "";
            if (p.parent_path().has_parent_path()) {
                module_name = p.parent_path().parent_path().filename().string();
            }

            temp_corr_pass1 = Correlation{property_for_new_corr, "", {}, {}, {}, "", module_name};
            collecting_corr = true;
            in_table_block = false;
            expecting_reference_title_content = false;

        } else if (collecting_corr && !line_pass1.empty()) {
            std::string processed_line_content = line_pass1;

            if (processed_line_content.find("=") == std::string::npos &&
                !processed_line_content.empty() && processed_line_content.back() == ';') {
                processed_line_content.pop_back();
            }

            processed_line_content = trim(processed_line_content);

            if (expecting_reference_title_content) {
                temp_corr_pass1.title = processed_line_content; 
                expecting_reference_title_content = false; 
            }

            if (processed_line_content.rfind("Type :", 0) == 0) {
                if (collecting_corr && !temp_corr_pass1.category.empty()) { 
                    temp_correlations_by_file[current_temp_file_path].push_back(temp_corr_pass1);
                }

                temp_corr_pass1 = Correlation{temp_corr_pass1.property, "", {}, {}, {}, "", temp_corr_pass1.moduleName};

                if (processed_line_content.rfind("Type : Table (tabular)", 0) == 0) {
                    in_table_block = true;
                    temp_corr_pass1.category = "Data Source";
                } else if (processed_line_content.rfind("Type : BibTeX Reference Title", 0) == 0) {
                    in_table_block = false;
                    temp_corr_pass1.category = "Reference";
                    expecting_reference_title_content = true; 
                }
                else { 
                    in_table_block = false;
                    temp_corr_pass1.category = "Model";
                }

            } else if (in_table_block) { 
                if (temp_corr_pass1.category.empty()) temp_corr_pass1.category = "Data Source";
                std::vector<std::string> singleLineVec = {processed_line_content};
                std::vector<std::string> raw_table_data = processTableLines(singleLineVec); 
                for (const auto& data_entry : raw_table_data) {
                    temp_corr_pass1.eqs.push_back(data_entry);
                }
            } else { 
                if (temp_corr_pass1.category.empty()) {
                    temp_corr_pass1.category = "Model";
                }

                if (temp_corr_pass1.category == "Model") { 
                    std::string equation_for_variable_manager = processed_line_content;

                    std::regex k_func_re(R"(^\s*k\(([^)]*)\)\s*(=.*)?$)");
                    std::regex k_bracket_re(R"(^\s*k\[([^\]]*)\]\s*(=.*)?$)");
                    std::smatch k_match;

                    if (std::regex_search(processed_line_content, k_match, k_func_re)) {
                        if (k_match.size() >= 2) {
                            equation_for_variable_manager = temp_corr_pass1.property + "(" + k_match[1].str() + ")" + k_match[2].str();
                        }
                    } else if (std::regex_search(processed_line_content, k_match, k_bracket_re)) {
                        if (k_match.size() >= 2) {
                            equation_for_variable_manager = temp_corr_pass1.property + "(" + k_match[1].str() + ")" + k_match[2].str();
                        }
                    }

                    std::vector<std::string> variables_detected, coefficients_detected;
                    detectVariablesAndCoefficients(equation_for_variable_manager, variables_detected, coefficients_detected); 

                    for (const auto& v : variables_detected) {
                        if (!v.empty() && std::find(temp_corr_pass1.variables.begin(), temp_corr_pass1.variables.end(), v) == temp_corr_pass1.variables.end()) {
                            temp_corr_pass1.variables.push_back(v);
                        }
                    }
                    for (const auto& c : coefficients_detected) {
                        if (!c.empty() && std::find(temp_corr_pass1.coefficients.begin(), temp_corr_pass1.coefficients.end(), c) == temp_corr_pass1.coefficients.end()) {
                            temp_corr_pass1.coefficients.push_back(c);
                        }
                    }

                    std::string cpp_eq = uniformizeVariable(processed_line_content, temp_corr_pass1.property); 
                    temp_corr_pass1.eqs.push_back(cpp_eq);

                }
            }
        }
    }
    if (collecting_corr) {
        temp_correlations_by_file[current_temp_file_path].push_back(temp_corr_pass1);
    }
    input_pass1.close();


    std::map<std::string, std::string> property_to_table_key;

    for (auto const& [file_path, corrs_in_file] : temp_correlations_by_file) {
        std::map<std::string, std::pair<std::string, std::string>> current_file_data_sources_with_units;
        std::map<std::string, std::string> variables_units;
        std::map<std::string, std::string> property_units;
        std::string current_file_reference_title; 

        std::vector<Correlation> processed_correlations_for_output;


        std::string property_table_key_for_this_file;

        for (const auto& corr : corrs_in_file) {
            if (corr.category == "Data Source") {
                for (const auto& data_line : corr.eqs) {
                    std::stringstream ss(data_line);
                    std::string segment;
                    std::vector<std::string> parts;
                    while(std::getline(ss, segment, ';')) {
                        parts.push_back(trim(segment));
                    }

                    if (parts.size() >= 4) {
                        std::string key = parts[0];
                        std::string value = parts[1];
                        std::string units = parts[2];
                        std::string type = parts[3];

                        if (type == "Parameter") {
                            current_file_data_sources_with_units[key] = {value, units};
                        } else if (type == "Variable") {
                            variables_units[key] = units;
                        } else if (type == "Property") {
                            property_units[key] = units;
                            
                            property_table_key_for_this_file = key;
                        }
                    } else {
                        std::cerr << "[WARNING] Incomplete table line (expected 4 parts): '" << data_line << "'\n";
                    }
                }
            } else if (corr.category == "Reference") {
                current_file_reference_title = corr.title; 
            } else {
                processed_correlations_for_output.push_back(corr);
            }
        }

        for (auto& corr : processed_correlations_for_output) {
            if (corr.category == "Model") {
                if (!current_file_reference_title.empty()) {
                    corr.title = current_file_reference_title;
                }

                std::vector<std::string> collected_formatted_coefs;
                corr.units.clear();

                for (const auto& coeff_name : corr.coefficients) {
                    auto it = current_file_data_sources_with_units.find(coeff_name);
                    if (it != current_file_data_sources_with_units.end()) {
                        collected_formatted_coefs.push_back(coeff_name + "=" + it->second.first);
                        corr.units[coeff_name] = it->second.second;
                    }
                }

                corr.coefficients.clear();
                if (!collected_formatted_coefs.empty()) {
                    std::string final_formatted_string = std::accumulate(
                        collected_formatted_coefs.begin() + 1,
                        collected_formatted_coefs.end(),
                        collected_formatted_coefs[0],
                        [](const std::string& a, const std::string& b){
                            return a + ", " + b;
                        }
                    );
                    corr.coefficients.push_back(final_formatted_string);
                }

                for (const auto& var_name : corr.variables) {
                    auto it = variables_units.find(var_name);
                    if (it != variables_units.end()) {
                        corr.units[var_name] = it->second;
                    }
                }

                
                auto it_prop = property_units.find(corr.property);
                if (it_prop == property_units.end() && !property_table_key_for_this_file.empty()) {
                    it_prop = property_units.find(property_table_key_for_this_file);
                }
                if (it_prop == property_units.end()) {
                    it_prop = property_units.find("k");
                }
                if (it_prop != property_units.end()) {
                    corr.units[corr.property] = it_prop->second;
                }
            }
        }

        for (const auto& corr : processed_correlations_for_output) {
            if (corr.category == "Model") {
                property_to_corrs[corr.property].push_back(corr);
            }
        }
    }

    for (const auto& [property, corrs_list] : property_to_corrs) {
        output << "Property: " << property << "\n";
        for (const auto& corr : corrs_list) {
            output << " Correlation:\n";
            output << " Category: " << corr.category << "\n";
            if (!corr.title.empty()) {
                 output << " Title: " << corr.title << "\n";
            }
            if (!corr.moduleName.empty()) {
                 output << " Module: " << corr.moduleName << "\n";
            }

            if (corr.category == "Model") {
                output << " Variables:";
                for (const auto& v : corr.variables) output << " " << v;
                output << "\n";

                output << " Coefficients:";
                if (!corr.coefficients.empty()) {
                    output << " " << corr.coefficients[0];
                }
                output << "\n";

                output << " Units:";
                if (!corr.units.empty()) {
                    std::vector<std::pair<std::string, std::string>> sorted_units(corr.units.begin(), corr.units.end());
                    std::sort(sorted_units.begin(), sorted_units.end(), [](const auto& a, const auto& b){
                    return a.first < b.first;
                    });
                    for (const auto& pair : sorted_units) {
                        output << " " << pair.first << ":" << pair.second;
                    }
                }                 
                output << "\n";

                output << " Equations:\n";
                for (const auto& eq_item : corr.eqs) {
                    if (!eq_item.empty() && eq_item.back() == ';') {
                         output << "      - " << eq_item << "\n";
                    } else {
                        output << "      - " << eq_item << ";\n";
                    }
                }
            }
        }
    }

    return property_to_corrs;
}



std::map<std::string, std::vector<Correlation>> loadAllCorrelations(const std::string& in_filename) {

    std::ifstream input(in_filename);

    if (!input) {
        std::cerr << "ERROR opening file" << in_filename << std::endl;
        exit(1);
    }

    std::map<std::string, std::vector<Correlation>> property_to_corrs;
    std::string line, property;

    Correlation current_corr = Correlation{"", "", {}, {}, {}, "", ""};

    bool in_corr = false;


    while (std::getline(input, line)) {
        if (line.rfind("Property: ", 0) == 0) {
            if (in_corr) {
                property_to_corrs[property].push_back(current_corr);
            }
            property = line.substr(10);

            current_corr = Correlation{property, "", {}, {}, {}, "", ""};
            in_corr = false;
        } else if (line.find(" Correlation:") != std::string::npos) {
            if (in_corr) {
                property_to_corrs[property].push_back(current_corr);
            }
            current_corr = Correlation{property, "", {}, {}, {}, "", ""};
            in_corr = true;
        } else if (line.find(" Category: ") != std::string::npos && in_corr) {
            current_corr.category = line.substr(line.find(":") + 2);
        }

        else if (line.rfind(" Title: ", 0) == 0 && in_corr) {
            current_corr.title = line.substr(line.find(":") + 2);
        }

        else if (line.rfind(" Module: ", 0) == 0 && in_corr) {
            current_corr.moduleName = line.substr(line.find(":") + 2);
        }

        else if (line.find(" Variables:") != std::string::npos && in_corr) {
            current_corr.variables.clear();
            std::istringstream iss(line.substr(line.find(":") + 1));
            std::string v;
            while (iss >> v) current_corr.variables.push_back(v);
        } else if (line.find(" Coefficients:") != std::string::npos && in_corr) {
            current_corr.coefficients.clear();
            std::string coef_line_content = line.substr(line.find(":") + 1);
            current_corr.coefficients.push_back(trim(coef_line_content));
        } else if (line.find(" Units:") != std::string::npos && in_corr) {
            current_corr.units.clear();
            std::string units_content = line.substr(line.find(":") + 1);
            std::istringstream iss(units_content);
            std::string token;
            while(iss >> token) {
                size_t colon_pos = token.find(":");
                if (colon_pos != std::string::npos) {
                    std::string key = trim(token.substr(0, colon_pos));
                    std::string unit = trim(token.substr(colon_pos + 1));
                    if (!key.empty()) {
                        current_corr.units[key] = unit;
                    }
                }
            }
        }
        else if (line.rfind("      - ", 0) == 0) {
            current_corr.eqs.push_back(line.substr(8));
        }

    }


    if (in_corr)
        property_to_corrs[property].push_back(current_corr);


    return property_to_corrs;

}