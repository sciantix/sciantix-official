#include "catch.hpp"
#include "../include/CorrelationProcessor.h" 
#include "../include/VariableManager.h"     
#include "../include/TableManager.h"         
#include <fstream>
#include <sstream>
#include <filesystem>
#include <map>
#include <vector>
#include <string>
#include <algorithm> 
#include <regex>     
#include <iostream>
#include <cassert>

namespace fs = std::filesystem;




void write_to_file(const fs::path& file_path, const std::string& content) {
    std::ofstream ofs(file_path);
    REQUIRE(ofs.is_open()); 
    ofs << content;
    ofs.close();
}


std::string read_file_content(const fs::path& file_path) {
    std::ifstream ifs(file_path);
    if (!ifs.is_open()) {
        return ""; 
    }
    std::stringstream ss;
    ss << ifs.rdbuf();
    return ss.str();
}


std::string normalize_output_string(std::string s) {

    s.erase(std::remove(s.begin(), s.end(), '\r'), s.end());

    s = std::regex_replace(s, std::regex("\\n+"), "\n");

    s = std::regex_replace(s, std::regex("^[ \\t]+|[ \\t]+$"), "");

    std::string result;
    std::stringstream ss(s);
    std::string line;
    while (std::getline(ss, line)) {
        if (!line.empty() && line.find_first_not_of(" \t") != std::string::npos) {
            result += line + "\n";
        }
    }

    if (!result.empty() && result.back() == '\n') {
        result.pop_back();
    }
    return result;
}



bool compareCorrelations(const Correlation& c1, const Correlation& c2) {

    if (c1.property != c2.property) {
        INFO("Property mismatch: '" << c1.property << "' vs '" << c2.property << "'");
        return false;
    }
    if (c1.category != c2.category) {
        INFO("Category mismatch: '" << c1.category << "' vs '" << c2.category << "'");
        return false;
    }
    if (c1.title != c2.title) {
        INFO("Title mismatch: '" << c1.title << "' vs '" << c2.title << "'");
        return false;
    }
    if (c1.moduleName != c2.moduleName) {
        INFO("Module name mismatch: '" << c1.moduleName << "' vs '" << c2.moduleName << "'");
        return false;
    }


    if (c1.variables != c2.variables) {
        INFO("Variables mismatch.");

        return false;
    }
    if (c1.coefficients != c2.coefficients) {
        INFO("Coefficients mismatch.");
        return false;
    }
    if (c1.eqs != c2.eqs) {
        INFO("Equations mismatch.");
        return false;
    }


    if (c1.units.size() != c2.units.size()) {
        INFO("Units map size mismatch.");
        return false;
    }
    for (const auto& pair : c1.units) {
        if (c2.units.find(pair.first) == c2.units.end() || c2.units.at(pair.first) != pair.second) {
            INFO("Units mismatch for key: '" << pair.first << "'. Expected '" << pair.second << "', Got '" << (c2.units.count(pair.first) ? c2.units.at(pair.first) : "NOT_FOUND") << "'");
            return false;
        }
    }

    return true;
}


bool compareCorrelationMaps(
    const std::map<std::string, std::vector<Correlation>>& map1,
    const std::map<std::string, std::vector<Correlation>>& map2)
{
    if (map1.size() != map2.size()) {
        INFO("Map size mismatch: " << map1.size() << " vs " << map2.size());
        return false;
    }

    for (const auto& pair1 : map1) {
        auto it2 = map2.find(pair1.first);
        if (it2 == map2.end()) {
            INFO("Property key missing: " << pair1.first);
            return false;
        }

        // Sort both vectors of correlations before comparing to handle potential order differences
        std::vector<Correlation> vec1 = pair1.second;
        std::vector<Correlation> vec2 = it2->second;


        if (vec1.size() != vec2.size()) {
            INFO("Vector of correlations size mismatch for property: " << pair1.first << ". " << vec1.size() << " vs " << vec2.size());
            return false;
        }

        for (size_t i = 0; i < vec1.size(); ++i) {
            if (!compareCorrelations(vec1[i], vec2[i])) {
                INFO("Individual correlation mismatch for property: " << pair1.first << ", index: " << i);
                return false;
            }
        }
    }
    return true;
}


    void printCorrelationsMap(const std::map<std::string, std::vector<Correlation>>& corr_map) {
    for (const auto& [property, corrs] : corr_map) {
        std::cout << "Property: " << property << "\n";
        for (const auto& c : corrs) {
            std::cout << "  Category: " << c.category << "\n";
            std::cout << "  Title: " << c.title << "\n";
            std::cout << "  Module: " << c.moduleName << "\n";
            std::cout << "  Variables:";
            for (const auto& v : c.variables) std::cout << " " << v;
            std::cout << "\n";
            std::cout << "  Coefficients:";
            for (const auto& coef : c.coefficients) std::cout << " " << coef;
            std::cout << "\n";
            std::cout << "  Units:";
            for (const auto& [k, v] : c.units) std::cout << " " << k << ":" << v;
            std::cout << "\n";
            std::cout << "  Equations:\n";
            for (const auto& eq : c.eqs) std::cout << "    - " << eq << "\n";
        }
    }
}
// --- TESTS ---

TEST_CASE("getAllCorrelations - various scenarios", "[parser][getAllCorrelations]") {

    SECTION("Basic Single Correlation") {
        fs::path temp_input_file = fs::temp_directory_path() / "test_input_single.txt";
        fs::path temp_output_file = fs::temp_directory_path() / "test_output_single.txt";
        std::string input_content = R"(
File : "Correlations/Matrix/Thermal_conductivity/article1.tex";
Type : BibTeX Reference Title;
A study on thermal conductivity of new fuels;
File : "Correlations/Matrix/Thermal_conductivity/article1.tex";
Type : Table (tabular);
{|l|l|l|l|};
\hline;
Key & Value & Units & Type \\;
\hline;
A & 10.5 & W/mK & Parameter\\;
B & 0.05 & /K & Parameter \\;
T &   & K    & Variable\\;
k &   & W/mK & Property\\;
\hline;
File : "Correlations/Matrix/Thermal_conductivity/article1.tex";
Type : Equation environment;
k(T)=(A-B*T);
)";
        write_to_file(temp_input_file, input_content);
        std::map<std::string, std::vector<Correlation>> actual_correlations = getAllCorrelations(temp_input_file.string(), temp_output_file.string());


        std::map<std::string, std::vector<Correlation>> expected_correlations_map;
        Correlation expected_corr;
        expected_corr.property = "Thermal_conductivity";
        expected_corr.category = "Model";
        expected_corr.moduleName = "Matrix";
        expected_corr.title = "A study on thermal conductivity of new fuels";
        expected_corr.variables = {"T"};
        expected_corr.coefficients = {"A=10.5, B=0.05"};
        expected_corr.units["A"] = "W/mK";
        expected_corr.units["B"] = "/K";
        expected_corr.units["T"] = "K";
        expected_corr.units["Thermal_conductivity"] = "W/mK";
        expected_corr.eqs = {"Thermal_conductivity =(A-B*T);"};
        expected_correlations_map["Thermal_conductivity"].push_back(expected_corr);

        REQUIRE(compareCorrelationMaps(actual_correlations, expected_correlations_map));

        std::string actual_output_content = read_file_content(temp_output_file);
        std::string expected_output_content = R"(Property: Thermal_conductivity
 Correlation:
 Category: Model
 Title: A study on thermal conductivity of new fuels
 Module: Matrix
 Variables: T
 Coefficients: A=10.5, B=0.05
 Units: A:W/mK B:/K T:K Thermal_conductivity:W/mK
 Equations:
      - Thermal_conductivity =(A-B*T);
)";
        REQUIRE(normalize_output_string(actual_output_content) == normalize_output_string(expected_output_content));
        fs::remove(temp_input_file);
        fs::remove(temp_output_file);
    }

    SECTION("Multiple Correlations Same Property") {
        fs::path temp_input_file = fs::temp_directory_path() / "test_input_multiple_same_prop.txt";
        fs::path temp_output_file = fs::temp_directory_path() / "test_output_multiple_same_prop.txt";
        std::string input_content = R"(
File : "Correlations/Matrix/Porosity/article4.tex";
Type : BibTeX Reference Title;
Study 4 on thermophysical behavior of advanced fuels;
File : "Correlations/Matrix/Porosity/article4.tex";
Type : Table (tabular);
{|l|l|l|l|};
\hline;
Key & Value & Units & Type \;
\hline;
A & 1.9 & mK/W & Parameter\\;
B & 3.8 & m/W & Parameter \\;
T &   & K    & Variable\\;
k &   & W/mK & Property\\;
\hline;
File : "Correlations/Matrix/Porosity/article4.tex";
Type : Equation environment;
k(T)=(1.0/(A+B*T));
File : "Correlations/Matrix/Porosity/article5.tex";
Type : BibTeX Reference Title;
Study 5 on thermophysical behavior of advanced fuels;
File : "Correlations/Matrix/Porosity/article5.tex";
Type : Table (tabular);
{|l|l|l|l|};
\hline;
Key & Value & Units & Type \;
\hline;
A & 2.0 & mK/W & Parameter\\;
B & 4.0 & m/W & Parameter \\;
T &   & K    & Variable\\;
k &   & W/mK & Property\\;
\hline;
File : "Correlations/Matrix/Porosity/article5.tex";
Type : Equation environment;
k(T)=(1.0/(A+B*T));
)";
        write_to_file(temp_input_file, input_content);
        std::map<std::string, std::vector<Correlation>> actual_correlations = getAllCorrelations(temp_input_file.string(), temp_output_file.string());

        std::map<std::string, std::vector<Correlation>> expected_correlations_map;
        Correlation corr1_porosity;
        corr1_porosity.property = "Porosity";
        corr1_porosity.category = "Model";
        corr1_porosity.moduleName = "Matrix";
        corr1_porosity.title = "Study 4 on thermophysical behavior of advanced fuels";
        corr1_porosity.variables = {"T"};
        corr1_porosity.coefficients = {"A=1.9, B=3.8"};
        corr1_porosity.units["A"] = "mK/W";
        corr1_porosity.units["B"] = "m/W";
        corr1_porosity.units["T"] = "K";
        corr1_porosity.units["Porosity"] = "W/mK";
        corr1_porosity.eqs = {"Porosity =(1.0/(A+B*T));"};
        expected_correlations_map["Porosity"].push_back(corr1_porosity);

        Correlation corr2_porosity;
        corr2_porosity.property = "Porosity";
        corr2_porosity.category = "Model";
        corr2_porosity.moduleName = "Matrix";
        corr2_porosity.title = "Study 5 on thermophysical behavior of advanced fuels";
        corr2_porosity.variables = {"T"};
        corr2_porosity.coefficients = {"A=2.0, B=4.0"};
        corr2_porosity.units["A"] = "mK/W";
        corr2_porosity.units["B"] = "m/W";
        corr2_porosity.units["T"] = "K";
        corr2_porosity.units["Porosity"] = "W/mK";
        corr2_porosity.eqs = {"Porosity =(1.0/(A+B*T));"};
        expected_correlations_map["Porosity"].push_back(corr2_porosity);

        REQUIRE(compareCorrelationMaps(actual_correlations, expected_correlations_map));

        std::string actual_output_content = read_file_content(temp_output_file);
        std::string expected_output_content = R"(Property: Porosity
 Correlation:
 Category: Model
 Title: Study 4 on thermophysical behavior of advanced fuels
 Module: Matrix
 Variables: T
 Coefficients: A=1.9, B=3.8
 Units: A:mK/W B:m/W Porosity:W/mK T:K
 Equations:
      - Porosity =(1.0/(A+B*T));
 Correlation:
 Category: Model
 Title: Study 5 on thermophysical behavior of advanced fuels
 Module: Matrix
 Variables: T
 Coefficients: A=2.0, B=4.0
 Units: A:mK/W B:m/W Porosity:W/mK T:K
 Equations:
      - Porosity =(1.0/(A+B*T));
)";
        REQUIRE(normalize_output_string(actual_output_content) == normalize_output_string(expected_output_content));
        fs::remove(temp_input_file);
        fs::remove(temp_output_file);
    }

    SECTION("Mixed Properties and Modules") {
        fs::path temp_input_file = fs::temp_directory_path() / "test_input_mixed.txt";
        fs::path temp_output_file = fs::temp_directory_path() / "test_output_mixed.txt";
        std::string input_content = R"(
File : "Correlations/Matrix/Thermal_conductivity/article8.tex";
Type : BibTeX Reference Title;
Study 8 on thermophysical behavior of advanced fuels;
File : "Correlations/Matrix/Thermal_conductivity/article8.tex";
Type : Table (tabular);
{|l|l|l|l|};
\hline;
Key & Value & Units & Type \\;
\hline;
A & 2.3 & mK/W & Parameter\\;
B & 4.6 & m/W & Parameter \\;
C & 8.9 & m/W & Parameter \\;
T &   & K    & Variable\\;
k &   & W/mK & Property\\;
\hline;
File : "Correlations/Matrix/Thermal_conductivity/article8.tex";
Type : Equation environment;
k(T)=(1.0/(A+B*T));
File : "Correlations/Gas/Pression/pression_gaz_model.tex";
Type : BibTeX Reference Title;
Modeling of fission gas partial pressure in irradiated UO2 fuel;
File : "Correlations/Gas/Pression/pression_gaz_model.tex";
Type : Table (tabular);
{|l|l|l|l|};
\hline;
Key & Value & Units & Type \\;
\hline;
A & 1.5e-3 & mol/m³ & Parameter \\;
B & 2500 & K & Parameter \\;
R & 8.314 & J/mol·K & Parameter\\;
T &   & K & Variable \\;
P_gas &   & Pa & Property \\;
\hline;
File : "Correlations/Gas/Pression/pression_gaz_model.tex";
Type : Equation environment;
P_gas(T)=A*exp(-(B/(R*T)));
)";
        write_to_file(temp_input_file, input_content);
        std::map<std::string, std::vector<Correlation>> actual_correlations = getAllCorrelations(temp_input_file.string(), temp_output_file.string());

        std::map<std::string, std::vector<Correlation>> expected_correlations_map;
        Correlation corr_thermal;
        corr_thermal.property = "Thermal_conductivity";
        corr_thermal.category = "Model";
        corr_thermal.moduleName = "Matrix";
        corr_thermal.title = "Study 8 on thermophysical behavior of advanced fuels";
        corr_thermal.variables = {"T"};
        corr_thermal.coefficients = {"A=2.3, B=4.6"};
        corr_thermal.units["A"] = "mK/W";
        corr_thermal.units["B"] = "m/W";
        corr_thermal.units["T"] = "K";
        corr_thermal.units["Thermal_conductivity"] = "W/mK";
        corr_thermal.eqs = {"Thermal_conductivity =(1.0/(A+B*T));"};
        expected_correlations_map["Thermal_conductivity"].push_back(corr_thermal);

        Correlation corr_pgas;
        corr_pgas.property = "Pression";
        corr_pgas.category = "Model";
        corr_pgas.moduleName = "Gas";
        corr_pgas.title = "Modeling of fission gas partial pressure in irradiated UO2 fuel";
        corr_pgas.variables = {"T"};
        corr_pgas.coefficients = {"A=1.5e-3, B=2500, R=8.314"};
        corr_pgas.units["A"] = "mol/m³";
        corr_pgas.units["B"] = "K";
        corr_pgas.units["R"] = "J/mol·K";
        corr_pgas.units["T"] = "K";
        corr_pgas.units["Pression"] = "Pa";
        corr_pgas.eqs = {"Pression =A*exp(-(B/(R*T)));"};
        expected_correlations_map["Pression"].push_back(corr_pgas);

        REQUIRE(compareCorrelationMaps(actual_correlations, expected_correlations_map));

        std::string actual_output_content = read_file_content(temp_output_file);
        std::string expected_output_content = R"(Property: Pression
 Correlation:
 Category: Model
 Title: Modeling of fission gas partial pressure in irradiated UO2 fuel
 Module: Gas
 Variables: T
 Coefficients: A=1.5e-3, B=2500, R=8.314
 Units: A:mol/m³ B:K Pression:Pa R:J/mol·K T:K
 Equations:
      - Pression =A*exp(-(B/(R*T)));
Property: Thermal_conductivity
 Correlation:
 Category: Model
 Title: Study 8 on thermophysical behavior of advanced fuels
 Module: Matrix
 Variables: T
 Coefficients: A=2.3, B=4.6
 Units: A:mK/W B:m/W T:K Thermal_conductivity:W/mK
 Equations:
      - Thermal_conductivity =(1.0/(A+B*T));
)";
        REQUIRE(normalize_output_string(actual_output_content) == normalize_output_string(expected_output_content));
        fs::remove(temp_input_file);
        fs::remove(temp_output_file);
    }

    SECTION("No Table Data (only equation)") {
        fs::path temp_input_file = fs::temp_directory_path() / "test_input_no_table.txt";
        fs::path temp_output_file = fs::temp_directory_path() / "test_output_no_table.txt";
        std::string input_content = R"(
File : "Correlations/Gas/Temperature/temp_model.tex";
Type : BibTeX Reference Title;
Simple temperature calculation;
File : "Correlations/Gas/Temperature/temp_model.tex";
Type : Equation environment;
T_final=T_initial + delta_T;
)";
        write_to_file(temp_input_file, input_content);
        std::map<std::string, std::vector<Correlation>> actual_correlations = getAllCorrelations(temp_input_file.string(), temp_output_file.string());

        std::map<std::string, std::vector<Correlation>> expected_correlations_map;
        Correlation expected_corr;
        expected_corr.property = "Temperature";
        expected_corr.category = "Model";
        expected_corr.moduleName = "Gas";
        expected_corr.title = "Simple temperature calculation";
        expected_corr.variables = {};
        expected_corr.coefficients = {};
        expected_corr.units = {};
        expected_corr.eqs = {"Temperature =T_initial + delta_T;"};
        expected_correlations_map["Temperature"].push_back(expected_corr);

        REQUIRE(compareCorrelationMaps(actual_correlations, expected_correlations_map));

        std::string actual_output_content = read_file_content(temp_output_file);
        std::string expected_output_content = R"(Property: Temperature
 Correlation:
 Category: Model
 Title: Simple temperature calculation
 Module: Gas
 Variables:
 Coefficients:
 Units:
 Equations:
      - Temperature =T_initial + delta_T;
)";
        REQUIRE(normalize_output_string(actual_output_content) == normalize_output_string(expected_output_content));
        fs::remove(temp_input_file);
        fs::remove(temp_output_file);
    }

    SECTION("Empty Input File") {
        fs::path temp_input_file = fs::temp_directory_path() / "test_input_empty.txt";
        fs::path temp_output_file = fs::temp_directory_path() / "test_output_empty.txt";
        write_to_file(temp_input_file, "");
        std::map<std::string, std::vector<Correlation>> actual_correlations = getAllCorrelations(temp_input_file.string(), temp_output_file.string());
        REQUIRE(actual_correlations.empty());
        std::string actual_output_content = read_file_content(temp_output_file);
        REQUIRE(normalize_output_string(actual_output_content).empty());
        fs::remove(temp_input_file);
        fs::remove(temp_output_file);
    }
}

TEST_CASE("loadAllCorrelations - Basic Valid Input", "[parser][loadAllCorrelations]") {

    std::string test_input_file = "temp_load_correlations_basic.txt";


    std::string file_content = 
        "Property: Porosity\n"
        " Correlation:\n"
        " Category: Model\n"
        " Title: Simple Porosity Model\n"
        " Module: Matrix\n"
        " Variables: T\n"
        " Coefficients: A=1.0, B=2.0\n"
        " Units: A:unitA B:unitB T:K\n"
        " Equations:\n"
        "      - Porosity = A + B * T;\n";

 
    {
        std::ofstream ofs(test_input_file);
        REQUIRE(ofs);
        ofs << file_content;
        ofs.close();
    }


    auto loaded_correlations = loadAllCorrelations(test_input_file);


    SECTION("Map structure and size") {
        REQUIRE(loaded_correlations.size() == 1); 
        REQUIRE(loaded_correlations.count("Porosity") == 1); 
        REQUIRE(loaded_correlations["Porosity"].size() == 1); 
    }

    SECTION("Content of the loaded Correlation") {
        const Correlation& corr = loaded_correlations["Porosity"][0]; 

        REQUIRE(corr.property == "Porosity");
        REQUIRE(corr.category == "Model");
        REQUIRE(corr.title == "Simple Porosity Model");
        REQUIRE(corr.moduleName == "Matrix");

        
        REQUIRE(corr.variables.size() == 1);
        REQUIRE(corr.variables[0] == "T");

        
        REQUIRE(corr.coefficients.size() == 1);
        REQUIRE(corr.coefficients[0] == "A=1.0, B=2.0");

        
        REQUIRE(corr.units.size() == 3);
        REQUIRE(corr.units.at("A") == "unitA");
        REQUIRE(corr.units.at("B") == "unitB");
        REQUIRE(corr.units.at("T") == "K");

        
        REQUIRE(corr.eqs.size() == 1);
        REQUIRE(corr.eqs[0] == "Porosity = A + B * T;");
    }

    
    std::remove(test_input_file.c_str());
}

TEST_CASE("processTableLines", "[parser][table_manager][processTableLines]") {

    SECTION("Base: simple table lines") {
        std::vector<std::string> raw_table_lines = {
            "Key & Value & Units & Type \\\\;",
            "A & 1.9 & mK/W & Parameter\\\\;",
            "B & 3.8 & m/W & Parameter \\\\;",
            "T &   & K    & Variable\\\\;",
            "k &   & W/mK & Property\\\\;"
        };

        std::vector<std::string> expected_formatted_lines = {
            "A;1.9;mK/W;Parameter;",
            "B;3.8;m/W;Parameter;",
            "T;;K;Variable;",
            "k;;W/mK;Property;"
        };

        std::vector<std::string> actual_formatted_lines = processTableLines(raw_table_lines);
        REQUIRE(actual_formatted_lines == expected_formatted_lines);

        
        raw_table_lines = {
            "\\hline;",
            "Key & Value & Units & Type \\\\;",
            "\\toprule;",
            "A & 10 & unit & Parameter \\\\;",
            "\\midrule;",
            "\\bottomrule;",
            "", 
            "{|l|l|l|l|};", 
            "Data: some data", 
            "Var1 & 1 & u1 & Variable \\\\;",
            "Format: some format" 
        };

        expected_formatted_lines = {
            "A;10;unit;Parameter;",
            "Var1;1;u1;Variable;"
        };

        actual_formatted_lines = processTableLines(raw_table_lines);
        REQUIRE(actual_formatted_lines == expected_formatted_lines);
    }

    SECTION("Malformed data lines - Too few delimiters") {
        std::vector<std::string> raw_lines = {
            "Key & Value & Units & Type \\\\;",
            "A & 1.9 & mK/W \\\\;" 
        };
        std::vector<std::string> expected = {}; 
        REQUIRE(processTableLines(raw_lines) == expected);
    }

    SECTION("Malformed data lines - Too many delimiters (should take first 4)") {
        std::vector<std::string> raw_lines = {
            "Key & Value & Units & Type \\\\;",
            "A & 1.9 & mK/W & Parameter & ExtraInfo \\\\;"
        };
        std::vector<std::string> expected = {
            "A;1.9;mK/W;Parameter;"
        };
        REQUIRE(processTableLines(raw_lines) == expected);
    }

    SECTION("Empty cells in data lines") {
        std::vector<std::string> raw_lines = {
            "Key & Value & Units & Type \\\\;",
            "A & 1.0 & & Parameter \\\\;", 
            "B & & K & Variable \\\\;"   
        };
        std::vector<std::string> expected = {
            "A;1.0;;Parameter;",
            "B;;K;Variable;"
        };
        REQUIRE(processTableLines(raw_lines) == expected);
    }

    SECTION("Content with escaped LaTeX characters in cells") {
        std::vector<std::string> raw_lines = {
            "Key & Value & Units & Type \\\\;",
            "A & {\\tau} & s & Time \\\\;"
        };
        std::vector<std::string> expected = {
            "A;{\\tau};s;Time;"
        };
        REQUIRE(processTableLines(raw_lines) == expected);
    }

    SECTION("Content with numbers/units that include spaces (should be trimmed per segment)") {
        std::vector<std::string> raw_lines = {
            "Key & Value & Units & Type \\\\;",
            "D & 1.2e-5 & m^2/s & Diffusivity \\\\;"
        };
        std::vector<std::string> expected = {
            "D;1.2e-5;m^2/s;Diffusivity;"
        };
        REQUIRE(processTableLines(raw_lines) == expected);
    }

    SECTION("Empty input vector") {
        REQUIRE(processTableLines({}) == std::vector<std::string>{});
    }

    SECTION("Lines that are just LaTeX comments or other non-data commands (should be ignored)") {
        std::vector<std::string> raw_lines = {
            "% This is a comment line",
            "\\usepackage{amsmath}",
            "Key & Value & Units & Type \\\\;",
            "A & 1.0 & unit & Parameter \\\\;"
        };
        std::vector<std::string> expected = {
            "A;1.0;unit;Parameter;"
        };
        REQUIRE(processTableLines(raw_lines) == expected);
    }

    SECTION("Lines with pipe '|' characters (often from tabular environment definition)") {
        std::vector<std::string> raw_lines = {
            "Key & Value & Units & Type \\\\;",
            "A & 1.0 & unit & Parameter \\\\;",
            "{|l|c|r|l|};" // Should be ignored
        };
        std::vector<std::string> expected = {
            "A;1.0;unit;Parameter;"
        };
        REQUIRE(processTableLines(raw_lines) == expected);
    }

    SECTION("Mix of ignored lines and valid data") {
        std::vector<std::string> raw_lines = {
            "\\toprule;",
            "Data: My Data",
            "Key & Value & Units & Type \\\\;",
            "\\hline;",
            "X & 100 & unitX & TypeX \\\\;",
            "", // Empty line
            "Y & 200 & unitY & TypeY \\\\;",
            "\\bottomrule;",
            "Format: some_format"
        };
        std::vector<std::string> expected = {
            "X;100;unitX;TypeX;",
            "Y;200;unitY;TypeY;"
        };
        REQUIRE(processTableLines(raw_lines) == expected);
    }

    SECTION("Header line with extra spaces or different case (should still be ignored)") {
        std::vector<std::string> raw_lines = {
            "  key   &   VALUE   &   uNiTs   &   TYPE   \\\\;", // Different casing and spaces
            "A & 1 & u & T \\\\;"
        };
        std::vector<std::string> expected = {
            "A;1;u;T;"
        };
        REQUIRE(processTableLines(raw_lines) == expected);
    }
}

TEST_CASE("uniformizeVariable - all scenarios", "[parser][variable_manager][uniformizeVariable]") {

    SECTION("Base cases") {
        REQUIRE(uniformizeVariable("k(T)=(A-B*T);", "Thermal_conductivity") == "Thermal_conductivity =(A-B*T);");
        REQUIRE(uniformizeVariable("P_m=0.15;", "Porosity") == "Porosity =0.15;");
        REQUIRE(uniformizeVariable("SomeFunction(x,y)=x+y;", "MyFunction") == "MyFunction =x+y;");
        REQUIRE(uniformizeVariable("just_a_variable", "my_prop") == "just_a_variable"); // No '=' sign
    }

    SECTION("Equation with multiple equals signs") {
        REQUIRE(uniformizeVariable("Property = Val = OtherVal", "NewProp") == "NewProp = Val = OtherVal");
    }

    SECTION("Property string with special characters") {
        REQUIRE(uniformizeVariable("k = 1.2 * T", "k_eff(T)") == "k_eff(T) = 1.2 * T");
    }

    SECTION("Equation with leading/trailing spaces around equals") {
        REQUIRE(uniformizeVariable("  k   =   1.2 * T  ", "k_new") == "k_new =   1.2 * T  ");
    }

    SECTION("Empty equation string") {
        REQUIRE(uniformizeVariable("", "Prop") == "");
    }

    SECTION("Empty property string") {
        REQUIRE(uniformizeVariable("k = 1.2 * T", "") == " = 1.2 * T");
    }

    SECTION("No equals sign, property name inside") {
        REQUIRE(uniformizeVariable("1.2 * T + Property", "Property") == "1.2 * T + Property");
    }
}

TEST_CASE("getInputVariables", "[variable_manager][getInputVariables]") {
    SECTION("Simple cases with parentheses") {
        REQUIRE(getInputVariables("func(x)") == std::vector<std::string>{"x"});
        REQUIRE(getInputVariables("func(a,b,c)") == std::vector<std::string>{"a", "b", "c"});
    }

    SECTION("Simple cases with square brackets") {
        REQUIRE(getInputVariables("func[x]") == std::vector<std::string>{"x"});
        REQUIRE(getInputVariables("func[a,b,c]") == std::vector<std::string>{"a", "b", "c"});
    }

    SECTION("No variables inside delimiters") {
        REQUIRE(getInputVariables("func()") == std::vector<std::string>{});
        REQUIRE(getInputVariables("func[]") == std::vector<std::string>{});
        REQUIRE(getInputVariables("func( )") == std::vector<std::string>{});
    }

    SECTION("Variables with numbers and underscores") {
        REQUIRE(getInputVariables("calc(var_1, myVar2)") == std::vector<std::string>{"var_1", "myVar2"});
    }

    SECTION("Spaces within delimiters") {
        REQUIRE(getInputVariables("f( var1 , var2 )") == std::vector<std::string>{"var1", "var2"});
    }

    SECTION("No matching pattern") {
        REQUIRE(getInputVariables("just_a_variable") == std::vector<std::string>{});
        REQUIRE(getInputVariables("x + y = z") == std::vector<std::string>{});
    }

    SECTION("Empty string input") {
        REQUIRE(getInputVariables("") == std::vector<std::string>{});
    }

    SECTION("More complex function names") {
        REQUIRE(getInputVariables("myFunction_ABC(val1, val_2)") == std::vector<std::string>{"val1", "val_2"});
    }
}

TEST_CASE("getAllSymbols", "[variable_manager][getAllSymbols]") {
    SECTION("Simple equation") {
        REQUIRE(getAllSymbols("x = a + b * c") == std::set<std::string>{"a", "b", "c"});
    }

    SECTION("Equation with keywords (should be excluded)") {
        REQUIRE(getAllSymbols("y = pow(A, 2) + log(B) - sin(C)") == std::set<std::string>{"A", "B", "C"});
    }

    SECTION("Equation with numbers and underscores") {
        REQUIRE(getAllSymbols("z = var_1 + 2.0 * Coeff2") == std::set<std::string>{"var_1", "Coeff2"});
    }

    SECTION("No equals sign (should return empty set)") {
        REQUIRE(getAllSymbols("a + b + c") == std::set<std::string>{});
    }

    SECTION("Empty right-hand side") {
        REQUIRE(getAllSymbols("x = ") == std::set<std::string>{});
    }

    SECTION("Complex expression") {
        std::set<std::string> expected = {"alpha", "beta", "gamma", "delta_eff", "T"};
        REQUIRE(getAllSymbols("res = (alpha * beta) / (gamma + delta_eff) + exp(T)") == expected);
    }

    SECTION("Equation with leading/trailing spaces") {
        REQUIRE(getAllSymbols("  Result  =  V1  * V2  ") == std::set<std::string>{"V1", "V2"});
    }

    SECTION("Empty string input") {
        REQUIRE(getAllSymbols("") == std::set<std::string>{});
    }

    SECTION("Equation with only numbers and operators on RHS") {
        REQUIRE(getAllSymbols("result = 1 + 2 * 3.0") == std::set<std::string>{});
    }
}

TEST_CASE("detectVariables", "[variable_manager][detectVariables]") {
    SECTION("Property is present in symbols, should be removed") {
        std::set<std::string> expected = {"A", "B", "x"};
        REQUIRE(detectVariables("y = A + B * x", "y") == expected);
    }

    SECTION("Property is not present in symbols, should remain unchanged (except property removal)") {
        std::set<std::string> expected = {"A", "B", "x"};
        REQUIRE(detectVariables("z = A + B * x", "someOtherProp") == expected);
    }

    SECTION("Empty equation string") {
        REQUIRE(detectVariables("", "Prop") == std::set<std::string>{});
    }

    SECTION("Empty property string") {
        std::set<std::string> expected = {"A", "B", "x"};
        REQUIRE(detectVariables("y = A + B * x", "") == expected); // Should return all symbols as no specific property to remove
    }

    SECTION("Property matches a keyword, should still be removed if found by getAllSymbols") {
        // Note: getAllSymbols would not find "pow" if it's a keyword, but let's test if it were a variable
        // This test case demonstrates 'erase' behavior
        std::set<std::string> expected = {"x", "y"};
        REQUIRE(detectVariables("result = pow(x, y)", "pow") == expected);
    }
}

TEST_CASE("makeInitializations", "[variable_manager][makeInitializations]") {
    SECTION("Empty set of variables") {
        REQUIRE(makeInitializations({}) == std::vector<std::string>{});
    }

    SECTION("Set with one variable") {
        REQUIRE(makeInitializations({"x"}) == std::vector<std::string>{"double x = 0.0;"});
    }

    SECTION("Set with multiple variables") {
        std::vector<std::string> expected = {"double pressure = 0.0;", "double temp = 0.0;", "double volume = 0.0;"};
        std::vector<std::string> actual = makeInitializations({"temp", "pressure", "volume"});
        std::sort(actual.begin(), actual.end()); // Order might not be guaranteed for set iteration
        std::sort(expected.begin(), expected.end());
        REQUIRE(actual == expected);
    }

    SECTION("Variables with numbers and underscores") {
        std::vector<std::string> expected = {"double myVar2 = 0.0;", "double var_1 = 0.0;"};
        std::vector<std::string> actual = makeInitializations({"var_1", "myVar2"});
        std::sort(actual.begin(), actual.end());
        std::sort(expected.begin(), expected.end());
        REQUIRE(actual == expected);
    }
}

TEST_CASE("detectVariablesAndCoefficients", "[variable_manager][detectVariablesAndCoefficients]") {
    std::vector<std::string> variables;
    std::vector<std::string> coefficients;

    SECTION("Equation with distinct input variables and coefficients") {
        variables.clear(); coefficients.clear();
        detectVariablesAndCoefficients("y(a, b) = c + d * a", variables, coefficients);
        REQUIRE(variables == std::vector<std::string>{"a", "b"});
        std::sort(coefficients.begin(), coefficients.end()); // std::set to std::vector conversion order not guaranteed
        REQUIRE(coefficients == std::vector<std::string>{"c", "d"});
    }

    SECTION("Equation where input variables are also coefficients (should be only in variables)") {
        variables.clear(); coefficients.clear();
        detectVariablesAndCoefficients("y(a) = a + b", variables, coefficients);
        REQUIRE(variables == std::vector<std::string>{"a"});
        REQUIRE(coefficients == std::vector<std::string>{"b"}); // 'a' should not be in coefficients
    }

    SECTION("No input variables (only coefficients)") {
        variables.clear(); coefficients.clear();
        detectVariablesAndCoefficients("x = A + B * C", variables, coefficients);
        REQUIRE(variables == std::vector<std::string>{});
        std::sort(coefficients.begin(), coefficients.end());
        REQUIRE(coefficients == std::vector<std::string>{"A", "B", "C"});
    }

    SECTION("No coefficients (only input variables, or all symbols are input variables)") {
        variables.clear(); coefficients.clear();
        detectVariablesAndCoefficients("y(a,b) = a + b", variables, coefficients);
        REQUIRE(variables == std::vector<std::string>{"a", "b"});
        REQUIRE(coefficients == std::vector<std::string>{});
    }

    SECTION("Empty equation string") {
        variables.clear(); coefficients.clear();
        detectVariablesAndCoefficients("", variables, coefficients);
        REQUIRE(variables.empty());
        REQUIRE(coefficients.empty());
    }

    SECTION("Equation with keywords (should not be in variables or coefficients unless explicitly listed in getInputVariables)") {
        variables.clear(); coefficients.clear();
        detectVariablesAndCoefficients("f(x) = pow(x, y) + z", variables, coefficients);
        REQUIRE(variables == std::vector<std::string>{"x"});
        std::sort(coefficients.begin(), coefficients.end());
        REQUIRE(coefficients == std::vector<std::string>{"y", "z"}); // 'pow' is a keyword, not a symbol
    }

    SECTION("Equation with no '=' sign") {
        variables.clear(); coefficients.clear();
        detectVariablesAndCoefficients("A + B + C", variables, coefficients);
        REQUIRE(variables.empty());
        REQUIRE(coefficients.empty()); // getAllSymbols returns empty if no '='
    }
}

