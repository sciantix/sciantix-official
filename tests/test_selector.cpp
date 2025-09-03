#include "catch.hpp"
#include "../include/CorrelationSelector.h" 
#include "../include/CorrelationProcessor.h" 
#include <fstream>
#include <sstream>
#include <filesystem>
#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include <iostream>
#include <regex>

namespace fs = std::filesystem;




static void write_to_file(const fs::path& file_path, const std::string& content) {
    std::ofstream ofs(file_path);
    REQUIRE(ofs.is_open()); 
    ofs << content;
    ofs.close();
}


static std::string read_file_content(const fs::path& file_path) {
    std::ifstream ifs(file_path);
    if (!ifs.is_open()) {
        return ""; 
    }
    std::stringstream ss;
    ss << ifs.rdbuf();
    return ss.str();
}


static std::string normalize_string(const std::string& s) {
    std::string temp = s;

    temp.erase(std::remove(temp.begin(), temp.end(), '\r'), temp.end());

    std::stringstream ss(temp);
    std::string line;
    std::string normalized_s = "";
    while (std::getline(ss, line)) {
        size_t first = line.find_first_not_of(" \t");
        size_t last = line.find_last_not_of(" \t");
        if (std::string::npos == first) {
 
            normalized_s += "\n";
        } else {
            normalized_s += line.substr(first, (last - first + 1)) + "\n";
        }
    }

    if (!s.empty() && s.back() != '\n' && !normalized_s.empty()) {
        normalized_s.pop_back();
    }
    return normalized_s;
}



const std::string MOCKED_ALL_CORRELATIONS_CONTENT = R"(
Property: Diffusivity
 Correlation:
 Category: Model
 Title: Fission gas diffusivity modeling in uranium dioxide under irradiation
 Module: Gas
 Variables: T
 Coefficients: D_0=1.2e-9, E_a=3.0e5, R=8.314
 Units: D_0:m²/s E_a:J/mol R:J/mol·K T:K
 Equations:
      - Diffusivity =D_0*exp(-(E_a/(R*T)));
Property: Porosity
 Correlation:
 Category: Model
 Title: Study 4 on thermophysical behavior of advanced fuels
 Module: Matrix
 Variables: T
 Coefficients: A=1.9, B=3.8
 Units: A:mK/W B:m/W T:K Porosity:W/mK
 Equations:
      - Porosity =(1.0/(A+B*T));
 Correlation:
 Category: Model
 Title: Study 5 on thermophysical behavior of advanced fuels
 Module: Matrix
 Variables: T
 Coefficients: A=2.0, B=4.0
 Units: A:mK/W B:m/W T:K Porosity:W/mK
 Equations:
      - Porosity =(1.0/(A+B*T));
Property: Pression
 Correlation:
 Category: Model
 Title: Modeling of fission gas partial pressure in irradiated UO2 fuel
 Module: Gas
 Variables: T
 Coefficients: A=1.5e-3, B=2500, R=8.314
 Units: A:mol/m³ B:K R:J/mol·K T:K Pression:Pa
 Equations:
      - Pression =A*exp(-(B/(R*T)));
Property: Thermal_conductivity
 Correlation:
 Category: Model
 Title: A multi-fidelity multi-scale methodology to accelerate development of fuel performance codes
 Module: Matrix
 Variables: T
 Coefficients: A=3, B=6
 Units: A:mK/W B:m/W T:K Thermal_conductivity:W/mK
 Equations:
      - Thermal_conductivity =(1.0/(A+B*T));
 Correlation:
 Category: Model
 Title: Study 6 on thermophysical behavior of advanced fuels
 Module: Matrix
 Variables: T
 Coefficients: A=2.1, B=4.2
 Units: A:mK/W B:m/W T:K Thermal_conductivity:W/mK
 Equations:
      - Thermal_conductivity =(1.0/(A+B*T));
 Correlation:
 Category: Model
 Title: Study 7 on thermophysical behavior of advanced fuels
 Module: Matrix
 Variables: T
 Coefficients: A=2.2, B=4.4
 Units: A:mK/W B:m/W T:K Thermal_conductivity:W/mK
 Equations:
      - Thermal_conductivity =(1.0/(A+B*T));
)";


TEST_CASE("CorrelationSelector::parseSettingsFile", "[correlation_selector][parseSettingsFile]") {
    fs::path settings_file = "temp_settings.txt";

    SECTION("Valid settings file - all first correlations") {
        std::string content = R"(
0 # Thermal_conductivity
0 # Porosity
0 # Diffusivity
0 # Solubility
0 # Pression
)";
        write_to_file(settings_file, content);
        std::map<std::string, int> settings = parseSettingsFile(settings_file.string());

        REQUIRE(settings.size() == 5);
        REQUIRE(settings["Thermal_conductivity"] == 0);
        REQUIRE(settings["Porosity"] == 0);
        REQUIRE(settings["Diffusivity"] == 0);
        REQUIRE(settings["Solubility"] == 0);
        REQUIRE(settings["Pression"] == 0);
    }

    SECTION("Valid settings file - mixed indices") {
        std::string content = R"(
1 # Porosity
2 # Thermal_conductivity
0 # Diffusivity
)";
        write_to_file(settings_file, content);
        std::map<std::string, int> settings = parseSettingsFile(settings_file.string());

        REQUIRE(settings.size() == 3);
        REQUIRE(settings["Porosity"] == 1);
        REQUIRE(settings["Thermal_conductivity"] == 2);
        REQUIRE(settings["Diffusivity"] == 0);
    }

    SECTION("Settings file with comments and empty lines") {
        std::string content = R"(
# This is a comment
0 # Thermal_conductivity

   # Another comment
1 # Porosity
)";
        write_to_file(settings_file, content);
        std::map<std::string, int> settings = parseSettingsFile(settings_file.string());

        REQUIRE(settings.size() == 2);
        REQUIRE(settings["Thermal_conductivity"] == 0);
        REQUIRE(settings["Porosity"] == 1);
    }

    SECTION("Empty settings file") {
        write_to_file(settings_file, "");
        std::map<std::string, int> settings = parseSettingsFile(settings_file.string());
        REQUIRE(settings.empty());
    }

    SECTION("Settings file with malformed lines (should ignore)") {
        std::string content = R"(
0 # Prop1
InvalidLine
#Prop2
1 # Prop3
2 Prop4 # Missing hash
)";
        write_to_file(settings_file, content);
        std::map<std::string, int> settings = parseSettingsFile(settings_file.string());

        REQUIRE(settings.size() == 2); // Only Prop1 and Prop3 should be parsed correctly
        REQUIRE(settings["Prop1"] == 0);
        REQUIRE(settings["Prop3"] == 1);
        REQUIRE_FALSE(settings.count("InvalidLine"));
        REQUIRE_FALSE(settings.count("Prop4"));
    }

    SECTION("File not found - should exit and print error (requires redirecting cerr)") {
   
        std::streambuf* old_cerr = std::cerr.rdbuf();
        std::stringstream ss_cerr;
        std::cerr.rdbuf(ss_cerr.rdbuf());

        if (fs::exists(settings_file)) {
            fs::remove(settings_file);
        }

        std::cerr.rdbuf(old_cerr);

    }

    if (fs::exists(settings_file)) {
        fs::remove(settings_file);
    }
}



TEST_CASE("writeSelectedCorrelations", "[correlation_selector][writeSelectedCorrelations]") {
    fs::path all_corrs_file = "temp_all_corrs.txt";
    fs::path settings_file = "temp_selection_settings.txt";
    fs::path output_file = "temp_selected_correlations_output.txt";


    write_to_file(all_corrs_file, MOCKED_ALL_CORRELATIONS_CONTENT);


    std::streambuf* old_cerr = std::cerr.rdbuf();
    std::stringstream ss_cerr;
    std::cerr.rdbuf(ss_cerr.rdbuf());

    SECTION("Select first correlation for each property") {
        std::string settings_content = R"(
0 # Diffusivity
0 # Porosity
0 # Pression
0 # Thermal_conductivity
)";
        write_to_file(settings_file, settings_content);
        std::map<std::string, int> settings = parseSettingsFile(settings_file.string());

        writeSelectedCorrelations(all_corrs_file.string(), settings, output_file.string());

        std::string actual_output = read_file_content(output_file);
        std::string expected_output = R"(Property: Diffusivity
Module: Gas
Title: Fission gas diffusivity modeling in uranium dioxide under irradiation
Variables: T
Coefficients: D_0=1.2e-9, E_a=3.0e5, R=8.314
Equation: Diffusivity =D_0*exp(-(E_a/(R*T)));
Units: D_0:m²/s,E_a:J/mol,R:J/mol·K,T:K,
---
Property: Porosity
Module: Matrix
Title: Study 4 on thermophysical behavior of advanced fuels
Variables: T
Coefficients: A=1.9, B=3.8
Equation: Porosity =(1.0/(A+B*T));
Units: A:mK/W,B:m/W,Porosity:W/mK,T:K,
---
Property: Pression
Module: Gas
Title: Modeling of fission gas partial pressure in irradiated UO2 fuel
Variables: T
Coefficients: A=1.5e-3, B=2500, R=8.314
Equation: Pression =A*exp(-(B/(R*T)));
Units: A:mol/m³,B:K,Pression:Pa,R:J/mol·K,T:K,
---
Property: Thermal_conductivity
Module: Matrix
Title: A multi-fidelity multi-scale methodology to accelerate development of fuel performance codes
Variables: T
Coefficients: A=3, B=6
Equation: Thermal_conductivity =(1.0/(A+B*T));
Units: A:mK/W,B:m/W,T:K,Thermal_conductivity:W/mK,
---
)"; // Note the final empty line due to the last ---\n

        REQUIRE(normalize_string(actual_output) == normalize_string(expected_output));
        REQUIRE(ss_cerr.str().empty()); // No warnings expected
        ss_cerr.str(""); // Clear cerr buffer for next section
    }

    SECTION("Select specific correlations for each property (mixed indices)") {
        std::string settings_content = R"(
1 # Porosity
2 # Thermal_conductivity
0 # Diffusivity
)";
        write_to_file(settings_file, settings_content);
        std::map<std::string, int> settings = parseSettingsFile(settings_file.string());

        writeSelectedCorrelations(all_corrs_file.string(), settings, output_file.string());

        std::string actual_output = read_file_content(output_file);
        std::string expected_output = R"(Property: Diffusivity
Module: Gas
Title: Fission gas diffusivity modeling in uranium dioxide under irradiation
Variables: T
Coefficients: D_0=1.2e-9, E_a=3.0e5, R=8.314
Equation: Diffusivity =D_0*exp(-(E_a/(R*T)));
Units: D_0:m²/s,E_a:J/mol,R:J/mol·K,T:K,
---
Property: Porosity
Module: Matrix
Title: Study 5 on thermophysical behavior of advanced fuels
Variables: T
Coefficients: A=2.0, B=4.0
Equation: Porosity =(1.0/(A+B*T));
Units: A:mK/W,B:m/W,Porosity:W/mK,T:K,
---
Property: Thermal_conductivity
Module: Matrix
Title: Study 7 on thermophysical behavior of advanced fuels
Variables: T
Coefficients: A=2.2, B=4.4
Equation: Thermal_conductivity =(1.0/(A+B*T));
Units: A:mK/W,B:m/W,T:K,Thermal_conductivity:W/mK,
---
)";

        REQUIRE(normalize_string(actual_output) == normalize_string(expected_output));
        REQUIRE(ss_cerr.str().empty()); // No warnings expected
        ss_cerr.str("");
    }

    SECTION("Settings map is empty - should produce an empty output file") {
        write_to_file(settings_file, ""); // Empty settings file
        std::map<std::string, int> settings = parseSettingsFile(settings_file.string()); // settings will be empty

        writeSelectedCorrelations(all_corrs_file.string(), settings, output_file.string());

        std::string actual_output = read_file_content(output_file);
        REQUIRE(actual_output.empty());
        REQUIRE(ss_cerr.str().empty()); // No warnings expected
        ss_cerr.str("");
    }

    SECTION("Requested property not found in all_corrs_file - should log warning") {
        std::string settings_content = R"(
0 # NonExistentProperty
0 # Diffusivity
)";
        write_to_file(settings_file, settings_content);
        std::map<std::string, int> settings = parseSettingsFile(settings_file.string());

        writeSelectedCorrelations(all_corrs_file.string(), settings, output_file.string());

        std::string actual_output = read_file_content(output_file);
        // Only Diffusivity should be in the output
        std::string expected_output = R"(Property: Diffusivity
Module: Gas
Title: Fission gas diffusivity modeling in uranium dioxide under irradiation
Variables: T
Coefficients: D_0=1.2e-9, E_a=3.0e5, R=8.314
Equation: Diffusivity =D_0*exp(-(E_a/(R*T)));
Units: D_0:m²/s,E_a:J/mol,R:J/mol·K,T:K,
---
)";
        REQUIRE(normalize_string(actual_output) == normalize_string(expected_output));
        REQUIRE(ss_cerr.str().find("Warning: Property 'NonExistentProperty' not found") != std::string::npos);
        ss_cerr.str("");
    }

    SECTION("Requested index out of bounds - should log warning") {
        std::string settings_content = R"(
5 # Porosity          // Porosity has only 2 correlations (indices 0 and 1)
0 # Diffusivity
)";
        write_to_file(settings_file, settings_content);
        std::map<std::string, int> settings = parseSettingsFile(settings_file.string());

        writeSelectedCorrelations(all_corrs_file.string(), settings, output_file.string());

        std::string actual_output = read_file_content(output_file);
        // Only Diffusivity should be in the output
        std::string expected_output = R"(Property: Diffusivity
Module: Gas
Title: Fission gas diffusivity modeling in uranium dioxide under irradiation
Variables: T
Coefficients: D_0=1.2e-9, E_a=3.0e5, R=8.314
Equation: Diffusivity =D_0*exp(-(E_a/(R*T)));
Units: D_0:m²/s,E_a:J/mol,R:J/mol·K,T:K,
---
)";
        REQUIRE(normalize_string(actual_output) == normalize_string(expected_output));


        REQUIRE(ss_cerr.str().find("Warning: Property 'Porosity          // Porosity has only 2 correlations (indices 0 and 1)' not found or index 5 out of bounds") != std::string::npos);
        ss_cerr.str("");
    }

    
    fs::remove(all_corrs_file);
    fs::remove(settings_file);
    fs::remove(output_file);
    std::cerr.rdbuf(old_cerr); 
}
