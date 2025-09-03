#include "catch.hpp"
#include "../include/OutputGenerator.h"
#include "../include/CorrelationProcessor.h"
#include <fstream>
#include <filesystem>
#include <string>
#include <sstream>
#include <map>
#include <vector>
#include <algorithm>
#include <iostream>

namespace fs = std::filesystem;



static void create_temp_file(const fs::path& file_path, const std::string& content) {
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

std::string normalize_string(std::string s) {
    s.erase(std::remove(s.begin(), s.end(), '\r'), s.end());
    std::stringstream ss(s);
    std::string line;
    std::string normalized_s;
    while (std::getline(ss, line)) {
        size_t first = line.find_first_not_of(" \t");
        size_t last = line.find_last_not_of(" \t");
        if (first == std::string::npos) {
            normalized_s += "\n";
        } else {
            normalized_s += line.substr(first, (last - first + 1)) + "\n";
        }
    }
    while (!normalized_s.empty() && (normalized_s.back() == '\n' || normalized_s.back() == ' ')) {
        normalized_s.pop_back();
    }
    return normalized_s;
}

static void clean_test_files(const std::vector<std::string>& files_or_dirs) {
    for (const auto& path : files_or_dirs) {
        std::error_code ec;
        if (fs::exists(path)) {
            if (fs::is_directory(path, ec)) {
                fs::remove_all(path, ec);
            } else {
                fs::remove(path, ec);
            }
        }
    }
}


const std::string MOCKED_SELECTED_CORRELATIONS_CONTENT = R"(Property: Diffusivity
Module: Gas
Title: Fission gas diffusivity modeling in uranium dioxide under irradiation
Variables: T
Coefficients: D_0=1.2e-9, E_a=3.0e5, R=8.314
Equation: Diffusivity =D_0*exp(-(E_a/(R*T)));
Units: D_0:m²/s,Diffusivity:m²/s,E_a:J/mol,R:J/mol·K,T:K,
---
Property: Porosity
Module: Matrix
Title: Study 4 on thermophysical behavior of advanced fuels
Variables: T
Coefficients: A=5.0, B=3.0
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
Property: Solubility
Module: Gas
Title: Solubility limits of xenon in uranium dioxide: A computational assessment
Variables: T
Coefficients: E_s=1.1e5, R=8.314, S_0=3.5e-5
Equation: Solubility =S_0*exp(-(E_s/(R*T)));
Units: E_s:J/mol,R:J/mol·K,S_0:mol/m³,Solubility:mol/m³,T:K,
---
Property: Thermal_conductivity
Module: Matrix
Title: A multi-fidelity multi-scale methodology to accelerate development of fuel performance codes
Variables: T
Coefficients: A=3, B=6
Equation: Thermal_conductivity =(1.0/(A+B*T));
Units: A:mK/W,B:m/W,T:K,Thermal_conductivity:W/mK,
---
)";

const std::string MOCKED_VARIABLE_MAPPING_CONTENT = R"(T=history_variable["Temperature"].getFinalValue()
t=history_variable["time"].getFinalValue()
dt=history_variable["Time step number"].getFinalValue()
P=history_variable["Steam pressure"].getFinalValue()
)";

// === TESTS ===

TEST_CASE("generateOutputsFromSelected (chemins réels, nettoyage après tests)", "[OutputGeneratorFromSelected]") {

    fs::path temp_root = fs::temp_directory_path() / fs::path("outputgenerator_test_tmp");
    fs::path temp_input_dir = temp_root / "input";
    fs::path temp_selected_correlations_file = temp_input_dir / "selected_correlations.txt";
    fs::path temp_variable_mapping_file = temp_input_dir / "variable_mapping.txt";

    
    fs::path cpp_output_dir = "../sciantix-official-main/src/generated_correlations";
    fs::path h_output_dir = "../sciantix-official-main/include/generated_correlations";
    fs::path interpretation_report_file = "output/interpretation_report.txt";

    auto setup_standard_files = [&]() {
        clean_test_files({temp_root.string()});
        clean_test_files({cpp_output_dir.string(), h_output_dir.string(), "output"});
        fs::create_directories(temp_root);
        fs::create_directories(temp_input_dir);
        create_temp_file(temp_selected_correlations_file, MOCKED_SELECTED_CORRELATIONS_CONTENT);
        create_temp_file(temp_variable_mapping_file, MOCKED_VARIABLE_MAPPING_CONTENT);
    };

    auto run_generate_outputs = [&]() {
        generateOutputsFromSelected(temp_selected_correlations_file.string(), temp_variable_mapping_file.string());
    };


    const std::string EXPECTED_DIFFUSIVITY_CPP_CONTENT = R"(#include <cmath>
#include <iostream>
#include <string>

#include "Gas.h"

void Gas::setDiffusivity(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices) {
    // Source: Fission gas diffusivity modeling in uranium dioxide under irradiation
    std::string reference = "";
    reference += "Fission gas diffusivity modeling in uranium dioxide under irradiation";
    // Units:
    //   D_0: m²/s
    //   Diffusivity: m²/s
    //   E_a: J/mol
    //   R: J/mol·K
    //   T: K
    const double D_0 = 1.2e-9;
    const double E_a = 3.0e5;
    const double R = 8.314;

    Diffusivity =D_0*exp(-(E_a/(R*history_variable["Temperature"].getFinalValue())));
}
)";

    const std::string EXPECTED_GAS_H_CONTENT = R"(#ifndef GAS_H
#define GAS_H

#include <cmath>
#include "Material.h"
#include "Constants.h"
#include "ErrorMessages.h"
#include "SciantixArray.h"
#include "SciantixVariable.h"

class Gas : virtual public Material
{
public:
    double Diffusivity;
    double Pression;
    double Solubility;

    void setDiffusivity(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices);
    void setPression(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices);
    void setSolubility(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices);

    double getDiffusivity()
    {
        return Diffusivity;
    }

    double getPression()
    {
        return Pression;
    }

    double getSolubility()
    {
        return Solubility;
    }

    /**
     * @brief Constructor
     */
    Gas() {}

    /**
     * @brief Destructor
     */
    ~Gas() {}
}; // class Gas

#endif // GAS_H
)";

    const std::string EXPECTED_INTERPRETATION_REPORT_CONTENT = R"(For Diffusivity, Unit :m²/s,
- T was assumed as history_variable["Temperature"].getFinalValue(). Unit : K
- D_0 was assumed as a parameter. Unit: m²/s
- E_a was assumed as a parameter. Unit: J/mol
- R was assumed as a parameter. Unit: J/mol·K

For Porosity, Unit :W/mK,
- T was assumed as history_variable["Temperature"].getFinalValue(). Unit : K
- A was assumed as a parameter. Unit: mK/W
- B was assumed as a parameter. Unit: m/W

For Pression, Unit :Pa,
- T was assumed as history_variable["Temperature"].getFinalValue(). Unit : K
- A was assumed as a parameter. Unit: mol/m³
- B was assumed as a parameter. Unit: K
- R was assumed as a parameter. Unit: J/mol·K

For Solubility, Unit :mol/m³,
- T was assumed as history_variable["Temperature"].getFinalValue(). Unit : K
- E_s was assumed as a parameter. Unit: J/mol
- R was assumed as a parameter. Unit: J/mol·K
- S_0 was assumed as a parameter. Unit: mol/m³

For Thermal_conductivity, Unit :W/mK,
- T was assumed as history_variable["Temperature"].getFinalValue(). Unit : K
- A was assumed as a parameter. Unit: mK/W
- B was assumed as a parameter. Unit: m/W
)";

    SECTION("Diffusivity.cpp is generated correctly") {
        setup_standard_files();
        run_generate_outputs();
        fs::path actual_file_path = cpp_output_dir / "Diffusivity.cpp";
        REQUIRE(fs::exists(actual_file_path));
        std::string actual_content = read_file_content(actual_file_path);
        REQUIRE(normalize_string(actual_content) == normalize_string(EXPECTED_DIFFUSIVITY_CPP_CONTENT));
        clean_test_files({cpp_output_dir.string(), h_output_dir.string(), "output", temp_root.string()});
    }

    SECTION("Gas.h is generated correctly") {
        setup_standard_files();
        run_generate_outputs();
        fs::path actual_file_path = h_output_dir / "Gas.h";
        REQUIRE(fs::exists(actual_file_path));
        std::string actual_content = read_file_content(actual_file_path);
        REQUIRE(normalize_string(actual_content) == normalize_string(EXPECTED_GAS_H_CONTENT));
        clean_test_files({cpp_output_dir.string(), h_output_dir.string(), "output", temp_root.string()});
    }

    SECTION("interpretation_report.txt is generated correctly") {
        setup_standard_files();
        run_generate_outputs();
        fs::path actual_file_path = interpretation_report_file;
        REQUIRE(fs::exists(actual_file_path));
        std::string actual_content = read_file_content(actual_file_path);
        REQUIRE(normalize_string(actual_content) == normalize_string(EXPECTED_INTERPRETATION_REPORT_CONTENT));
        clean_test_files({cpp_output_dir.string(), h_output_dir.string(), "output", temp_root.string()});
    }

    SECTION("Empty correlations file") {
        fs::path empty_file = temp_input_dir / "empty.txt";
        fs::create_directories(temp_input_dir);
        std::ofstream ofs(empty_file);
        ofs.close();
        REQUIRE_NOTHROW(generateOutputsFromSelected(empty_file.string(), temp_variable_mapping_file.string()));
        REQUIRE(fs::exists(cpp_output_dir));
        REQUIRE(std::filesystem::is_empty(cpp_output_dir));
        clean_test_files({cpp_output_dir.string(), h_output_dir.string(), "output", temp_root.string()});
    }

    SECTION("File with only separators") {
        fs::path sep_file = temp_input_dir / "separators.txt";
        std::ofstream ofs(sep_file);
        ofs << "---\n---\n";
        ofs.close();
        REQUIRE_NOTHROW(generateOutputsFromSelected(sep_file.string(), temp_variable_mapping_file.string()));
        REQUIRE(fs::exists(cpp_output_dir));
        REQUIRE(std::filesystem::is_empty(cpp_output_dir));
        clean_test_files({cpp_output_dir.string(), h_output_dir.string(), "output", temp_root.string()});
    }

    SECTION("Malformed: property missing") {
        fs::path mal_file = temp_input_dir / "malformed.txt";
        std::ofstream ofs(mal_file);
        ofs << "Module: Gas\nVariables: T\nCoefficients: D_0=1\nUnits: T:K\n---\n";
        ofs.close();
        REQUIRE_NOTHROW(generateOutputsFromSelected(mal_file.string(), temp_variable_mapping_file.string()));
        REQUIRE(!fs::exists(cpp_output_dir / "Gas.cpp"));
        clean_test_files({cpp_output_dir.string(), h_output_dir.string(), "output", temp_root.string()});
    }

    SECTION("Malformed: coefficients without equals") {
        fs::path noeq_file = temp_input_dir / "coef_noeq.txt";
        std::ofstream ofs(noeq_file);
        ofs << "Property: Test\nModule: Gas\nVariables: X\nCoefficients: abc,def\nUnits: X:m\n---\n";
        ofs.close();
        REQUIRE_NOTHROW(generateOutputsFromSelected(noeq_file.string(), temp_variable_mapping_file.string()));
        clean_test_files({cpp_output_dir.string(), h_output_dir.string(), "output", temp_root.string()});
    }

    SECTION("Absent mapping file: variables are left as is") {
        fs::path nomap_file = temp_input_dir / "nomap.txt";
        std::ofstream ofs(nomap_file);
        ofs << "Property: Bar\nModule: Foo\nVariables: V\nCoefficients: C=1\nUnits: V:kg,Bar:m\n---\n";
        ofs.close();
        REQUIRE_NOTHROW(generateOutputsFromSelected(nomap_file.string(), "doesnotexist.txt"));
        clean_test_files({cpp_output_dir.string(), h_output_dir.string(), "output", temp_root.string()});
    }


}