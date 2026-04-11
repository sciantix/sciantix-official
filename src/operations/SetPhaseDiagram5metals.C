//////////////////////////////////////////////////////////////////////////////////////
//       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
//      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
//     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
//      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
//  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
//  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
//                                                                                  //
//  Originally developed by D. Pizzocri & T. Barani                                 //
//                                                                                  //
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, G. Zullo, G. Nicodemo                                     //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <map>     
#include <cmath>
#include <limits>
#include <algorithm>
#include <json/json.h>
#include <cctype>
#include <regex>
#include <set>
#include <exception>

namespace
{
struct OCPhaseData5metals
{
    double moles = 0.0;
    double volume = 0.0;
    std::map<std::string, double> elements;
};

struct OCOutputData5metals
{
    std::map<std::string, OCPhaseData5metals> solution_phases;
};

std::string trim(const std::string& input)
{
    const std::string whitespace = " \t\r\n";
    const size_t begin = input.find_first_not_of(whitespace);
    if (begin == std::string::npos) return "";

    const size_t end = input.find_last_not_of(whitespace);
    return input.substr(begin, end - begin + 1);
}

std::vector<std::string> split(const std::string& input)
{
    std::vector<std::string> parts;
    std::stringstream stream(input);
    std::string token;

    while (stream >> token) parts.push_back(token);
    return parts;
}

double safeFloat(const std::string& value)
{
    try
    {
        return std::stod(value);
    }
    catch (const std::exception&)
    {
        if (value.find('E') == std::string::npos && value.find('e') == std::string::npos)
        {
            const size_t exp_pos = value.find('-', 1);
            if (exp_pos != std::string::npos)
                return std::stod(value.substr(0, exp_pos) + "E-" + value.substr(exp_pos + 1));
        }

        throw;
    }
}

bool isNumericToken(const std::string& value)
{
    static const std::regex numeric_pattern(R"(^[+-]?(?:\d+\.?\d*|\.\d+)(?:[Ee][+-]?\d+)?$)");
    return std::regex_match(value, numeric_pattern);
}

std::string normalizeElementCase(const std::string& token, const std::vector<std::string>& valid_elements)
{
    std::map<std::string, std::string> valid_set;
    for (const auto& element : valid_elements)
    {
        std::string upper = element;
        std::transform(upper.begin(), upper.end(), upper.begin(), [](unsigned char c) { return std::toupper(c); });
        valid_set[upper] = element;
    }

    std::string result;
    size_t i = 0;

    while (i < token.size())
    {
        if (i + 2 <= token.size())
        {
            std::string candidate = token.substr(i, 2);
            std::transform(candidate.begin(), candidate.end(), candidate.begin(), [](unsigned char c) { return std::toupper(c); });

            const auto two_char_it = valid_set.find(candidate);
            if (two_char_it != valid_set.end())
            {
                result += two_char_it->second;
                i += 2;
                continue;
            }
        }

        std::string candidate(1, token[i]);
        std::transform(candidate.begin(), candidate.end(), candidate.begin(), [](unsigned char c) { return std::toupper(c); });
        const auto one_char_it = valid_set.find(candidate);
        if (one_char_it != valid_set.end())
        {
            result += one_char_it->second;
            ++i;
            continue;
        }

        result += token[i];
        ++i;
    }

    return result;
}

std::string normalizeName(const std::string& raw_name)
{
    std::string name = raw_name;

    while (!name.empty() && name.back() == '.')
        name.pop_back();

    const size_t hash_pos = name.find('#');
    if (hash_pos != std::string::npos)
        name = name.substr(0, hash_pos);

    const size_t auto_pos = name.find("_AUTO");
    if (auto_pos != std::string::npos)
        name = name.substr(0, auto_pos);

    const size_t chkd_pos = name.find("_CHKD");
    if (chkd_pos != std::string::npos)
        name = name.substr(0, chkd_pos);

    return trim(name);
}

std::string toLowerCopy(std::string text)
{
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return std::tolower(c); });
    return text;
}

std::string toUpperCopy(std::string text)
{
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return std::toupper(c); });
    return text;
}

bool isPhaseToSkip(const std::string& phase)
{
    const std::string normalized = toLowerCopy(phase);
    return normalized == "condensed" || normalized == "gas";
}

void setThermochemistryVariableForPhaseElement(
    SciantixArray<ThermochemistryVariable>& thermochemistry_variable,
    const std::string& element,
    const std::string& phase,
    const std::string& location,
    double value)
{
    std::vector<std::string> element_candidates = {
        element,
        toUpperCopy(element),
        toLowerCopy(element)
    };

    std::vector<std::string> phase_candidates = {
        phase,
        toLowerCopy(phase),
        toUpperCopy(phase)
    };

    for (const auto& element_candidate : element_candidates)
    {
        for (const auto& phase_candidate : phase_candidates)
        {
            const std::string variable_name =
                element_candidate + " (" + phase_candidate + ", " + location + ")";
            if (thermochemistry_variable.isElementPresent(variable_name))
            {
                thermochemistry_variable[variable_name].setFinalValue(value);
                return;
            }
        }
    }
}

OCOutputData5metals parseOCOutputFile5metals(
    const std::string& filepath,
    const std::vector<std::string>& valid_elements)
{
    std::ifstream file(filepath);
    if (!file)
    {
        std::cerr << "Error: Cannot open OPENCALPHAD output file: " << filepath << std::endl;
        return {};
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line))
        lines.push_back(line);

    OCOutputData5metals data;
    bool in_components_section = false;
    bool in_phases_section = false;
    bool in_constitution_section = false;
    std::string current_phase;

    for (size_t i = 0; i < lines.size(); ++i)
    {
        const std::string stripped = trim(lines[i]);

        if (stripped.find("Some data for components") != std::string::npos)
        {
            in_components_section = true;
            in_phases_section = false;
            continue;
        }

        if (in_components_section)
        {
            if (stripped.rfind("Component name", 0) == 0)
                continue;

            if (stripped.find("Some data for phases") != std::string::npos || stripped.empty())
            {
                in_components_section = false;
                in_phases_section = true;
                continue;
            }

            continue;
        }

        if (!in_phases_section)
            continue;

        if (stripped.rfind("Name", 0) == 0 || stripped.find("Moles") != std::string::npos)
            continue;

        if (stripped.find(".. E") != std::string::npos && stripped.find("X:") != std::string::npos)
        {
            in_constitution_section = false;
            const std::vector<std::string> parts = split(stripped);
            if (parts.size() < 4 || !isNumericToken(parts[2]) || !isNumericToken(parts[3]))
                continue;

            current_phase = normalizeName(parts[0]);

            OCPhaseData5metals& phase = data.solution_phases[current_phase];
            phase.moles += safeFloat(parts[2]);
            phase.volume += safeFloat(parts[3]);

            for (size_t j = i + 1; j < lines.size(); ++j)
            {
                const std::string next_line = trim(lines[j]);
                if (next_line.rfind("Constitution: There are", 0) == 0)
                {
                    in_constitution_section = true;
                    break;
                }

                if (next_line.find("Sublattice") != std::string::npos || next_line.empty())
                    break;

                const std::vector<std::string> element_parts = split(next_line);
                for (size_t k = 0; k + 1 < element_parts.size(); k += 2)
                {
                    if (!isNumericToken(element_parts[k + 1]))
                        continue;

                    const std::string element = normalizeElementCase(element_parts[k], valid_elements);
                    const double fraction = safeFloat(element_parts[k + 1]);

                    data.solution_phases[current_phase].elements[element] =
                        fraction * data.solution_phases[current_phase].moles;
                }
            }
        }
        else if (in_constitution_section)
        {
            const std::string next_line = stripped;
            if (next_line.empty() || next_line.rfind("Name", 0) == 0 || next_line.rfind("Some", 0) == 0 ||
                next_line.find("with") != std::string::npos)
            {
                in_constitution_section = false;
                continue;
            }

            const std::vector<std::string> species_parts = split(next_line);
            for (size_t k = 0; k + 1 < species_parts.size(); k += 2)
            {
                if (!isNumericToken(species_parts[k + 1]))
                    continue;

                const std::string species_name = normalizeName(species_parts[k]);
                const double mole_fraction = safeFloat(species_parts[k + 1]);
                if (data.solution_phases.find(current_phase) == data.solution_phases.end())
                    continue;

                const double species_moles = mole_fraction * data.solution_phases[current_phase].moles;

                // Species strings in many OC databases are simple element labels for metallic phases
                // (e.g. MO, PD, RH). Use them as an additional robust source for elemental moles.
                const std::string normalized_species = normalizeElementCase(species_name, valid_elements);
                if (std::find(valid_elements.begin(), valid_elements.end(), normalized_species) != valid_elements.end())
                {
                    data.solution_phases[current_phase].elements[normalized_species] += species_moles;
                }
            }
        }
    }

    return data;
}
} // namespace

/**
 * @brief Main entry point to set the thermodynamic phase diagram for Mo-Pd-Rh-Ru-Tc.
 * @param location The physical domain: "in grain", "at grain boundary", or "matrix".
 */

void Simulation::SetPhaseDiagram5metals(std::string location)
{
    // Skip if thermochemistry coupling is not enabled (iThermochimica < 2)
    if (input_variable["iThermochimica"].getValue() < 2) return;

    // Local variables for boundary conditions
    double temperature = history_variable["Temperature"].getFinalValue();
    double pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue() * scaling_factors["Dummy"].getValue();
    
    // Domain-specific checks
    if (location == "in grain" || location == "at grain boundary")
    {
        if (sciantix_variable["Grain radius"].getFinalValue() <= 0.0) return;
    }
    else if (location != "matrix")
    {
        std::cout << "Location not yet modelled: " << location << std::endl;
        return;
    }

    CallNobleMetalsModule(pressure, temperature, location, sciantix_variable);
}

/**
 * @brief Interfaces with OpenCalphad to calculate the chemical equilibrium of noble metals.
 */

void Simulation::CallNobleMetalsModule(double pressure, double temperature, std::string location, SciantixArray<SciantixVariable> &sciantix_variable)
{
    // 1. Load thermochemistry settings from JSON
    std::string thermochemistryJsonPath = "./input_thermochemistry.json";
    std::ifstream jsonFile(thermochemistryJsonPath);
    if (!jsonFile.is_open()) return;

    Json::Value root;
    jsonFile >> root;

    // Determine which settings block to use based on location
    std::string settingsBlock = (location == "matrix") ? "matrix" : "noble_metals";
    std::string module = root["Settings"][settingsBlock]["module"].asString();

    if (module == "OPENCALPHAD")
    {
        // Define paths for OpenCalphad interface
        std::string directoryPath = "./../../../opencalphad/";
        std::string ocmInputPath = directoryPath + "inputs/noble_metals.OCM";
        std::string datOutputPath = directoryPath + "outputs/noble_metals_out.DAT";
        std::string databasePath = "./../data/" + root["Settings"][settingsBlock]["database"].asString();
        
        // Ensure database extension is correct
        if (location != "matrix") databasePath += ".TDB";
        
        std::string ocExecutable = directoryPath + "oc6P " + ocmInputPath;
        
        // Retrieve element list from JSON settings
        Json::Value& elements = root["Settings"][settingsBlock]["elements"];
        std::vector<std::string> elementNames = elements.getMemberNames();

        // 2. Check if there is enough material to justify a thermodynamic call
        double total_moles = 0.0;
        for (auto& elementName : elementNames)
        {
            std::string sciantix_elem = elementName;
            if (sciantix_elem.length() > 1) sciantix_elem[1] = std::tolower(sciantix_elem[1]);

            double atomsAvailable = sciantix_variable[sciantix_elem + " produced"].getFinalValue();
            total_moles += atomsAvailable / avogadro_number;
        }

        // If total moles are negligible skip OpenCalphad
        if (total_moles < 1e-10) return;

        // 3. Generate OpenCalphad Command Macro (.OCM)
        std::ofstream inputFile(ocmInputPath);
        if (!inputFile) return;

        inputFile << "@$ Initialize variables:\n";
        inputFile << "r t " << databasePath << "\n\n\n\n"; // Skip database warnings
        inputFile << "set c t=" << temperature << "\n";
        inputFile << "set c p=" << pressure << "\n";

        std::cout << "[NobleMetals OC Input] location=" << location
                  << ", T=" << temperature
                  << ", P=" << pressure
                  << ", total_moles=" << total_moles << "\n";

        

        // OpenCalphad is fed with normalized element fractions to avoid issues with
        // very small absolute amounts. Real moles are reconstructed after parsing.
        std::map<std::string, double> normalizedFractions;
        double fractionSum = 0.0;
        for (auto& elementName : elementNames)
        {
            std::string sciantix_elem = elementName;
            if (sciantix_elem.length() > 1) sciantix_elem[1] = std::tolower(sciantix_elem[1]);

            double moles = sciantix_variable[sciantix_elem + " produced"].getFinalValue() / avogadro_number;
            double fraction = moles / total_moles;

            // Numerical safety: avoid absolute zero to prevent logarithm errors in OC.
            if (fraction < 1e-12) fraction = 1e-12;

            normalizedFractions[elementName] = fraction;
            fractionSum += fraction;
        }

        for (auto& elementName : elementNames)
        {
            double normalizedFraction = normalizedFractions[elementName] / fractionSum;
            inputFile << "set c n(" << elementName << ")=" << normalizedFraction << "\n";
            std::cout << "[NobleMetals OC Input] n(" << elementName << ")="
                      << normalizedFraction << " (fraction)\n";
        }

        inputFile << "c e\n"; // Calculate Equilibrium
        inputFile << "l /out=./../outputs/noble_metals_out.DAT r 1\n"; // Save output
        inputFile << "fin\n"; 
        inputFile.close();

        // 4. Run OpenCalphad
        if (std::system(ocExecutable.c_str()) != 0) 
        {
            std::cerr << "Error: OpenCalphad execution failed.\n";
            return;
        }

        // Print full OpenCalphad DAT output so it is captured in thermo.log.
        std::ifstream ocDatFile(datOutputPath);
        if (ocDatFile.is_open())
        {
            std::cout << "[OpenCalphad DAT Begin] " << datOutputPath << "\n";
            std::string ocLine;
            while (std::getline(ocDatFile, ocLine))
            {
                std::cout << ocLine << "\n";
            }
            std::cout << "[OpenCalphad DAT End]\n";
        }
        else
        {
            std::cout << "[OpenCalphad DAT] Unable to open " << datOutputPath << "\n";
        }

        // 5. Parse DAT output directly (same strategy as newparser OCOutputParser).
        const OCOutputData5metals parsed_output =
            parseOCOutputFile5metals(datOutputPath, elementNames);

        for (const auto& phase_entry : parsed_output.solution_phases)
        {
            const std::string& phase = phase_entry.first;
            const OCPhaseData5metals& phase_data = phase_entry.second;

            if (isPhaseToSkip(phase))
                continue;

            for (const auto& element_entry : phase_data.elements)
            {
                const std::string& element = element_entry.first;
                const double normalized_moles_in_phase = element_entry.second;
                const double moles_in_phase = normalized_moles_in_phase * total_moles;

                std::cout << "[NobleMetals OC Output] phase=" << phase
                          << ", element=" << element
                          << ", normalized=" << normalized_moles_in_phase
                          << ", real_moles=" << moles_in_phase << "\n";

                setThermochemistryVariableForPhaseElement(
                    thermochemistry_variable,
                    element,
                    phase,
                    location,
                    moles_in_phase);
            }
        }

        if (location == "matrix") return;

        // 6. Transition from total produced to reacted/unreacted atoms.
        for (auto& element : elementNames)
        {
            std::string sciantix_elem = element;
            if (sciantix_elem.length() > 1) sciantix_elem[1] = std::tolower(sciantix_elem[1]);

            // Sum element moles across all stable solution phases (BCC, HCP, SIGMA, etc.)
            double equilibrium_moles = 0.0;
            for (const auto& phase_entry : parsed_output.solution_phases)
            {
                const auto element_it = phase_entry.second.elements.find(element);
                if (element_it == phase_entry.second.elements.end())
                    continue;

                equilibrium_moles += element_it->second * total_moles;
            }

            double availableAtoms = sciantix_variable[sciantix_elem + " produced"].getFinalValue();
            double precipitatedAtoms = std::min(availableAtoms, equilibrium_moles * avogadro_number);

            if (location == "in grain") 
            {
                sciantix_variable[sciantix_elem + " reacted - IG"].setFinalValue(precipitatedAtoms);
                sciantix_variable[sciantix_elem + " in grain"].setFinalValue(std::max(0.0, availableAtoms - precipitatedAtoms));
            }            
            else if (location == "at grain boundary")
            {
                sciantix_variable[sciantix_elem + " reacted - GB"].setFinalValue(precipitatedAtoms);   
                sciantix_variable[sciantix_elem + " at grain boundary"].setFinalValue(std::max(0.0, availableAtoms - precipitatedAtoms));
            }
        }
    }
}
