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
//  Version: under development                                                      //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E.Cappellari                                    //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "OCUtilsCoupling.h"
#include "MainVariables.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <exception>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <regex>
#include <set>
#include <sstream>
#include <vector>
#include <sys/wait.h>

namespace
{
std::string trim(const std::string& input)
{
    const std::string whitespace = " \t\r\n";
    const size_t      begin      = input.find_first_not_of(whitespace);
    if (begin == std::string::npos)
        return "";

    const size_t end = input.find_last_not_of(whitespace);
    return input.substr(begin, end - begin + 1);
}

std::vector<std::string> split(const std::string& input)
{
    std::vector<std::string> parts;
    std::stringstream        stream(input);
    std::string              token;

    while (stream >> token)
        parts.push_back(token);

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
    size_t      i = 0;

    while (i < token.size())
    {
        if (i + 2 <= token.size())
        {
            std::string candidate = token.substr(i, 2);
            std::transform(candidate.begin(), candidate.end(), candidate.begin(), [](unsigned char c) { return std::toupper(c); });

            const auto it = valid_set.find(candidate);
            if (it != valid_set.end())
            {
                result += it->second;
                i += 2;
                continue;
            }
        }

        std::string candidate(1, token[i]);
        std::transform(candidate.begin(), candidate.end(), candidate.begin(), [](unsigned char c) { return std::toupper(c); });
        const auto it = valid_set.find(candidate);
        if (it != valid_set.end())
        {
            result += it->second;
            ++i;
            continue;
        }

        result += token[i];
        ++i;
    }

    return result;
}

std::string normalizePhaseName(const std::string& raw_phase)
{
    std::string phase = raw_phase;

    while (!phase.empty() && phase.back() == '.')
        phase.pop_back();

    const size_t hash_pos = phase.find('#');
    if (hash_pos != std::string::npos)
        phase = phase.substr(0, hash_pos);

    const size_t auto_pos = phase.find("_AUTO");
    if (auto_pos != std::string::npos)
        phase = phase.substr(0, auto_pos);

    const size_t chkd_pos = phase.find("_CHKD");
    if (chkd_pos != std::string::npos)
        phase = phase.substr(0, chkd_pos);

    phase = trim(phase);

    static const std::set<std::string> passthrough_phases = {
        "GAS", "LIQUID", "LIQUID_IONIC", "IONIC_LIQUID", "PURE_CONDENSED", "SOLID"
    };

    if (passthrough_phases.count(phase) > 0)
    {
        std::string lower = phase;
        std::transform(lower.begin(), lower.end(), lower.begin(), [](unsigned char c) { return std::tolower(c); });
        return lower;
    }

    return "condensed";
}

std::string normalizeSpeciesName(const std::string& raw_name)
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
}  // namespace

OCOutputData parseOCOutputFile(const std::string& filepath, const std::vector<std::string>& valid_elements)
{
    std::ifstream file(filepath);
    if (!file)
    {
        std::cerr << "Error: Cannot open OPENCALPHAD output file: " << filepath << std::endl;
        exit(1);
    }

    std::vector<std::string> lines;
    std::string              line;
    while (std::getline(file, line))
        lines.push_back(line);

    OCOutputData data;

    bool        in_components_section   = false;
    bool        in_phases_section       = false;
    bool        in_constitution_section = false;
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

            const std::vector<std::string> parts = split(stripped);
            if (parts.size() < 5)
                continue;

            OCComponentData* component = nullptr;
            try
            {
                component = &data.components[normalizeElementCase(parts[0], valid_elements)];
                component->moles = safeFloat(parts[1]);
                component->mole_fraction = safeFloat(parts[2]);
                component->chemical_potential_over_rt = safeFloat(parts[3]);
                component->activity = safeFloat(parts[4]);
            }
            catch (const std::exception&)
            {
                continue;
            }

            if (parts.size() > 5)
            {
                std::ostringstream reference_state;
                for (size_t token_index = 5; token_index < parts.size(); ++token_index)
                {
                    if (token_index > 5)
                        reference_state << " ";
                    reference_state << parts[token_index];
                }
                component->reference_state = reference_state.str();
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

            const std::string raw_phase_name = normalizeSpeciesName(parts[0]);
            current_phase = normalizePhaseName(raw_phase_name);

            if (current_phase != "condensed")
            {
                OCPhaseData& phase = data.solution_phases[current_phase];
                phase.moles += safeFloat(parts[2]);
                phase.volume += safeFloat(parts[3]);
            }
            else
            {
                const std::string species_name = normalizeSpeciesName(raw_phase_name);
                OCPhaseData&      phase        = data.solution_phases[current_phase];
                OCSpeciesData&    species      = phase.species[species_name];
                const double      moles        = safeFloat(parts[2]);
                const double      volume       = safeFloat(parts[3]);
                const double      stoichiometric_size =
                    (parts.size() > 5 && isNumericToken(parts[5])) ? safeFloat(parts[5]) : 1.0;
                
                species.moles += moles;
                species.volume += volume;
                species.stoichiometric_size = (stoichiometric_size > 0.0) ? stoichiometric_size : 1.0;
                phase.moles += moles;
                phase.volume += volume;
            }

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
                    const double      fraction = safeFloat(element_parts[k + 1]);

                    if (current_phase != "condensed")
                    {
                        data.solution_phases[current_phase].elements[element] =
                            fraction * data.solution_phases[current_phase].moles;
                    }
                    else
                    {
                        const std::string species_name = normalizeSpeciesName(raw_phase_name);
                        OCPhaseData&      phase        = data.solution_phases[current_phase];
                        OCSpeciesData&    species      = phase.species[species_name];
                        const double      element_moles = fraction * species.moles * species.stoichiometric_size;

                        species.elements[element] = element_moles;
                        phase.elements[element] += element_moles;
                    }
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
                // Keep explicit constituent parsing only for gas.
                if (current_phase == "condensed" || current_phase == "liquid" || current_phase == "ionic_liquid")
                    continue;

                if (!isNumericToken(species_parts[k + 1]))
                    continue;

                const std::string species_name = normalizeSpeciesName(species_parts[k]);
                const double      mole_fraction = safeFloat(species_parts[k + 1]);
                OCPhaseData&      phase         = data.solution_phases[current_phase];
                OCSpeciesData&    species       = phase.species[species_name];
                species.moles = mole_fraction * phase.moles;
            }
        }
    }

    return data;
}

namespace OCUtilsCoupling
{
std::string solveModeLabel(OpenCalphadSolveMode mode)
{
    switch (mode)
    {
        case OpenCalphadSolveMode::SaveReadWarmStart:
            return "WARM-START";
        case OpenCalphadSolveMode::GlobalEquilibrium:
            return "COLD-START";
        case OpenCalphadSolveMode::PressureAxisStepGlobalEquilibrium:
            return "PRESSURE-SWEEP";
        case OpenCalphadSolveMode::FixedOxygenMolesFromInvalidPotentialSolve:
            return "OXYGEN-MOLES-FIXED";
        case OpenCalphadSolveMode::OnlyC1MO2:
            return "ONLY C1MO2 ENTERED";
    }

    return "unknown";
}

std::string buildSolveCommandBlock(OpenCalphadSolveMode mode,
                                   double               pressure,
                                   double               fixed_oxygen_moles)
{
    std::ostringstream commands;
    commands << std::setprecision(16);

    if (mode == OpenCalphadSolveMode::PressureAxisStepGlobalEquilibrium)
    {
        const double start_pressure = 1.0e5;
        commands << "set c p=" << start_pressure << "\n";
        commands << "c w\n";
        commands << "set axis\n";
        commands << "1\n";
        commands << "p\n";
        commands << start_pressure << "\n";
        commands << pressure << "\n";
        commands << "\n\n";
        commands << "step\n";
        commands << "normal\n\n";
        commands << "set c p=" << pressure << "\n";
    }
    else if (mode == OpenCalphadSolveMode::FixedOxygenMolesFromInvalidPotentialSolve)
    {
        commands << "set c n(o)=" << fixed_oxygen_moles << "\n";
        commands << "set c mu(o)=none\n\n";
    }
    else if (mode == OpenCalphadSolveMode::OnlyC1MO2)
    {
        commands << "set st ph gas=fix 0\n";
        commands << "set st ph *=dor\n";
        commands << "set st ph gas=e 1\n";
        commands << "c e\n";
        commands << "set st ph C1_MO2=e 1\n";
    }
    commands << "c e\nc w\n";

    return commands.str();
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

std::string stripTdbExtension(const std::string& database_name)
{
    if (database_name.size() >= 4)
    {
        const std::string suffix = database_name.substr(database_name.size() - 4);
        if (suffix == ".TDB" || suffix == ".tdb")
            return database_name.substr(0, database_name.size() - 4);
    }

    return database_name;
}

std::string readTextFile(const std::string& file_path)
{
    std::ifstream file(file_path);
    if (!file)
        return "<unable to open file>";

    std::ostringstream content;
    content << file.rdbuf();
    return content.str();
}

bool writeTextFile(const std::string& file_path, const std::string& content)
{
    std::ofstream file(file_path);
    if (!file)
        return false;

    file << content;
    return file.good();
}

bool fileExists(const std::string& file_path)
{
    std::ifstream file(file_path);
    return static_cast<bool>(file);
}

bool hasOpenCalphadSavedState(const std::string& state_file_path)
{
    return fileExists(state_file_path) ||
           fileExists(state_file_path + ".OCU") ||
           fileExists(state_file_path + ".ocu");
}

bool hasInvalidEquilibriumResult(const std::string& output_text)
{
    return output_text.find("not a valid equilibrium as last calculation failed") != std::string::npos ||
           output_text.find("No results as no equilibrium calculated") != std::string::npos ||
           output_text.find("*** The results listed below may be inconsistent with the current conditions") != std::string::npos;
}

bool tryGetOxygenMolesFromOutput(const std::string& output_file_path,
                                 const std::set<std::string>& active_elements,
                                 double& oxygen_moles)
{
    std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());
    if (std::find(valid_elements.begin(), valid_elements.end(), "O") == valid_elements.end())
        valid_elements.push_back("O");

    const OCOutputData parsed_output = parseOCOutputFile(output_file_path, valid_elements);
    const auto oxygen_component = parsed_output.components.find("O");
    if (oxygen_component == parsed_output.components.end())
        return false;

    if (oxygen_component->second.moles <= 0.0 || !std::isfinite(oxygen_component->second.moles))
        return false;

    oxygen_moles = oxygen_component->second.moles;
    return true;
}

void dumpParsedOcOutput(const OCOutputData& output_data)
{
    std::cout << "\n[OC parser] Parsed components" << std::endl;
    if (output_data.components.empty())
        std::cout << "  <none>" << std::endl;
    else
    {
        for (const auto& component_entry : output_data.components)
        {
            const auto& name = component_entry.first;
            const auto& data = component_entry.second;
            std::cout << "  " << name
                      << " : moles=" << data.moles
                      << ", x=" << data.mole_fraction
                      << ", mu/RT=" << data.chemical_potential_over_rt
                      << ", activity=" << data.activity
                      << ", ref=" << data.reference_state
                      << std::endl;
        }
    }

    std::cout << "\n[OC parser] Parsed phases" << std::endl;
    if (output_data.solution_phases.empty())
    {
        std::cout << "  <none>" << std::endl;
        return;
    }

    for (const auto& phase_entry : output_data.solution_phases)
    {
        const auto& phase_name = phase_entry.first;
        const auto& phase_data = phase_entry.second;

        std::cout << "  Phase " << phase_name
                  << " : moles=" << phase_data.moles
                  << ", volume=" << phase_data.volume
                  << std::endl;

        if (!phase_data.elements.empty())
        {
            std::cout << "    Elements" << std::endl;
            for (const auto& element_entry : phase_data.elements)
            {
                std::cout << "      " << element_entry.first
                          << " = " << element_entry.second
                          << std::endl;
            }
        }

        if (!phase_data.species.empty())
        {
            std::cout << "    Species" << std::endl;
            for (const auto& species_entry : phase_data.species)
            {
                const auto& species_name = species_entry.first;
                const auto& species_data = species_entry.second;
                std::cout << "      " << species_name
                          << " : moles=" << species_data.moles
                          << ", volume=" << species_data.volume
                          << std::endl;

                for (const auto& element_entry : species_data.elements)
                {
                    std::cout << "        " << element_entry.first
                              << " = " << element_entry.second
                              << std::endl;
                }
            }
        }
    }
}

struct OpenCalphadInputComponent
{
    std::string name;
    double      content  = 0.0;
    double      fraction = 0.0;
};

std::vector<OpenCalphadInputComponent> buildOpenCalphadInputComponents(
    const std::set<std::string>&     selected_elements,
    SciantixArray<SciantixVariable>& sciantix_variable,
    double&                          total_content)
{
    constexpr double open_calphad_component_fraction_cutoff = 1.0e-8;
    std::vector<OpenCalphadInputComponent> components;
    total_content = 0.0;

    for (const auto& element_name : selected_elements)
    {
        OpenCalphadInputComponent component;
        component.name = element_name;
        if (element_name == "O" || element_name == "U" || element_name == "Pu")
            component.content = std::max(0.0, sciantix_variable[element_name + " content"].getFinalValue());
        else
        {
            const double atoms_available =
                sciantix_variable[element_name + " produced"].getFinalValue() -
                sciantix_variable[element_name + " decayed"].getFinalValue() -
                sciantix_variable[element_name + " in grain"].getFinalValue() -
                sciantix_variable[element_name + " released"].getInitialValue();

            component.content = std::max(0.0, atoms_available / avogadro_number);
        }

        if (component.content > 0.0)
        {
            total_content += component.content;
            components.push_back(component);
        }
    }

    if (total_content <= 0.0 || components.empty())
        return components;

    for (auto& component : components)
        component.fraction = component.content / total_content;

    components.erase(
        std::remove_if(
            components.begin(),
            components.end(),
            [](const OpenCalphadInputComponent& component)
            {
                return component.fraction < open_calphad_component_fraction_cutoff;
            }),
        components.end());

    total_content = 0.0;
    for (const auto& component : components)
        total_content += component.content;

    if (total_content <= 0.0 || components.empty())
        return components;

    for (auto& component : components)
        component.fraction = component.content / total_content;

    return components;
}

bool writeOpenCalphadInput(const std::string& input_file_path,
                           const std::string& output_file_path,
                           const std::string& state_file_path,
                           const std::string& data_path,
                           double             pressure,
                           double             temperature,
                           OpenCalphadSolveMode solve_mode,
                           const std::string& location,
                           const std::set<std::string>& selected_elements,
                           SciantixArray<SciantixVariable>& sciantix_variable,
                           std::set<std::string>& active_elements,
                           double&                total_input_content,
                           double                 fixed_oxygen_moles)
{
    (void)output_file_path;
    (void)location;
    std::ofstream input_file(input_file_path);
    if (!input_file)
    {
        std::cerr << "Error: Cannot create input file: " << input_file_path << std::endl;
        return false;
    }

    const bool use_oxygen_potential = (selected_elements.count("O") > 0 && 
                                        selected_elements.count("U") == 0 &&
                                        selected_elements.count("Pu") == 0);
                                        
    const double oxygen_potential_j_per_mol =
        sciantix_variable["Fuel oxygen potential"].getFinalValue() * 1.0e3 / 2.0; // Convert from kJ/mol O2 to J/mol O

    std::vector<OpenCalphadInputComponent> solve_components =
        buildOpenCalphadInputComponents(selected_elements, sciantix_variable, total_input_content);

    active_elements.clear();
    for (const auto& component : solve_components)
        active_elements.insert(component.name);

    if (solve_components.empty() || total_input_content <= 0.0)
        return false;

    const bool use_saved_state =
        (solve_mode == OpenCalphadSolveMode::SaveReadWarmStart) &&
        hasOpenCalphadSavedState(state_file_path);

    if (use_saved_state)
        input_file << "r u " << state_file_path << ".OCU\n\n";
    else
    {
        input_file << "@$ Initialize variables:\n";
        input_file << "r t " << data_path;
        for (const auto& component : solve_components)
            input_file << " " << toLowerCopy(component.name);
        input_file << "\n\n";
    }

    input_file << "set ref o gas * " << reference_oxygen_pressure_bar * 1.0e6 << "\n\n";
    input_file << "set c t=" << temperature << "\n";
    input_file << "set c p=" << pressure << "\n";
    for (const auto& component : solve_components)
        input_file << "set c n(" << toLowerCopy(component.name) << ")=" << component.fraction << "\n";
    input_file << "c e\n";
    if (use_oxygen_potential &&
        solve_mode != OpenCalphadSolveMode::FixedOxygenMolesFromInvalidPotentialSolve)
    {
        input_file << "set c n(o)=none\n";
        input_file << "set c mu(o)=" << oxygen_potential_j_per_mol << "\n\n";
    }

    if (solve_mode == OpenCalphadSolveMode::SaveReadWarmStart)
    {
        input_file << "c w\n\n";
        input_file << "save u " << state_file_path << " Y\n\n";
    }
    else
    {
        input_file << buildSolveCommandBlock(
            solve_mode,
            pressure,
            fixed_oxygen_moles);
    }

    input_file << "l /out=" << output_file_path << " r 1\n\n";
    input_file << "fin";
    return true;
}

bool runOpenCalphadCase(const std::string& input_file_path,
                        const std::string& output_file_path,
                        const std::string& executable,
                        std::string&       raw_output,
                        int                timeout_seconds)
{
    std::cout << "\n[OC input] " << input_file_path << std::endl;
    std::cout << "----------------------------------------" << std::endl;
    std::cout << readTextFile(input_file_path) << std::endl;
    std::cout << "----------------------------------------" << std::endl;

    const std::string command =
        "timeout --signal=TERM " + std::to_string(timeout_seconds) + "s " + executable;
    const int status = std::system(command.c_str());
    if (status != 0)
    {
        if (WIFEXITED(status) && WEXITSTATUS(status) == 124)
        {
            std::cerr << "Warning: OpenCalphad timed out after "
                      << timeout_seconds
                      << " s."
                      << std::endl;
        }
        else
        {
            std::cerr << "Error: Execution of OPENCALPHAD failed." << std::endl;
        }
        return false;
    }

    std::cout << "\n[OC output] " << output_file_path << std::endl;
    std::cout << "----------------------------------------" << std::endl;
    raw_output = readTextFile(output_file_path);
    std::cout << raw_output << std::endl;
    std::cout << "----------------------------------------" << std::endl;

    return true;
}

bool hasRequiredComponents(const OCOutputData& output_data,
                           const std::set<std::string>& active_elements)
{
    for (const auto& component : active_elements)
    {
        if (output_data.components.find(component) == output_data.components.end())
            return false;
    }
    return true;
}

void updateThermochemistryVariablesFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                              const std::string&                         location,
                                              double                                     content_scaling_factor,
                                              SciantixArray<ThermochemistryVariable>&    thermochemistry_variable)
{
    static std::set<std::string> logged_thermochemistry_variables;

    auto formatComposition = [](const std::map<std::string, double>& composition)
    {
        if (composition.empty())
            return std::string("<empty>");

        std::ostringstream stream;
        stream << std::setprecision(8);
        bool first = true;
        for (const auto& entry : composition)
        {
            if (!first)
                stream << ",";
            stream << entry.first << ":" << entry.second;
            first = false;
        }
        return stream.str();
    };

    auto logThermochemistryVariable = [&](const std::string& variable_name)
    {
        if (!thermochemistry_variable.isElementPresent(variable_name))
            return;
        if (!logged_thermochemistry_variables.insert(variable_name).second)
            return;

        const std::map<std::string, double> composition = thermochemistry_variable[variable_name].getComposition();
        const std::string composition_label = !composition.empty() ? formatComposition(composition) : "<empty>";

        std::cout << "[Thermochemistry] " << variable_name
                  << " composition=" << composition_label
                  << " molar_mass=" << thermochemistry_variable[variable_name].getMolarMass()
                  << " g/mol"
                  << std::endl;
    };

    auto computeNormalizedPhaseComposition = [&](const OCPhaseData& phase_data)
    {
        std::map<std::string, double> composition;
        double total_element_moles = 0.0;
        for (const auto& element_entry : phase_data.elements)
            total_element_moles += std::max(0.0, element_entry.second);

        if (total_element_moles <= 0.0)
            return composition;

        for (const auto& element_entry : phase_data.elements)
            composition[element_entry.first] = std::max(0.0, element_entry.second) / total_element_moles;

        return composition;
    };

    auto computeCompositionMolarMass = [](const std::map<std::string, double>& composition)
    {
        static const std::map<std::string, double> atomic_masses = {
            {"Cs", 132.90545196},
            {"Cr", 51.9961},
            {"I", 126.90447},
            {"Mo", 95.95},
            {"O", 15.999},
            {"Te", 127.60},
            {"U", 238.02891},
            {"Pu", 239.052},
            {"Va", 0.0}
        };

        double molar_mass = 0.0;
        for (const auto& entry : composition)
        {
            const auto it = atomic_masses.find(entry.first);
            if (it == atomic_masses.end())
                continue;
            molar_mass += entry.second * it->second;
        }
        return molar_mass;
    };

    for (const auto& phase_entry : solution_phases)
    {
        const std::string& phase_name = phase_entry.first;
        const OCPhaseData& phase_data = phase_entry.second;
        const bool is_single_phase_liquid = (phase_name == "liquid" || phase_name == "ionic_liquid");
        const std::map<std::string, double> liquid_phase_composition =
            is_single_phase_liquid ? computeNormalizedPhaseComposition(phase_data) : std::map<std::string, double>{};
        const std::string liquid_phase_variable_name = "LIQUID (" + phase_name + ", " + location + ")";

        if (is_single_phase_liquid && thermochemistry_variable.isElementPresent(liquid_phase_variable_name))
        {
            thermochemistry_variable[liquid_phase_variable_name].setFinalValue(
                phase_data.moles * content_scaling_factor);
            if (!liquid_phase_composition.empty())
                thermochemistry_variable[liquid_phase_variable_name].setComposition(liquid_phase_composition);

            logThermochemistryVariable(liquid_phase_variable_name);

            if (!liquid_phase_composition.empty())
            {
                std::ostringstream phase_stream;
                phase_stream << std::setprecision(8);
                bool first = true;
                for (const auto& entry : liquid_phase_composition)
                {
                    if (!first)
                        phase_stream << ",";
                    phase_stream << entry.first << ":" << entry.second;
                    first = false;
                }
                const double phase_molar_mass = computeCompositionMolarMass(liquid_phase_composition);
                std::cout << "[ThermochemistryPhase] " << phase_name << " (" << location
                          << ") composition=" << phase_stream.str()
                          << " molar_mass=" << phase_molar_mass
                          << " g/mol" << std::endl;
            }

            continue;
        }

        if (!phase_data.species.empty())
        {
            for (const auto& species_entry : phase_data.species)
            {
                const std::string variable_name =
                    species_entry.first + " (" + phase_name + ", " + location + ")";

                if (thermochemistry_variable.isElementPresent(variable_name))
                {
                    thermochemistry_variable[variable_name].setFinalValue(
                        species_entry.second.moles * content_scaling_factor);
                    std::map<std::string, double> composition;
                    if (species_entry.second.moles > 0.0)
                    {
                        for (const auto& element_entry : species_entry.second.elements)
                            composition[element_entry.first] = element_entry.second / species_entry.second.moles;
                    }
                    thermochemistry_variable[variable_name].setComposition(composition);
                    logThermochemistryVariable(variable_name);
                }
            }

            for (const auto& element_entry : phase_data.elements)
            {
                const std::string variable_name = element_entry.first + " (" + phase_name + ", " + location + ")";
                const std::string uppercase_variable_name =
                    toUpperCopy(element_entry.first) + " (" + phase_name + ", " + location + ")";
                const bool has_variable = thermochemistry_variable.isElementPresent(variable_name);
                const bool has_uppercase_variable =
                    thermochemistry_variable.isElementPresent(uppercase_variable_name);

                if (has_variable)
                {
                    thermochemistry_variable[variable_name].setFinalValue(
                        element_entry.second * content_scaling_factor);
                    thermochemistry_variable[variable_name].setComposition({{element_entry.first, 1.0}});
                    logThermochemistryVariable(variable_name);
                }
                else if (has_uppercase_variable)
                {
                    thermochemistry_variable[uppercase_variable_name].setFinalValue(
                        element_entry.second * content_scaling_factor);
                    thermochemistry_variable[uppercase_variable_name].setComposition({{element_entry.first, 1.0}});
                    logThermochemistryVariable(uppercase_variable_name);
                }
            }
            continue;
        }

        for (const auto& element_entry : phase_data.elements)
        {
            const std::string variable_name = element_entry.first + " (" + phase_name + ", " + location + ")";
            const std::string uppercase_variable_name =
                toUpperCopy(element_entry.first) + " (" + phase_name + ", " + location + ")";

            if (thermochemistry_variable.isElementPresent(variable_name))
            {
                thermochemistry_variable[variable_name].setFinalValue(
                    element_entry.second * content_scaling_factor);
                if (!liquid_phase_composition.empty())
                    thermochemistry_variable[variable_name].setComposition(liquid_phase_composition);
                else
                    thermochemistry_variable[variable_name].setComposition({{element_entry.first, 1.0}});
                logThermochemistryVariable(variable_name);
            }
            else if (thermochemistry_variable.isElementPresent(uppercase_variable_name))
            {
                thermochemistry_variable[uppercase_variable_name].setFinalValue(
                    element_entry.second * content_scaling_factor);
                if (!liquid_phase_composition.empty())
                    thermochemistry_variable[uppercase_variable_name].setComposition(liquid_phase_composition);
                else
                    thermochemistry_variable[uppercase_variable_name].setComposition({{element_entry.first, 1.0}});
                logThermochemistryVariable(uppercase_variable_name);
            }
        }

        if (is_single_phase_liquid && !liquid_phase_composition.empty())
        {
            std::ostringstream phase_stream;
            phase_stream << std::setprecision(8);
            bool first = true;
            for (const auto& entry : liquid_phase_composition)
            {
                if (!first)
                    phase_stream << ",";
                phase_stream << entry.first << ":" << entry.second;
                first = false;
            }
            const double phase_molar_mass = computeCompositionMolarMass(liquid_phase_composition);
            std::cout << "[ThermochemistryPhase] " << phase_name << " (" << location
                      << ") composition=" << phase_stream.str()
                      << " molar_mass=" << phase_molar_mass
                      << " g/mol" << std::endl;
        }
    }
}

void updateMatrixFromOutput(const OCOutputData&              output_data,
                            double                           pressure,
                            double                           temperature,
                            SciantixArray<SciantixVariable>& sciantix_variable,
                            SciantixArray<Matrix>&           matrices)
{
    (void)pressure;
    (void)matrices;
    const auto oxygen_component = output_data.components.find("O");
    double calphad_oxygen_potential(0.0), calphad_oxygen_partial_pressure(0.0);
    if (oxygen_component != output_data.components.end())
    {
        calphad_oxygen_potential =
            2.0 * oxygen_component->second.chemical_potential_over_rt * gas_constant * temperature * 1.0e-3;
        calphad_oxygen_partial_pressure =
            reference_oxygen_pressure_bar * oxygen_component->second.activity * oxygen_component->second.activity;
    }

    sciantix_variable["Fuel oxygen partial pressure - CALPHAD"].setFinalValue(calphad_oxygen_partial_pressure);
    sciantix_variable["Fuel oxygen potential - CALPHAD"].setFinalValue(calphad_oxygen_potential);

    if (calphad_oxygen_partial_pressure > 0.0)
    {
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(calphad_oxygen_partial_pressure);
        sciantix_variable["Fuel oxygen potential"].setFinalValue(calphad_oxygen_potential);
    }
}

void updateGrainBoundaryFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                   const std::set<std::string>&               selected_elements,
                                   double                                     content_scaling_factor,
                                   SciantixArray<SciantixVariable>&           sciantix_variable)
{
    const auto gas_phase = solution_phases.find("gas");

    for (const auto& element : selected_elements)
    {
        if (element == "O" || element == "U" || element == "Pu")
            continue;

        double gas_moles = 0.0;
        if (gas_phase != solution_phases.end() && gas_phase->second.elements.count(element) > 0)
            gas_moles = gas_phase->second.elements.at(element) * content_scaling_factor;

        const double available =
            sciantix_variable[element + " produced"].getFinalValue() -
            sciantix_variable[element + " decayed"].getFinalValue() -
            sciantix_variable[element + " in grain"].getFinalValue() -
            sciantix_variable[element + " released"].getInitialValue();

        const double updated_atoms = std::min(available, gas_moles * avogadro_number);
        sciantix_variable[element + " at grain boundary"].setFinalValue(updated_atoms);
        sciantix_variable[element + " reacted - GB"].setFinalValue(available - updated_atoms);
    }
}
}  // namespace OCUtilsCoupling
