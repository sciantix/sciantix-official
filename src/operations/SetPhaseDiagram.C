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
#include "MainVariables.h"
#include "OCOutputParser.h"
#include "ThermochemistryManifest.h"
#include "ThermochemistrySettings.h"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <sys/wait.h>
#include <vector>

namespace
{
constexpr double open_calphad_component_fraction_cutoff = 1.0e-8;
constexpr int open_calphad_timeout_seconds = 60;

enum class OpenCalphadSolveMode
{
    SaveReadWarmStart,
    GlobalEquilibrium,
    PressureAxisStepGlobalEquilibrium,
    TemperatureAxisStepGlobalEquilibrium,
    CalculateCarefully,
    NoGlobalFallback,
    WithCheckAfterFallback,
    RoundedInputGlobalEquilibrium,
};

struct OpenCalphadInputComponent
{
    std::string name;
    double      content  = 0.0;
    double      fraction = 0.0;
};

bool isGrainBoundaryLocation(const std::string& location)
{
    return location == "at grain boundary";
}

bool isMatrixLocation(const std::string& location)
{
    return location == "matrix";
}

bool useOxygenPotentialConstraint(const std::set<std::string>& manifest_elements)
{
    return manifest_elements.count("O") > 0 && manifest_elements.count("U") == 0;
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

std::string buildOpenCalphadLocationTag(const std::string& location)
{
    if (isMatrixLocation(location))
        return "matrix";

    if (isGrainBoundaryLocation(location))
        return "grain_boundary";

    return "unknown";
}

bool hasInvalidEquilibriumResult(const std::string& output_text)
{
    return output_text.find("not a valid equilibrium as last calculation failed") != std::string::npos ||
           output_text.find("No results as no equilibrium calculated") != std::string::npos;
}

std::string solveModeLabel(OpenCalphadSolveMode mode)
{
    switch (mode)
    {
        case OpenCalphadSolveMode::SaveReadWarmStart:
            return "save/read warm-start (first: c e, then: r u + c w)";
        case OpenCalphadSolveMode::GlobalEquilibrium:
            return "c e";
        case OpenCalphadSolveMode::PressureAxisStepGlobalEquilibrium:
            return "set axis(P) + step + c e";
        case OpenCalphadSolveMode::TemperatureAxisStepGlobalEquilibrium:
            return "set axis(T) + step + c e";
        case OpenCalphadSolveMode::NoGlobalFallback:
            return "c n";
        case OpenCalphadSolveMode::CalculateCarefully:
            return "c c";
        case OpenCalphadSolveMode::WithCheckAfterFallback:
            return "c w";
        case OpenCalphadSolveMode::RoundedInputGlobalEquilibrium:
            return "c e with rounded n(element)";
    }

    return "unknown";
}

double buildPressureStepIncrement(double target_pressure)
{
    const double start_pressure = 1.0e5;
    if (target_pressure <= 0.0)
        return 0.0;

    if (target_pressure <= start_pressure)
        return 0.0;

    const double pressure_span = target_pressure - start_pressure;
    const double minimum_increment = 2.25e4;
    const double target_increment = pressure_span / 20.0;

    return std::min(pressure_span, std::max(minimum_increment, target_increment));
}

double buildTemperatureStepIncrement(double target_temperature)
{
    const double start_temperature = 300.0;
    if (target_temperature <= 0.0)
        return 0.0;

    if (target_temperature <= start_temperature)
        return 0.0;

    const double temperature_span = target_temperature - start_temperature;
    const double minimum_increment = 20.0;
    const double target_increment = temperature_span / 20.0;

    return std::min(temperature_span, std::max(minimum_increment, target_increment));
}

std::string buildSolveCommandBlock(OpenCalphadSolveMode mode, double temperature, double pressure)
{
    std::ostringstream commands;
    commands << std::setprecision(16);

    if (mode == OpenCalphadSolveMode::PressureAxisStepGlobalEquilibrium)
    {
        const double start_pressure = 1.0e5;
        const double pressure_increment = buildPressureStepIncrement(pressure);

        commands << "set c p=" << start_pressure << "\n\n";
        commands << "c e\n\n";
        commands << "set axis\n";
        commands << "1\n";
        commands << "p\n";
        commands << start_pressure << "\n";
        commands << pressure << "\n";
        commands << pressure_increment << "\n\n";
        commands << "step\n";
        commands << "normal\n\n";
        commands << "set c p=" << pressure << "\n\n";
        commands << "c c\n\n";
    }
    else if (mode == OpenCalphadSolveMode::TemperatureAxisStepGlobalEquilibrium)
    {
        const double start_temperature = 300.0;
        const double temperature_increment = buildTemperatureStepIncrement(temperature);

        commands << "set c t=" << start_temperature << "\n\n";
        commands << "c c\n\n";
        commands << "set axis\n";
        commands << "1\n";
        commands << "t\n";
        commands << start_temperature << "\n";
        commands << temperature << "\n";
        commands << temperature_increment << "\n\n";
        commands << "step\n";
        commands << "normal\n\n";
        commands << "set c t=" << temperature << "\n\n";
        commands << "c c\n\n";
    }
    else if (mode == OpenCalphadSolveMode::NoGlobalFallback)    
    {
        commands << "c e\n\n";
        commands << "c n\n\n";
    }
    else if (mode == OpenCalphadSolveMode::WithCheckAfterFallback)
    {
        commands << "c e\n\n";
        commands << "c n\n\n";
        commands << "c w\n\n";
    }
    else if (mode == OpenCalphadSolveMode::RoundedInputGlobalEquilibrium)
    {
        commands << "set n 1000\n\n";
        commands << "c e\n\n";
        commands << "c c\n\n";
    }
    else if (mode == OpenCalphadSolveMode::CalculateCarefully)
    {
        commands << "c e\n\n";
        commands << "c c\n\n";
    }
    else
        commands << "c e\n\n";
    
    return commands.str();
}

std::vector<double> rounded(const std::vector<double>& fractions)
{
    std::vector<double> rounded_fractions(fractions.size(), 0.0);
    if (fractions.empty())
        return rounded_fractions;

    const int scale = 10000;

    std::vector<int> base_units(fractions.size(), 0);
    std::vector<std::pair<double, size_t>> remainders;
    int assigned_units = 0;

    for (size_t index = 0; index < fractions.size(); ++index)
    {
        const double scaled = std::max(0.0, fractions[index]) * scale;
        const int base = static_cast<int>(std::floor(scaled));
        base_units[index] = base;
        assigned_units += base;
        remainders.push_back(std::make_pair(scaled - base, index));
    }

    std::sort(
        remainders.begin(),
        remainders.end(),
        [](const std::pair<double, size_t>& left, const std::pair<double, size_t>& right)
        {
            if (left.first == right.first)
                return left.second < right.second;
            return left.first > right.first;
        });

    int remaining_units = std::max(0, scale - assigned_units);
    for (int i = 0; i < remaining_units && i < static_cast<int>(remainders.size()); ++i)
        base_units[remainders[i].second] += 1;

    for (size_t index = 0; index < base_units.size(); ++index)
        rounded_fractions[index] = static_cast<double>(base_units[index]) / scale;

    return rounded_fractions;
}

void dumpParsedOcOutput(const OCOutputData& output_data)
{
    std::cout << "\n[OC parser] Parsed components" << std::endl;
    if (output_data.components.empty())
    {
        std::cout << "  <none>" << std::endl;
    }
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

void releaseGrainBoundarySpecies(SciantixArray<System>& sciantix_system,
                                SciantixArray<SciantixVariable>& sciantix_variable)
{
    for (auto& system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0 && system.getGas().getChemicallyActive() == 1.0)
        {
            sciantix_variable[system.getGasName() + " at grain boundary"].addValue(
                sciantix_variable[system.getGasName() + " reacted - GB"].getFinalValue());
            sciantix_variable[system.getGasName() + " reacted - GB"].setFinalValue(0.0);
        }
    }
}

std::vector<OpenCalphadInputComponent> buildOpenCalphadInputComponents(
    const std::string&                    location,
    const std::set<std::string>&          manifest_elements,
    SciantixArray<SciantixVariable>&      sciantix_variable,
    double&                               total_content)
{
    std::vector<OpenCalphadInputComponent> components;
    total_content = 0.0;

    for (const auto& element_name : manifest_elements)
    {
        OpenCalphadInputComponent component;
        component.name = element_name;

        if (isMatrixLocation(location))
        {
            if (element_name == "O")
                component.content = std::max(0.0, sciantix_variable["Oxygen content"].getFinalValue());
            else if (element_name == "U")
                component.content = std::max(0.0, sciantix_variable["Uranium content"].getFinalValue());
        }
        else if (isGrainBoundaryLocation(location))
        {
            double grain_boundary = sciantix_variable["Grain boundary fraction"].getFinalValue();
            if (element_name == "O")
            {
                // With O referenced to O2 gas at the standard pressure, AC(O)^2 = pO2 / pO2_ref.
                const double oxygen_partial_pressure =
                    std::max(0.0, sciantix_variable["Fuel oxygen partial pressure"].getFinalValue());
                double activity = std::sqrt(oxygen_partial_pressure / reference_oxygen_pressure_bar);
                const double oxygen_content = std::max(0.0, sciantix_variable["Oxygen content"].getFinalValue());

                component.content = oxygen_content;
            }
            else if (element_name == "U")
            {
                const double uranium_content = std::max(0.0, sciantix_variable["Uranium content"].getFinalValue());

                component.content = uranium_content;
            }
            else
            {
                const double atoms_available =
                    sciantix_variable[element_name + " produced"].getFinalValue() -
                    sciantix_variable[element_name + " decayed"].getFinalValue() -
                    sciantix_variable[element_name + " in grain"].getFinalValue() -
                    sciantix_variable[element_name + " released"].getInitialValue();

                component.content = std::max(0.0, atoms_available / avogadro_number);
            }
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
                           const std::set<std::string>& manifest_elements,
                           SciantixArray<SciantixVariable>& sciantix_variable,
                           std::set<std::string>& active_elements,
                           double&                total_input_content)
{
    std::ofstream input_file(input_file_path);
    if (!input_file)
    {
        std::cerr << "Error: Cannot create input file: " << input_file_path << std::endl;
        return false;
    }

    const double reference_oxygen_pressure_pa = reference_oxygen_pressure_bar * 1.0e6;
    const bool use_oxygen_potential = useOxygenPotentialConstraint(manifest_elements);
    const double oxygen_potential_j_per_mol =
        sciantix_variable["Fuel oxygen potential"].getFinalValue() * 1.0e3;

    const std::vector<OpenCalphadInputComponent> input_components =
        buildOpenCalphadInputComponents(location, manifest_elements, sciantix_variable, total_input_content);

    std::vector<OpenCalphadInputComponent> solve_components = input_components;

    if (solve_mode == OpenCalphadSolveMode::RoundedInputGlobalEquilibrium)
    {
        std::vector<double> fractions;
        fractions.reserve(solve_components.size());
        for (const auto& component : solve_components)
            fractions.push_back(component.fraction);

        const std::vector<double> rounded_fractions = rounded(fractions);

        for (size_t index = 0; index < solve_components.size(); ++index)
            solve_components[index].fraction = rounded_fractions[index];

        solve_components.erase(
            std::remove_if(
                solve_components.begin(),
                solve_components.end(),
                [](const OpenCalphadInputComponent& component) { return component.fraction <= 0.0; }),
            solve_components.end());
    }

    active_elements.clear();
    for (const auto& component : solve_components)
        active_elements.insert(component.name);

    if (solve_components.empty() || total_input_content <= 0.0)
        return false;

    const bool use_saved_state =
        (solve_mode == OpenCalphadSolveMode::SaveReadWarmStart) && hasOpenCalphadSavedState(state_file_path);

    if (use_saved_state)
    {
        input_file << "r u " << state_file_path << ".OCU\n\n";
    }
    else
    {
        input_file << "@$ Initialize variables:\n";
        input_file << "r t " << data_path;
        for (const auto& component : solve_components)
            input_file << " " << toLowerCopy(component.name);
        input_file << "\n\n";
    }

    input_file << "set ref o gas * " << reference_oxygen_pressure_pa << "\n";
    input_file << "\nset c t=" << temperature << " p=" << pressure << "\n";
    input_file << "\nset c ";
    
    if (use_oxygen_potential)
        input_file << "mu(o)=" << oxygen_potential_j_per_mol << " ";

    if (solve_components.size() > 1)
    {
        for (const auto& component : solve_components)
        {
            if (use_oxygen_potential && component.name == "O")
                continue;
            input_file << "n(" << toLowerCopy(component.name) << ")=" << component.fraction << " ";
        }
    }
    input_file << "\n\n";
    if (solve_mode == OpenCalphadSolveMode::SaveReadWarmStart)
    {
        if (use_saved_state)
            input_file << "c w\n\n";
        else
            input_file << "c e\n\n";

        input_file << "save u " << state_file_path << " Y\n\n";
    }
    else
    {
        input_file << buildSolveCommandBlock(solve_mode, temperature, pressure);
    }

    input_file << "l /out=" << output_file_path << " r 1\n\n";
    input_file << "fin";
    return true;
}

bool runOpenCalphadCase(const std::string& input_file_path,
                        const std::string& output_file_path,
                        const std::string& executable,
                        std::string&       raw_output)
{
    std::cout << "\n[OC input] " << input_file_path << std::endl;
    std::cout << "----------------------------------------" << std::endl;
    std::cout << readTextFile(input_file_path) << std::endl;
    std::cout << "----------------------------------------" << std::endl;

    const std::string command =
        "timeout --signal=TERM " + std::to_string(open_calphad_timeout_seconds) + "s " + executable;
    const int status = std::system(command.c_str());
    if (status != 0)
    {
        if (WIFEXITED(status) && WEXITSTATUS(status) == 124)
        {
            std::cerr << "Warning: OpenCalphad timed out after "
                      << open_calphad_timeout_seconds
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

bool hasRequiredComponents(const OCOutputData&              output_data,
                           const std::string&              location,
                           const std::set<std::string>&    active_elements)
{
    std::vector<std::string> required_components;

    required_components.assign(active_elements.begin(), active_elements.end());
    
    for (const auto& component : required_components)
    {
        std::cout << "Temporary check: "<<component<<std::endl;
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
    for (const auto& phase_entry : solution_phases)
    {
        const std::string& phase_name = phase_entry.first;
        const OCPhaseData& phase_data = phase_entry.second;

        if (!phase_data.species.empty())
        {
            for (const auto& species_entry : phase_data.species)
            {
                const std::string variable_name =
                    species_entry.first + " (" + phase_name + ", " + location + ")";

                if (thermochemistry_variable.isElementPresent(variable_name))
                    thermochemistry_variable[variable_name].setFinalValue(
                        species_entry.second.moles * content_scaling_factor);
            }

            // For non-condensed phases, OpenCalphad may report ionic constituents
            // (e.g. CS+, MO+4) while SCIANTIX manifest entries are often expressed
            // as element totals (e.g. CS, MO). Keep updating the phase-element
            // variables from the parsed elemental composition as well.
            for (const auto& element_entry : phase_data.elements)
            {
                const std::string variable_name = element_entry.first + " (" + phase_name + ", " + location + ")";
                const std::string uppercase_variable_name =
                    toUpperCopy(element_entry.first) + " (" + phase_name + ", " + location + ")";
                const bool has_variable = thermochemistry_variable.isElementPresent(variable_name);
                const bool has_uppercase_variable =
                    thermochemistry_variable.isElementPresent(uppercase_variable_name);

                if (has_variable)
                    thermochemistry_variable[variable_name].setFinalValue(
                        element_entry.second * content_scaling_factor);
                else if (has_uppercase_variable)
                    thermochemistry_variable[uppercase_variable_name].setFinalValue(
                        element_entry.second * content_scaling_factor);
            }
            continue;
        }

        for (const auto& element_entry : phase_data.elements)
        {
            const std::string variable_name = element_entry.first + " (" + phase_name + ", " + location + ")";
            const std::string uppercase_variable_name =
                toUpperCopy(element_entry.first) + " (" + phase_name + ", " + location + ")";

            if (thermochemistry_variable.isElementPresent(variable_name))
                thermochemistry_variable[variable_name].setFinalValue(
                    element_entry.second * content_scaling_factor);
            else if (thermochemistry_variable.isElementPresent(uppercase_variable_name))
                thermochemistry_variable[uppercase_variable_name].setFinalValue(
                    element_entry.second * content_scaling_factor);
        }
    }
}

void updateMatrixFromOutput(const OCOutputData&                output_data,
                            double                             pressure,
                            double                             temperature,
                            SciantixArray<SciantixVariable>&   sciantix_variable,
                            SciantixArray<Matrix>&             matrices)
{
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
                                   const std::set<std::string>&               manifest_elements,
                                   double                                     content_scaling_factor,
                                   SciantixArray<SciantixVariable>&           sciantix_variable)
{
    const auto gas_phase = solution_phases.find("gas");

    for (const auto& element : manifest_elements)
    {
        if (element == "O" || element == "U")
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
}  // namespace

void Simulation::SetPhaseDiagram(std::string location)
{
    if (isGrainBoundaryLocation(location))
    {
        Matrix fuel_(matrices[0]);
        double grain_boundary = 1.0 - (pow(fuel_.getGrainRadius(), 3.0)/pow(fuel_.getGrainRadius() + fuel_.getGrainBoundaryThickness(), 3.0));
        sciantix_variable["Grain boundary fraction"].setFinalValue(grain_boundary);

        if (input_variable["iThermochimica"].getValue() == 0 ||
            sciantix_variable["Xe at grain boundary"].getInitialValue() <= 0.0)
        {
            releaseGrainBoundarySpecies(sciantix_system, sciantix_variable);
            return;
        }

        CallThermochemistryModule(location, sciantix_variable);
        return;
    }

    if (isMatrixLocation(location))
    {
        if (input_variable["iThermochimica"].getValue() == 0)
            return;

        CallThermochemistryModule(location, sciantix_variable);
        return;
    }

    std::cout << "Location not yet modelled: " << location << std::endl;
}

void Simulation::CallThermochemistryModule(std::string                        location,
                                           SciantixArray<SciantixVariable>&   sciantix_variable)
{
    const double temperature = history_variable["Temperature"].getFinalValue();
    const double pressure = history_variable["System pressure"].getFinalValue();

    const std::vector<ThermochemistryManifestEntry> manifest =
        loadThermochemistryManifest(TestPath + "input_thermochemistry.txt");
    const ThermochemistrySettings settings =
        loadThermochemistrySettings(TestPath + "input_thermochemistry_settings.txt");
    const std::vector<ThermochemistryManifestEntry> filtered_manifest =
        filterThermochemistryManifest(manifest, settings);

    const std::string category = isMatrixLocation(location) ? "matrix" : "fission_products";
    const std::set<std::string> manifest_elements =
        getThermochemistryElements(filtered_manifest, category, location);
    const ThermochemistryPhaseSettings& location_settings =
        isMatrixLocation(location) ? settings.matrix : settings.fission_products;

    if (location_settings.module != "OPENCALPHAD")
    {
        if (isGrainBoundaryLocation(location))
        {
            releaseGrainBoundarySpecies(sciantix_system, sciantix_variable);
            return;
        }

        std::cout << "Location not yet modelled: " << location << std::endl;
        return;
    }

    const std::string directory_path = settings.opencalphad_path;
    const std::string location_tag = buildOpenCalphadLocationTag(location);
    const std::string input_file_path = TestPath + "OCinput_" + location_tag + ".OCM";
    const std::string output_file_path = TestPath + "OCoutput_" + location_tag + ".DAT";
    const std::string state_file_path = TestPath + "OCoutput_" + location_tag;
    const std::string data_path = directory_path + "data/" + stripTdbExtension(location_settings.database);
    const std::string executable = directory_path + "oc6P " + input_file_path;

    std::string raw_output;
    bool solved = false;
    OCOutputData output_data;
    double total_input_content = 0.0;
    std::set<std::string> active_elements;
    const std::string previous_output_snapshot =
        fileExists(output_file_path) ? readTextFile(output_file_path) : "";
    std::vector<OpenCalphadSolveMode> solve_modes = {
        OpenCalphadSolveMode::SaveReadWarmStart,
        OpenCalphadSolveMode::GlobalEquilibrium,
    };

    if (pressure > 1.0e5 + 1.0)
        solve_modes.push_back(OpenCalphadSolveMode::PressureAxisStepGlobalEquilibrium);

    if (temperature > 300.0 + 1.0)
        solve_modes.push_back(OpenCalphadSolveMode::TemperatureAxisStepGlobalEquilibrium);

    solve_modes.push_back(OpenCalphadSolveMode::CalculateCarefully);
    solve_modes.push_back(OpenCalphadSolveMode::RoundedInputGlobalEquilibrium);

    for (const auto solve_mode : solve_modes)
    {
        if (!writeOpenCalphadInput(
                input_file_path,
                output_file_path,
                state_file_path,
                data_path,
                pressure,
                temperature,
                solve_mode,
                location,
                manifest_elements,
                sciantix_variable,
                active_elements,
                total_input_content))
        {
            return;
        }

        std::cout << "OpenCalphad attempt for '" << location << "' with " << solveModeLabel(solve_mode)
                  << std::endl;

        if (!runOpenCalphadCase(input_file_path, output_file_path, executable, raw_output))
        {
            std::cout << "Warning: OpenCalphad attempt for location '" << location
                      << "' with " << solveModeLabel(solve_mode)
                      << " failed or timed out. Retrying." << std::endl;
            continue;
        }

        if (!hasInvalidEquilibriumResult(raw_output))
        {
            const std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());
            output_data = parseOCOutputFile(output_file_path, valid_elements);

            if (hasRequiredComponents(output_data, location, active_elements))
            {
                solved = true;
                break;
            }

            std::cout << "Warning: OpenCalphad produced an equilibrium for location '" << location
                      << "' using " << solveModeLabel(solve_mode)
                      << " but the required component lines are missing. Retrying." << std::endl;
            continue;
        }

        std::cout << "Warning: OpenCalphad returned an invalid equilibrium for location '" << location
                  << "' using " << solveModeLabel(solve_mode) << "." << std::endl;
    }

    if (!solved)
    {
        const std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());

        if (!previous_output_snapshot.empty() &&
            writeTextFile(output_file_path, previous_output_snapshot))
        {
            output_data = parseOCOutputFile(output_file_path, valid_elements);
            if (hasRequiredComponents(output_data, location, active_elements))
            {
                solved = true;
                std::cout << "Warning: all OpenCalphad attempts failed for location '" << location
                          << "'. Reusing the previous timestep equilibrium from " << output_file_path
                          << "." << std::endl;
            }
        }

        if (!solved)
        {
            std::cout << "Warning: all OpenCalphad attempts failed for location '" << location
                      << "' and no valid previous timestep equilibrium was available. Continue in any case."
                      << std::endl;
            output_data = parseOCOutputFile(output_file_path, valid_elements);
        }
    }

    dumpParsedOcOutput(output_data);

    updateThermochemistryVariablesFromOutput(
        output_data.solution_phases,
        location,
        total_input_content,
        thermochemistry_variable);

    if (isMatrixLocation(location))
    {
        updateMatrixFromOutput(output_data, pressure, temperature, sciantix_variable, matrices);
        return;
    }

    if (isGrainBoundaryLocation(location))
    {
        updateGrainBoundaryFromOutput(
            output_data.solution_phases,
            manifest_elements,
            total_input_content,
            sciantix_variable);
        return;
    }

    std::cout << "Location not yet modelled: " << location << std::endl;
}
