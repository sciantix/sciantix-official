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
#include <vector>

namespace
{
enum class OpenCalphadSolveMode
{
    GlobalEquilibrium,
    NoGlobalFallback,
    WithCheckAfterFallback,
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

std::string toLowerCopy(std::string text)
{
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return std::tolower(c); });
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

bool hasInvalidEquilibriumResult(const std::string& output_text)
{
    return output_text.find("not a valid equilibrium as last calculation failed") != std::string::npos ||
           output_text.find("No results as no equilibrium calculated") != std::string::npos;
}

std::string solveModeLabel(OpenCalphadSolveMode mode)
{
    switch (mode)
    {
        case OpenCalphadSolveMode::GlobalEquilibrium:
            return "c e";
        case OpenCalphadSolveMode::NoGlobalFallback:
            return "c n";
        case OpenCalphadSolveMode::WithCheckAfterFallback:
            return "c w";
    }

    return "unknown";
}

std::string buildSolveCommandBlock(OpenCalphadSolveMode mode, double temperature, double pressure)
{
    std::ostringstream commands;

    if (mode == OpenCalphadSolveMode::NoGlobalFallback)
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
    else
    {
        commands << "c e\n\n";
    }

    return commands.str();
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
            if (element_name == "O")
            {
                // With O referenced to O2 gas at the standard pressure, AC(O)^2 = pO2 / pO2_ref.
                const double oxygen_partial_pressure =
                    std::max(0.0, sciantix_variable["Fuel oxygen partial pressure"].getFinalValue());
                double activity = std::sqrt(oxygen_partial_pressure / reference_oxygen_pressure_bar);
                const double oxygen_content = std::max(0.0, sciantix_variable["Oxygen content"].getFinalValue());

                component.content = activity * oxygen_content;
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

    return components;
}

bool writeOpenCalphadInput(const std::string& input_file_path,
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

    const std::vector<OpenCalphadInputComponent> input_components =
        buildOpenCalphadInputComponents(location, manifest_elements, sciantix_variable, total_input_content);

    active_elements.clear();
    for (const auto& component : input_components)
        active_elements.insert(component.name);

    if (input_components.empty() || total_input_content <= 0.0)
        return false;

    input_file << "@$ Initialize variables:\n";
    input_file << "r t " << data_path;
    for (const auto& component : input_components)
        input_file << " " << toLowerCopy(component.name);
    input_file << "\n\n";
    input_file << "set ref o gas * " << reference_oxygen_pressure_pa << "\n";
    input_file << "\nset c t=" << temperature << " p=" << pressure << " ";
    if (input_components.size() > 1)
    {
        for (const auto& component : input_components)
            input_file << "n(" << toLowerCopy(component.name) << ")=" << component.fraction << " ";
    }
    input_file << "\n\n" << buildSolveCommandBlock(solve_mode, temperature, pressure);
    input_file << "l /out=./OCoutput.DAT r 1\n\n";
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

    const int status = std::system(executable.c_str());
    if (status != 0)
    {
        std::cerr << "Error: Execution of OPENCALPHAD failed." << std::endl;
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
            continue;
        }

        for (const auto& element_entry : phase_data.elements)
        {
            const std::string variable_name = element_entry.first + " (" + phase_name + ", " + location + ")";

            if (thermochemistry_variable.isElementPresent(variable_name))
                thermochemistry_variable[variable_name].setFinalValue(
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
        if (element == "O")
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
    const std::string input_file_path = TestPath + "OCinput.OCM";
    const std::string output_file_path = TestPath + "OCoutput.DAT";
    const std::string data_path = directory_path + "data/" + stripTdbExtension(location_settings.database);
    const std::string executable = directory_path + "oc6P " + input_file_path;

    std::string raw_output;
    bool solved = false;
    OCOutputData output_data;
    double total_input_content = 0.0;
    std::set<std::string> active_elements;
    const std::vector<OpenCalphadSolveMode> solve_modes = {
        OpenCalphadSolveMode::GlobalEquilibrium,
        OpenCalphadSolveMode::NoGlobalFallback,
        OpenCalphadSolveMode::WithCheckAfterFallback,
    };

    for (const auto solve_mode : solve_modes)
    {
        if (!writeOpenCalphadInput(
                input_file_path,
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
            return;

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
        std::cout << "Warning: all OpenCalphad attempts failed for location '" << location
                  << "'. Continue in any case." << std::endl;
        const std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());
        output_data = parseOCOutputFile(output_file_path, valid_elements);
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
        
        remove(input_file_path.c_str());
        remove(output_file_path.c_str());
        return;
    }

    if (isGrainBoundaryLocation(location))
    {
        updateGrainBoundaryFromOutput(
            output_data.solution_phases,
            manifest_elements,
            total_input_content,
            sciantix_variable);

        remove(input_file_path.c_str());
        remove(output_file_path.c_str());
        return;
    }

    std::cout << "Location not yet modelled: " << location << std::endl;
}
