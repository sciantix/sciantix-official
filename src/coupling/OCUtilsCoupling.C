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
#include "OCASIAdapter.h"
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
std::string toUpperCopy(std::string text)
{
    std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c) { return std::toupper(c); });
    return text;
}

bool isLiquidPhase(const std::string& phase_name)
{
    return phase_name == "liquid" || phase_name == "ionic_liquid" || phase_name == "liquid_ionic";
}

}  // namespace

namespace OCUtilsCoupling
{
    
bool fileExists(const std::string& file_path)
{
    std::ifstream file(file_path);
    return static_cast<bool>(file);
}

// Debug
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
                  << ", form_units=" << phase_data.form_units
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

        if (!phase_data.sublattices.empty())
        {
            std::cout << "    Sublattices" << std::endl;
            for (const auto& sublattice : phase_data.sublattices)
            {
                std::cout << "      Sublattice " << sublattice.index
                          << " : constituents=" << sublattice.constituents_count
                          << ", sites=" << sublattice.sites
                          << ", phase_instance=" << sublattice.phase_instance
                          << ", phase_moles=" << sublattice.phase_moles
                          << std::endl;

                for (const auto& constituent_entry : sublattice.composition)
                {
                    std::cout << "        " << constituent_entry.first
                              << " = " << constituent_entry.second
                              << std::endl;
                }
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
                          << ", atom_equivalent_moles=" << species_data.atom_equivalent_moles
                          << ", stoichiometric_size=" << species_data.stoichiometric_size
                          << ", volume=" << species_data.volume
                          << std::endl;

                for (const auto& element_entry : species_data.elements)
                {
                    std::cout << "        " << element_entry.first
                              << " = " << element_entry.second
                              << std::endl;
                }

                for (const auto& sublattice : species_data.sublattices)
                {
                    std::cout << "        Sublattice " << sublattice.index
                              << " : constituents=" << sublattice.constituents_count
                              << ", sites=" << sublattice.sites
                              << ", phase_instance=" << sublattice.phase_instance
                              << ", phase_moles=" << sublattice.phase_moles
                              << std::endl;

                    for (const auto& constituent_entry : sublattice.composition)
                    {
                        std::cout << "          " << constituent_entry.first
                                  << " = " << constituent_entry.second
                                  << std::endl;
                    }
                }
            }
        }
    }
}

bool writePhaseSublatticeCompositionOutput(const std::string& file_path,
                                           double             time_hours,
                                           const std::string& location,
                                           const OCOutputData& output_data,
                                           double             content_scaling_factor)
{
    const bool write_header = !fileExists(file_path);
    std::ofstream output_file(file_path, std::ios::app);
    if (!output_file)
        return false;

    if (write_header)
    {
        output_file << "Time (h)\tLocation\tPhase\tPhase instance\tMoles (mol/m3)\t"
                    << "Form units (mol/m3)\tSublattice\tSites\tConstituent\tSite fraction\n";
    }

    output_file << std::setprecision(10);
    for (const auto& phase_entry : output_data.solution_phases)
    {
        const std::string& phase_name = phase_entry.first;
        const OCPhaseData& phase_data = phase_entry.second;

        if (phase_name == "condensed")
        {
            for (const auto& species_entry : phase_data.species)
            {
                const std::string& species_name = species_entry.first;
                const OCSpeciesData& species_data = species_entry.second;

                for (const auto& sublattice : species_data.sublattices)
                {
                    for (const auto& constituent_entry : sublattice.composition)
                    {
                        output_file << time_hours << "\t"
                                    << location << "\t"
                                    << species_name << "\t"
                                    << sublattice.phase_instance << "\t"
                                    << sublattice.phase_moles * content_scaling_factor << "\t"
                                    << sublattice.phase_form_units * content_scaling_factor << "\t"
                                    << sublattice.index << "\t"
                                    << sublattice.sites << "\t"
                                    << constituent_entry.first << "\t"
                                    << constituent_entry.second << "\n";
                    }
                }
            }
            continue;
        }

        for (const auto& sublattice : phase_data.sublattices)
        {
            for (const auto& constituent_entry : sublattice.composition)
            {
                output_file << time_hours << "\t"
                            << location << "\t"
                            << phase_name << "\t"
                            << sublattice.phase_instance << "\t"
                            << sublattice.phase_moles * content_scaling_factor << "\t"
                            << sublattice.phase_form_units * content_scaling_factor << "\t"
                            << sublattice.index << "\t"
                            << sublattice.sites << "\t"
                            << constituent_entry.first << "\t"
                            << constituent_entry.second << "\n";
            }
        }
    }

    return output_file.good();
}

std::vector<InputComponent> buildInputComponents(
     const std::set<std::string>&     selected_elements,
     SciantixArray<SciantixVariable>& sciantix_variable,
     SciantixArray<System>&           sciantix_system,
     double&                          total_content,
     const std::string& location)
{
    std::vector<InputComponent> components;
    total_content = 0.0;

    if (location == "matrix")
    {
        // Matrix component
        for (const auto& element_name : selected_elements)
        {
            InputComponent component;
            component.name = element_name;
            component.content = std::max(0.0, sciantix_variable[element_name + " content"].getFinalValue());

            if (component.content > 0.0)
            {
                total_content += component.content;
                components.push_back(component);
            }
        }
    }
    else if (location == "at grain boundary")
    {
        if (selected_elements.count("O") > 0)
        {
            InputComponent component;
            component.name = "O";
            component.content = std::max(0.0, sciantix_variable["O available content"].getFinalValue());
            
            if (component.content > 0.0)
            {
                total_content += component.content;
                components.push_back(component);
            }
        }

        // FP component
        for (auto& system : sciantix_system)
        {
            const std::string element_name = system.getFissionProductName();
            if (selected_elements.count(element_name) == 0)
                continue;

            InputComponent component;
            component.name = element_name;

            if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
            {
                const double atoms_available =
                    sciantix_variable[element_name + " produced"].getFinalValue() -
                    sciantix_variable[element_name + " decayed"].getFinalValue() -
                    sciantix_variable[element_name + " in grain"].getFinalValue() -
                    sciantix_variable[element_name + " released"].getInitialValue();

                component.content = std::max(0.0, atoms_available / avogadro_number);
            }
            else if (system.getRestructuredMatrix() == 0 && system.isMetallicFP())
            {
                const double atoms_available =
                    sciantix_variable[element_name + " produced"].getFinalValue();

                component.content = std::max(0.0, atoms_available / avogadro_number);
            }

            if (component.content > 0.0)
            {
                total_content += component.content;
                components.push_back(component);
            }
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
            [](const InputComponent& component)
            {
                return component.fraction < 1.0e-8; // cut-off
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

bool runOpenCalphadCaseOCASI(const std::string& database_path,
                             double temperature,
                             double pressure,
                             const std::vector<InputComponent>& components,
                             const std::vector<std::string>& valid_elements,
                             OpenCalphadSolveMode solve_mode,
                             const std::string& location,
                             double oxygen_potential_kj_per_mol_o2,
                             OCOutputData& output_data)
{
    try
    {
        const OCASIAdapter::OpenCalphadContext context =
            (location == "matrix")
                ? OCASIAdapter::OpenCalphadContext::Matrix
                : OCASIAdapter::OpenCalphadContext::FissionProducts;
        auto& oc = OCASIAdapter::getOpenCalphadInterface(context);

        const bool use_stored_equilibrium =
            solve_mode == OpenCalphadSolveMode::SaveReadWarmStart;
        const bool database_ready = use_stored_equilibrium
            ? oc.ensureDatabaseLoaded(database_path, valid_elements)
            : oc.loadDatabase(database_path, valid_elements);

        if (!database_ready)
        {
            std::cerr << "Error: Failed to load OpenCalphad database: " << database_path << std::endl;
            return false;
        }

        if (!oc.setReferenceState("O", "GAS", -1.0, reference_oxygen_pressure_bar * 1.0e6))
            std::cerr << "Warning: Failed to set OpenCalphad oxygen gas reference state" << std::endl;

        std::map<std::string, double> components_map;
        for (const auto& comp : components)
            components_map[comp.name] = comp.fraction;

        if (!oc.setConditions(temperature, pressure, components_map))
        {
            std::cerr << "Error: Failed to set OpenCalphad conditions" << std::endl;
            return false;
        }
        
        // Same first solve as the previous macro `c e`: no grid minimizer.
        bool clear_equilibrium = true;

        if (!oc.calculateEquilibrium(-1))
        {
            std::cerr << "Warning: Initial OpenCalphad equilibrium calculation failed" << std::endl;
            clear_equilibrium = false;
        }

        const bool use_oxygen_potential =
            location == "at grain boundary" && solve_mode != OpenCalphadSolveMode::FixedOxygenMoles;
        if (use_oxygen_potential)
        {
            if (!oc.removeComponentCondition("O"))
            {
                std::cerr << "Error: Failed to remove OpenCalphad oxygen amount condition" << std::endl;
                return false;
            }

            const double oxygen_potential_j_per_mol_o = oxygen_potential_kj_per_mol_o2 * 1.0e3 / 2.0;
            if (!oc.setComponentPotential("O", oxygen_potential_j_per_mol_o))
            {
                std::cerr << "Error: Failed to set OpenCalphad oxygen potential condition" << std::endl;
                return false;
            }
        }

        if (solve_mode == OpenCalphadSolveMode::SaveReadWarmStart ||
            solve_mode == OpenCalphadSolveMode::GlobalEquilibrium)
        {
            if (!oc.calculateEquilibriumChecked())
            {
                std::cerr << "Warning: OpenCalphad checked equilibrium calculation failed" << std::endl;
                clear_equilibrium = false;
            }
        }
        else if (solve_mode == OpenCalphadSolveMode::PressureAxisStep)
        {
            constexpr double start_pressure = 1.0e5;
            const double pressure_increment = std::max(1.0, 0.025 * std::abs(pressure - start_pressure));

            if (!oc.setPressure(start_pressure) || !oc.calculateEquilibriumChecked())
            {
                std::cerr << "Warning: OpenCalphad checked equilibrium at start pressure failed" << std::endl;
                clear_equilibrium = false;
            }

            if (!oc.stepNormal("P", start_pressure, pressure, pressure_increment) ||
                !oc.setPressure(pressure) ||
                !oc.calculateEquilibrium(-1) ||
                !oc.calculateEquilibriumChecked())
            {
                std::cerr << "Warning: OpenCalphad pressure-axis final equilibrium failed" << std::endl;
                clear_equilibrium = false;
            }
        }
        else if (solve_mode == OpenCalphadSolveMode::OnlyC1MO2)
        {
            oc.setPhaseStatus("*", -2, 0.0);
            oc.setPhaseStatus("GAS", 0, 1.0);
            if (!oc.calculateEquilibrium(-1))
            {
                std::cerr << "Warning: OpenCalphad gas-only equilibrium calculation failed" << std::endl;
                clear_equilibrium = false;
            }
            oc.setPhaseStatus("C1_MO2", 0, 1.0);

            if (!oc.calculateEquilibrium(-1) || !oc.calculateEquilibriumChecked())
            {
                std::cerr << "Warning: OpenCalphad fixed C1_MO2 equilibrium failed" << std::endl;
                clear_equilibrium = false;
            }
        }

        if (!oc.extractResults(output_data))
        {
            std::cerr << "Error: Failed to extract OpenCalphad results" << std::endl;
            return false;
        }

        return clear_equilibrium;
    }
    catch (const std::exception& e)
    {
        std::cerr << "Exception in runOpenCalphadCaseOCASI: " << e.what() << std::endl;
        return false;
    }
}

void updateThermochemistryVariablesFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                              const std::string&                         location,
                                              double                                     content_scaling_factor,
                                              SciantixArray<ThermochemistryVariable>&    thermochemistry_variable,
                                              SciantixArray<SciantixVariable>&           sciantix_variable)
{
    auto computePhaseComposition = [](const OCPhaseData& phase_data)
    {
        std::map<std::string, double> composition;
        if (phase_data.moles <= 0.0)
            return composition;

        for (const auto& element_entry : phase_data.elements)
            composition[element_entry.first] = std::max(0.0, element_entry.second) / phase_data.moles;

        return composition;
    };

    double oxygen_with_fps = 0.0;
    for (const auto& phase_entry : solution_phases)
    {
        const std::string& phase_name = phase_entry.first;
        const OCPhaseData& phase_data = phase_entry.second;
        const bool liquid_phase = isLiquidPhase(phase_name);

        const auto oxygen = phase_data.elements.find("O");
        if (oxygen != phase_data.elements.end())
            oxygen_with_fps += oxygen->second * content_scaling_factor;

        if (liquid_phase)
        {
            const std::string liquid_variable_name = "LIQUID (" + phase_name + ", " + location + ")";
            if (thermochemistry_variable.isElementPresent(liquid_variable_name))
            {
                thermochemistry_variable[liquid_variable_name].setFinalValue(
                    phase_data.moles * content_scaling_factor);

                const std::map<std::string, double> composition = computePhaseComposition(phase_data);
                if (!composition.empty())
                    thermochemistry_variable[liquid_variable_name].setComposition(composition);
            }
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
                }
            }

            if (liquid_phase)
                continue;

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
                }
                else if (has_uppercase_variable)
                {
                    thermochemistry_variable[uppercase_variable_name].setFinalValue(
                        element_entry.second * content_scaling_factor);
                    thermochemistry_variable[uppercase_variable_name].setComposition({{element_entry.first, 1.0}});
                }
            }
            continue;
        }

        if (liquid_phase)
            continue;

        for (const auto& element_entry : phase_data.elements)
        {
            const std::string variable_name = element_entry.first + " (" + phase_name + ", " + location + ")";
            const std::string uppercase_variable_name =
                toUpperCopy(element_entry.first) + " (" + phase_name + ", " + location + ")";

            if (thermochemistry_variable.isElementPresent(variable_name))
            {
                thermochemistry_variable[variable_name].setFinalValue(
                    element_entry.second * content_scaling_factor);
                thermochemistry_variable[variable_name].setComposition({{element_entry.first, 1.0}});
            }
            else if (thermochemistry_variable.isElementPresent(uppercase_variable_name))
            {
                thermochemistry_variable[uppercase_variable_name].setFinalValue(
                    element_entry.second * content_scaling_factor);
                thermochemistry_variable[uppercase_variable_name].setComposition({{element_entry.first, 1.0}});
            }
        }
    }

    if (location == "at grain boundary")
        sciantix_variable["O available content"].setFinalValue(oxygen_with_fps);
}

void updateMatrixFromOutput(const OCOutputData&              output_data,
                            double                           temperature,
                            SciantixArray<SciantixVariable>& sciantix_variable)
{
    const auto oxygen_component = output_data.components.find("O");
    double calphad_oxygen_potential(0.0), calphad_oxygen_partial_pressure(0.0);
    const bool has_usable_oxygen_component =
        oxygen_component != output_data.components.end() &&
        oxygen_component->second.activity > 0.0;
    if (has_usable_oxygen_component)
    {
        calphad_oxygen_potential =
            2.0 * oxygen_component->second.chemical_potential_over_rt * gas_constant * temperature * 1.0e-3;
        calphad_oxygen_partial_pressure =
            reference_oxygen_pressure_bar * oxygen_component->second.activity * oxygen_component->second.activity;

        sciantix_variable["Fuel oxygen partial pressure - CALPHAD"].setFinalValue(calphad_oxygen_partial_pressure);
        sciantix_variable["Fuel oxygen potential - CALPHAD"].setFinalValue(calphad_oxygen_potential);
    }

    if (calphad_oxygen_partial_pressure > 0.0)
    {
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(calphad_oxygen_partial_pressure);
        sciantix_variable["Fuel oxygen potential"].setFinalValue(calphad_oxygen_potential);
    }
}

void updateGrainBoundaryFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                   const std::set<std::string>&               selected_elements,
                                   double                                     content_scaling_factor,
                                   SciantixArray<SciantixVariable>&           sciantix_variable,
                                   SciantixArray<System>&                     sciantix_system)
{
    const auto gas_phase = solution_phases.find("gas");

    for (auto& system : sciantix_system)
    {
        const std::string element = system.getFissionProductName();
        if (selected_elements.count(element) == 0)
            continue;

        double gas_moles = 0.0;
        if (gas_phase != solution_phases.end() && gas_phase->second.elements.count(element) > 0)
            gas_moles = gas_phase->second.elements.at(element) * content_scaling_factor;

        if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
        {
            const double available = (
                sciantix_variable[element + " produced"].getFinalValue() -
                sciantix_variable[element + " decayed"].getFinalValue() -
                sciantix_variable[element + " in grain"].getFinalValue() -
                sciantix_variable[element + " released"].getInitialValue()
            );

            const double updated_atoms = std::min(available, gas_moles * avogadro_number);
            sciantix_variable[element + " at grain boundary"].setFinalValue(updated_atoms);
            sciantix_variable[element + " reacted"].setFinalValue(available - updated_atoms);
        }
        else if (system.getRestructuredMatrix() == 0 && system.isMetallicFP())
        {
            const double available =
                sciantix_variable[element + " produced"].getFinalValue();

            const double updated_atoms = std::min(available, gas_moles * avogadro_number);
            sciantix_variable[element + " in solution"].setFinalValue(updated_atoms);
            sciantix_variable[element + " reacted"].setFinalValue(available - updated_atoms);
        }
    }
}
}  // namespace OCUtilsCoupling
