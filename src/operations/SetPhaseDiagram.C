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

#include "Simulation.h"
#include "MainVariables.h"
#include "OCUtilsCoupling.h"
#include "ThermochemistrySettings.h"

#include <iostream>
#include <set>
#include <string>
#include <vector>

void Simulation::SetPhaseDiagram(std::string location) // qui tutti eccetto i gas. 
{
    if (location == "at grain boundary")
    {
        if (input_variable["iThermochimica"].getValue() == 0 ||
            sciantix_variable["Xe at grain boundary"].getInitialValue() <= 0.0)
        {
            for (auto& system : sciantix_system)
            {
                if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
                {
                    sciantix_variable[system.getFissionProductName() + " at grain boundary"].addValue(
                        sciantix_variable[system.getFissionProductName() + " reacted - GB"].getFinalValue());
                    sciantix_variable[system.getFissionProductName() + " reacted - GB"].setFinalValue(0.0);
                }
            }
            return;
        }

        CallThermochemistryModule(location, sciantix_variable);
        return;
    }

    if (location == "matrix")
    {
        if (input_variable["iThermochimica"].getValue() == 0)
            return;

        CallThermochemistryModule(location, sciantix_variable);
        return;
    }

    std::cout << "Location not yet modelled: " << location << std::endl;
}

void Simulation::CallThermochemistryModule(std::string                      location,
                                           SciantixArray<SciantixVariable>& sciantix_variable)
{
    const ThermochemistrySettings& Sciantix_thermochemistry_settings = thermochemistry_settings;
    using OpenCalphadSolveMode = OCUtilsCoupling::OpenCalphadSolveMode;
    constexpr int open_calphad_timeout_seconds = 60;

    const double temperature = history_variable["Temperature"].getFinalValue();
    const double pressure = history_variable["System pressure"].getFinalValue();

    const std::string category = (location == "matrix") ? "matrix" : "fission_products";
    const ThermochemistryPhaseSettings& location_settings =
        (location == "matrix") ? Sciantix_thermochemistry_settings.matrix : Sciantix_thermochemistry_settings.fission_products;
    std::set<std::string> selected_elements(location_settings.elements.begin(), location_settings.elements.end());

    if (location_settings.module != "OPENCALPHAD")
    {
        if (location == "at grain boundary")
        {
            for (auto& system : sciantix_system)
            {
                if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
                {
                    sciantix_variable[system.getFissionProductName() + " at grain boundary"].addValue(
                        sciantix_variable[system.getFissionProductName() + " reacted - GB"].getFinalValue());
                    sciantix_variable[system.getFissionProductName() + " reacted - GB"].setFinalValue(0.0);
                }
            }
            return;
        }

        std::cout << "Location not yet modelled: " << location << std::endl;
        return;
    }

    const std::string directory_path = Sciantix_thermochemistry_settings.opencalphad_path;
    const std::string input_file_path = TestPath + "OCinput_" + category + ".OCM";
    const std::string output_file_path = TestPath + "OCoutput_" + category + ".DAT";
    const std::string state_file_path = TestPath + "OCoutput_" + category;
    const std::string data_path =
        directory_path + "data/" + OCUtilsCoupling::stripTdbExtension(location_settings.database);
    const std::string executable = directory_path + "oc6P " + input_file_path;

    std::string raw_output;
    bool solved = false;
    OCOutputData output_data;
    double total_input_content = 0.0;
    std::set<std::string> active_elements;

    const std::string previous_output_snapshot =
        OCUtilsCoupling::fileExists(output_file_path) ? OCUtilsCoupling::readTextFile(output_file_path) : "";
    const bool oxygen_potential_constraint = (selected_elements.count("O") > 0 && 
                                                selected_elements.count("U") == 0 &&
                                                selected_elements.count("Pu") == 0);
    double fallback_oxygen_moles = -1.0;
    std::vector solve_attempts = {
        OpenCalphadSolveMode::SaveReadWarmStart,
        OpenCalphadSolveMode::GlobalEquilibrium,
    };
    bool has_deferred_matrix_stoichiometric_solution = false;
    OCOutputData deferred_matrix_stoichiometric_output_data;
    std::set<std::string> deferred_matrix_stoichiometric_active_elements;

    if (pressure > 1.0e5 + 1.0)
        solve_attempts.push_back(OpenCalphadSolveMode::PressureAxisStepGlobalEquilibrium);
    if (oxygen_potential_constraint)
        solve_attempts.push_back(OpenCalphadSolveMode::FixedOxygenMolesFromInvalidPotentialSolve);

    for (const auto& solve_attempt : solve_attempts)
    {
        if (solve_attempt == OpenCalphadSolveMode::FixedOxygenMolesFromInvalidPotentialSolve &&
            fallback_oxygen_moles <= 0.0)
        {
            std::cout << "Warning: skipping " << OCUtilsCoupling::solveModeLabel(solve_attempt)
                      << " because no fallback O moles were captured from previous failed MU(O) attempts."
                      << std::endl;
            continue;
        }

        if (!OCUtilsCoupling::writeOpenCalphadInput(
                input_file_path,
                output_file_path,
                state_file_path,
                data_path,
                pressure,
                temperature,
                solve_attempt,
                location,
                selected_elements,
                sciantix_variable,
                active_elements,
                total_input_content,
                fallback_oxygen_moles))
        {
            return;
        }

        std::cout << "OpenCalphad attempt for '" << location << "' with "
                  << OCUtilsCoupling::solveModeLabel(solve_attempt) << std::endl;

        if (!OCUtilsCoupling::runOpenCalphadCase(
                input_file_path, output_file_path, executable, raw_output, open_calphad_timeout_seconds))
        {
            std::cout << "Warning: OpenCalphad attempt for location '" << location
                      << "' with " << OCUtilsCoupling::solveModeLabel(solve_attempt)
                      << " failed or timed out. Retrying." << std::endl;
            continue;
        }

        if (!OCUtilsCoupling::hasInvalidEquilibriumResult(raw_output))
        {
            const std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());
            output_data = parseOCOutputFile(output_file_path, valid_elements);

            if (OCUtilsCoupling::hasRequiredComponents(output_data, active_elements))
            {
                solved = true;
                break;
            }

            std::cout << "Warning: OpenCalphad produced an equilibrium for location '" << location
                      << "' using " << OCUtilsCoupling::solveModeLabel(solve_attempt)
                      << " but the required component lines are missing. Retrying." << std::endl;
            continue;
        }

        if ((location == "matrix") &&
            raw_output.find("C1_MO2") != std::string::npos)
        {
            const std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());
            const OCOutputData candidate_output_data = parseOCOutputFile(output_file_path, valid_elements);

            has_deferred_matrix_stoichiometric_solution = true;
            deferred_matrix_stoichiometric_output_data = candidate_output_data;
            deferred_matrix_stoichiometric_active_elements = active_elements;
            std::cout << "Warning: OpenCalphad returned a stoichiometric C1_MO2 stabilization failure for "
                        << "location '" << location << "' using "
                        << OCUtilsCoupling::solveModeLabel(solve_attempt)
                        << ". Deferring acceptance until all solve attempts are exhausted." << std::endl;
        }

        std::cout << "Warning: OpenCalphad returned an invalid equilibrium for location '" << location
                  << "' using " << OCUtilsCoupling::solveModeLabel(solve_attempt) << "." << std::endl;

        if (oxygen_potential_constraint &&
            solve_attempt != OpenCalphadSolveMode::FixedOxygenMolesFromInvalidPotentialSolve)
        {
            double extracted_oxygen_moles = -1.0;
            if (OCUtilsCoupling::tryGetOxygenMolesFromOutput(
                    output_file_path, active_elements, extracted_oxygen_moles))
            {
                fallback_oxygen_moles = extracted_oxygen_moles;
                std::cout << "Captured fallback N(O)=" << fallback_oxygen_moles
                          << " from invalid MU(O) output for location '" << location
                          << "'." << std::endl;
            }
        }
    }

    if (!solved)
    {
        if (has_deferred_matrix_stoichiometric_solution)
        {
            solved = true;
            output_data = deferred_matrix_stoichiometric_output_data;
            active_elements = deferred_matrix_stoichiometric_active_elements;
            std::cout << "Warning: all OpenCalphad solve attempts failed for location '" << location
                      << "'. Accepting deferred stoichiometric C1_MO2 stabilization result."
                      << std::endl;
        }
        else
        {
            const std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());

            if (!previous_output_snapshot.empty() &&
                OCUtilsCoupling::writeTextFile(output_file_path, previous_output_snapshot))
            {
                output_data = parseOCOutputFile(output_file_path, valid_elements);
                if (OCUtilsCoupling::hasRequiredComponents(output_data, active_elements))
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
    }

    OCUtilsCoupling::dumpParsedOcOutput(output_data);

    OCUtilsCoupling::updateThermochemistryVariablesFromOutput(
        output_data.solution_phases,
        location,
        total_input_content,
        thermochemistry_variable);

    if (location == "matrix")
    {
        OCUtilsCoupling::updateMatrixFromOutput(
            output_data, pressure, temperature, sciantix_variable, matrices);
        return;
    }

    if (location == "at grain boundary")
    {
        OCUtilsCoupling::updateGrainBoundaryFromOutput(
            output_data.solution_phases,
            selected_elements,
            total_input_content,
            sciantix_variable);
        return;
    }

    std::cout << "Location not yet modelled: " << location << std::endl;
}
