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

namespace
{
bool hasUsableOpenCalphadOutput(const OCOutputData& output_data)
{
    if (!output_data.solution_phases.empty())
        return true;

    for (const auto& component_entry : output_data.components)
    {
        const OCComponentData& component = component_entry.second;
        if (component.moles > 0.0 ||
            component.activity > 0.0 ||
            component.chemical_potential_over_rt != 0.0)
            return true;
    }

    return false;
}
}

void Simulation::SetPhaseDiagram(std::string location) // qui tutti eccetto i gas. 
{
    if (location == "at grain boundary")
    {
        if (input_variable["iThermochimica"].getValue() == 0 ||
            sciantix_variable["Xe at grain boundary"].getInitialValue() <= 0.0 ||
            thermochemistry_settings == nullptr)
        {
            for (auto& system : sciantix_system)
            {
                if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
                {
                    sciantix_variable[system.getFissionProductName() + " at grain boundary"].addValue(
                        sciantix_variable[system.getFissionProductName() + " reacted"].getFinalValue());
                    sciantix_variable[system.getFissionProductName() + " reacted"].setFinalValue(0.0);
                }
                if (system.getRestructuredMatrix() == 0 && system.isMetallicFP())
                {
                    sciantix_variable[system.getFissionProductName() + " in solution"].setFinalValue(
                        sciantix_variable[system.getFissionProductName() + " produced"].getFinalValue());
                    sciantix_variable[system.getFissionProductName() + " reacted"].setFinalValue(0.0);
                }
            }
            return;
        }

        CallThermochemistryModule(location, sciantix_variable);
        return;
    }
    else if (location == "matrix")
    {
        if (input_variable["iThermochimica"].getValue() == 0)
            return;

        CallThermochemistryModule(location, sciantix_variable);
        return;
    }
    else        
        std::cerr << "Location not yet modelled: " << location << std::endl;
}

void Simulation::CallThermochemistryModule(std::string                      location,
                                           SciantixArray<SciantixVariable>& sciantix_variable)
{

    const ThermochemistrySettings& Sciantix_thermochemistry_settings = *thermochemistry_settings;
    const ThermochemistryPhaseSettings& location_settings =
        (location == "matrix") ? Sciantix_thermochemistry_settings.matrix : Sciantix_thermochemistry_settings.fission_products;
    const std::string category = (location == "matrix") ? "matrix" : "fission_products";
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
                        sciantix_variable[system.getFissionProductName() + " reacted"].getFinalValue());
                    sciantix_variable[system.getFissionProductName() + " reacted"].setFinalValue(0.0);
                }
                if (system.getRestructuredMatrix() == 0 && system.isMetallicFP())
                {
                    sciantix_variable[system.getFissionProductName() + " in solution"].setFinalValue(
                        sciantix_variable[system.getFissionProductName() + " produced"].getFinalValue());
                    sciantix_variable[system.getFissionProductName() + " reacted"].setFinalValue(0.0);
                }
            }
        }
        return;
    }

    // Solver for O
    using OCSolver = OCUtilsCoupling::OpenCalphadSolveMode;
    std::vector<OCSolver> solvers;
    #if !defined(COUPLING_TU)
        solvers.push_back(OCSolver::SaveReadWarmStart);
    #endif
    solvers.push_back(OCSolver::GlobalEquilibrium);
    if (history_variable["System pressure"].getFinalValue() > 1.0e5 + 1.0)
        solvers.push_back(OCSolver::PressureAxisStep);
    if (location == "matrix")
        solvers.push_back(OCSolver::OnlyC1MO2);


    // Path to OC files
    const std::string state_file_path = TestPath + "OC_" + category;
    const std::string data_path =
        Sciantix_thermochemistry_settings.opencalphad_path + "data/" + location_settings.database;
    
    // Needed later
    std::string raw_output;
    bool solved = false;
    bool has_fallback_output = false;
    OCOutputData output_data;
    OCOutputData fallback_output_data;
    double total_input_content = 0.0;
    std::vector<InputComponent> components =
    OCUtilsCoupling::buildInputComponents(selected_elements, sciantix_variable, sciantix_system, total_input_content, location);
    const std::vector<std::string> valid_elements(selected_elements.begin(), selected_elements.end());

    // Attempt for each solver
    for (const auto& solver : solvers)
    { 
        // Use OCASI direct interface instead of subprocess
        bool case_success = OCUtilsCoupling::runOpenCalphadCaseOCASI(
            data_path,
            history_variable["Temperature"].getFinalValue(),
            history_variable["System pressure"].getFinalValue(),
            components,
            valid_elements,
            solver,
            location,
            sciantix_variable["Fuel oxygen potential"].getFinalValue(),
            output_data);

        const bool has_usable_output = hasUsableOpenCalphadOutput(output_data);
        if (has_usable_output)
        {
            fallback_output_data = output_data;
            has_fallback_output = true;
        }

        if (!case_success || !has_usable_output)
        {
            continue;
        }

        // Mark as solved since we got results
        solved = true;
        
        // Debug output
        std::cout << "\n[OCASI Output - Direct C++ Interface]" << std::endl;
        std::cout << "----------------------------------------" << std::endl;
        std::cout << "Temperature: " << history_variable["Temperature"].getFinalValue() << " K" << std::endl;
        std::cout << "Pressure: " << history_variable["System pressure"].getFinalValue() << " Pa" << std::endl;
        std::cout << "----------------------------------------" << std::endl;

        break;
    }

    if (!solved)
    {
        std::cout << "Warning: all OpenCalphad attempts failed for location: " << location << std::endl;
        if (has_fallback_output)
        {
            std::cout << "Warning: using the last available OpenCalphad output for location: "
                      << location << std::endl;
            output_data = fallback_output_data;
        }
    }

    // Debug
    OCUtilsCoupling::dumpParsedOcOutput(output_data);

    if (Sciantix_thermochemistry_settings.output_phase_sublattice_composition)
    {
        const std::string sublattice_output_path =
            TestPath + "phase_sublattice_composition.txt";
        if (!OCUtilsCoupling::writePhaseSublatticeCompositionOutput(
                sublattice_output_path,
                history_variable["Time"].getFinalValue(),
                location,
                output_data,
                total_input_content))
        {
            std::cout << "Warning: cannot write phase sublattice composition output: "
                      << sublattice_output_path << std::endl;
        }
    }

    OCUtilsCoupling::updateThermochemistryVariablesFromOutput(
        output_data.solution_phases,
        location,
        total_input_content,
        thermochemistry_variable,
        sciantix_variable);

    if (location == "matrix")
    {
        OCUtilsCoupling::updateMatrixFromOutput(
            output_data, history_variable["Temperature"].getFinalValue(), sciantix_variable);
        return;
    }
    else if (location == "at grain boundary")
    {
        OCUtilsCoupling::updateGrainBoundaryFromOutput(
            output_data.solution_phases,
            selected_elements,
            total_input_content,
            sciantix_variable,
            sciantix_system);
        return;
    }
}
