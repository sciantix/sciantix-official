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
    std::cout << "[Thermochemistry debug] location=" << location
              << " update" << std::endl;

    const ThermochemistrySettings& Sciantix_thermochemistry_settings = *thermochemistry_settings;
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
    if (location == "matrix")
        solvers.push_back(OCSolver::OnlyC1MO2);

    std::cout << "[Thermochemistry debug] solvers for " << location << ':';
    for (const auto& solver : solvers)
        std::cout << ' ' << static_cast<int>(solver);
    std::cout << std::endl;

    const std::string data_path =
        Sciantix_thermochemistry_settings.opencalphad_path + "data/" + location_settings.database;
    
    bool solved = false;
    OCOutputData output_data;
    double total_input_content = 0.0;
    std::vector<InputComponent> components =
    OCUtilsCoupling::buildInputComponents(selected_elements, sciantix_variable, sciantix_system, total_input_content, location);
    std::set<std::string> active_elements;
    for (const auto& component : components)
        active_elements.insert(component.name);
    const std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());

    std::cout << "[Thermochemistry debug] input for " << location
              << " T=" << history_variable["Temperature"].getFinalValue()
              << " P=" << history_variable["System pressure"].getFinalValue()
              << " database=" << data_path
              << " total_input_content=" << total_input_content
              << std::endl;
    std::cout << "[Thermochemistry debug] components:";
    for (const auto& component : components)
        std::cout << ' ' << component.name
                  << "(content=" << component.content
                  << ", fraction=" << component.fraction << ')';
    std::cout << std::endl;
    std::cout << "[Thermochemistry debug] valid elements:";
    for (const auto& element : valid_elements)
        std::cout << ' ' << element;
    std::cout << std::endl;

    // Attempt for each solver
    for (const auto& solver : solvers)
    { 
        std::cout << "[Thermochemistry debug] attempting " << location
                  << " solver=" << static_cast<int>(solver) << std::endl;

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
        
        bool has_usable_output = false; 
        if (location == "matrix" && ((!output_data.solution_phases.empty())))
            has_usable_output = true;
        else if (location == "at grain boundary")
            has_usable_output = case_success;

        std::cout << "[Thermochemistry debug] solver=" << static_cast<int>(solver)
                  << " case_success=" << case_success
                  << " has_usable_output=" << has_usable_output
                  << " phases=" << output_data.solution_phases.size()
                  << " components=" << output_data.components.size()
                  << std::endl;

        if (!case_success || !has_usable_output)
            continue;

        // Mark as solved since we got results
        solved = true;
        break;
    }

    if (!solved)
        std::cout << "Warning: all OpenCalphad attempts failed for location: " << location << std::endl;
    
    if (location != "matrix")
        OCUtilsCoupling::getOpenCalphadResults(location, output_data);

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

    if (location == "matrix")
    {
        std::cout << "[Thermochemistry debug] updating matrix from OpenCalphad output"
                  << std::endl;

        OCUtilsCoupling::updateThermochemistryVariablesFromOutput(
            output_data.solution_phases,
            location,
            total_input_content,
            thermochemistry_variable,
            sciantix_variable);

        OCUtilsCoupling::updateMatrixFromOutput(
            output_data, history_variable["Temperature"].getFinalValue(), sciantix_variable);
        return;
    }
    else if (location == "at grain boundary")
    {
        std::cout << "[Thermochemistry debug] updating grain boundary from OpenCalphad output"
                  << std::endl;

        OCUtilsCoupling::updateThermochemistryVariablesFromOutput(
            output_data.solution_phases,
            location,
            total_input_content,
            thermochemistry_variable,
            sciantix_variable);
            
        OCUtilsCoupling::updateGrainBoundaryFromOutput(
            output_data.solution_phases,
            selected_elements,
            total_input_content,
            sciantix_variable,
            sciantix_system);

        return;
    }
}
