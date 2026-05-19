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
    OCOutputData output_data;
    double total_input_content = 0.0;
    std::vector<InputComponent> components =
    OCUtilsCoupling::buildInputComponents(selected_elements, sciantix_variable, sciantix_system, total_input_content, location);
    std::set<std::string> active_elements;
    for (const auto& component : components)
        active_elements.insert(component.name);
    const std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());

    // Attempt for each solver
    for (const auto& solver : solvers)
    { 
        if (!OCUtilsCoupling::writeOpenCalphadInput(
                state_file_path,
                data_path,
                history_variable["System pressure"].getFinalValue(),
                history_variable["Temperature"].getFinalValue(),
                solver,
                location,
                components,
                sciantix_variable))
        {
            return;
        }

        if (!OCUtilsCoupling::runOpenCalphadCase(
                Sciantix_thermochemistry_settings.opencalphad_path + "oc6P " + state_file_path + ".OCM")
            )        
            continue;

        raw_output = OCUtilsCoupling::readTextFile(state_file_path + ".DAT");

        // debug
        std::cout << "\n[OC output] " << std::endl;
        std::cout << "----------------------------------------" << std::endl;
        std::cout << raw_output << std::endl;
        std::cout << "----------------------------------------" << std::endl;

        if (!OCUtilsCoupling::hasInvalidEquilibriumResult(raw_output))
        {  
            solved = true;
            break;
        }
    }

    if (!solved)
    {
        std::cout << "Warning: all OpenCalphad attempts failed for location: " << location << std::endl;
        if ((location == "matrix") && raw_output.find("C1_MO2") != std::string::npos)
            solved = true;

        if (!solved)
        {
            std::cout << "Warning: all OpenCalphad attempts failed for location: " << location
                    << "' and no valid previous timestep equilibrium was available. Continue in any case."
                    << std::endl;
        }
    }

    output_data = parseOCOutputFile(state_file_path + ".DAT", valid_elements);

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
