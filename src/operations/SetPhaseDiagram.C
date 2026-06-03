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
#include "Constants.h"
#include "MainVariables.h"
#include "OCUtilsCoupling.h"
#include "ThermochemistrySettings.h"

#include <iostream>
#include <set>
#include <string>
#include <vector>

void Simulation::SetPhaseDiagram() // qui tutti eccetto i gas. 
{
    auto moveFissionProductsWithoutThermochemistry = [&]()
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
    };

    if (input_variable["iThermochimica"].getValue() == 0 || thermochemistry_settings == nullptr)
    {
        moveFissionProductsWithoutThermochemistry();
        return;
    } 

    const ThermochemistrySettings& Sciantix_thermochemistry_settings = *thermochemistry_settings;

    enum class PhaseDiagramLocation
    {
        Matrix,
        GrainBoundary
    };

    using OCSolver = OCUtilsCoupling::OpenCalphadSolveMode;

    for (const PhaseDiagramLocation location_case : {PhaseDiagramLocation::Matrix, PhaseDiagramLocation::GrainBoundary})
    {
        const ThermochemistryPhaseSettings* location_settings = nullptr;
        std::string location;
        std::vector<OCSolver> solvers;

        #if !defined(COUPLING_TU)
            solvers.push_back(OCSolver::SaveReadWarmStart);
        #endif
        solvers.push_back(OCSolver::GlobalEquilibrium);

        switch (location_case)
        {
            case PhaseDiagramLocation::Matrix:
                location_settings = &Sciantix_thermochemistry_settings.matrix;
                location = "matrix";
                solvers.push_back(OCSolver::OnlyC1MO2);
                break;

            case PhaseDiagramLocation::GrainBoundary:
                if (sciantix_variable["Xe at grain boundary"].getInitialValue() <= 0.0)
                {
                    moveFissionProductsWithoutThermochemistry();
                    return;
                }
                location_settings = &Sciantix_thermochemistry_settings.fission_products;
                location = "at grain boundary";
                break;
        }

        if (location_settings->module != "OPENCALPHAD")
        {
            if (location_case == PhaseDiagramLocation::GrainBoundary)
                moveFissionProductsWithoutThermochemistry();
            continue;
        }

        std::set<std::string> selected_elements(location_settings->elements.begin(), location_settings->elements.end());
        const std::string data_path =
            Sciantix_thermochemistry_settings.opencalphad_path + "data/" + location_settings->database;

        bool solved = false;
        OCOutputData output_data;
        double total_input_content = 0.0;
        std::vector<InputComponent> components =
            OCUtilsCoupling::buildInputComponents(
                selected_elements,
                sciantix_variable,
                sciantix_system,
                total_input_content,
                location);
        std::set<std::string> active_elements;
        for (const auto& component : components)
            active_elements.insert(component.name);
        const std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());

        for (const auto& solver : solvers)
        {
            output_data.solution_phases.clear();
            output_data.components.clear();

            const bool case_success = OCUtilsCoupling::runOpenCalphadCaseOCASI(
                data_path,
                history_variable["Temperature"].getFinalValue(),
                history_variable["System pressure"].getFinalValue(),
                components,
                valid_elements,
                solver,
                location,
                sciantix_variable["Fuel oxygen potential"].getFinalValue(),
                output_data);
            const bool has_usable_output = !output_data.solution_phases.empty();

            if (case_success && has_usable_output)
            {
                solved = true;
                break;
            }
        }

        if (!solved)
            std::cerr << "Warning: all OpenCalphad attempts failed for " << location << std::endl;
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
                std::cerr << "Warning: cannot write phase sublattice composition output: "
                          << sublattice_output_path << std::endl;
            }
        }

        OCUtilsCoupling::updateThermochemistryVariablesFromOutput(
            output_data.solution_phases,
            location,
            total_input_content,
            thermochemistry_variable,
            sciantix_variable);

        switch (location_case)
        {
            case PhaseDiagramLocation::Matrix:
                OCUtilsCoupling::updateMatrixFromOutput(
                    output_data,
                    history_variable["Temperature"].getFinalValue(),
                    sciantix_variable);
                break;

            case PhaseDiagramLocation::GrainBoundary:
                OCUtilsCoupling::updateGrainBoundaryFromOutput(
                    output_data.solution_phases,
                    selected_elements,
                    total_input_content,
                    sciantix_variable,
                    sciantix_system);
                break;
        }
    }
}
