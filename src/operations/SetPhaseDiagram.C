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

#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <set>
#include <string>
#include <vector>

namespace
{
using TimingClock = std::chrono::steady_clock;

double elapsedMilliseconds(const TimingClock::time_point& start)
{
    return std::chrono::duration<double, std::milli>(TimingClock::now() - start).count();
}

std::string solverName(OCUtilsCoupling::OpenCalphadSolveMode solver)
{
    using OCSolver = OCUtilsCoupling::OpenCalphadSolveMode;

    switch (solver)
    {
        case OCSolver::SaveReadWarmStart:
            return "SaveReadWarmStart";
        case OCSolver::GlobalEquilibrium:
            return "GlobalEquilibrium";
        case OCSolver::PressureAxisStep:
            return "PressureAxisStep";
        case OCSolver::FixedOxygenMoles:
            return "FixedOxygenMoles";
        case OCSolver::OnlyC1MO2:
            return "OnlyC1MO2";
    }

    return "Unknown";
}

void appendThermochemistryTiming(const std::string& location,
                                 const std::string& scope,
                                 const std::string& solver,
                                 bool               success,
                                 bool               usable_output,
                                 double             elapsed_ms,
                                 std::size_t        component_count,
                                 double             total_input_content,
                                 std::size_t        phase_count)
{
    const std::string timing_file_path = TestPath + "thermochemistry_timing.txt";
    const bool write_header = !OCUtilsCoupling::fileExists(timing_file_path);
    std::ofstream timing_file(timing_file_path, std::ios::app);

    if (!timing_file)
        return;

    if (write_header)
    {
        timing_file << "Time (h)\tTime step\tLocation\tScope\tSolver\tSuccess\tUsable output\t"
                    << "Elapsed (ms)\tComponents\tTotal input content\tOutput phases\n";
    }

    timing_file << std::setprecision(12) << std::scientific
                << Time_h << "\t"
                << Time_step_number << "\t"
                << location << "\t"
                << scope << "\t"
                << solver << "\t"
                << success << "\t"
                << usable_output << "\t"
                << elapsed_ms << "\t"
                << component_count << "\t"
                << total_input_content << "\t"
                << phase_count << "\n";
}
}  // namespace

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
    const auto module_start = TimingClock::now();
    const bool preserve_matrix_oxygen_state = location == "at grain boundary";
    const double fuel_oxygen_potential =
        preserve_matrix_oxygen_state ? sciantix_variable["Fuel oxygen potential"].getFinalValue() : 0.0;
    const double fuel_oxygen_partial_pressure =
        preserve_matrix_oxygen_state ? sciantix_variable["Fuel oxygen partial pressure"].getFinalValue() : 0.0;
    const double calphad_oxygen_potential =
        preserve_matrix_oxygen_state ? sciantix_variable["Fuel oxygen potential - CALPHAD"].getFinalValue() : 0.0;
    const double calphad_oxygen_partial_pressure =
        preserve_matrix_oxygen_state ? sciantix_variable["Fuel oxygen partial pressure - CALPHAD"].getFinalValue() : 0.0;

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
    if (location == "matrix")
        solvers.push_back(OCSolver::OnlyC1MO2);
    else
        solvers.push_back(OCSolver::FixedOxygenMoles);
    // if (history_variable["System pressure"].getFinalValue() > 1.0e5 + 1.0)
    //     solvers.push_back(OCSolver::PressureAxisStep);


    // Path to OC files
    const std::string state_file_path = TestPath + "OC_" + category;
    const std::string data_path =
        Sciantix_thermochemistry_settings.opencalphad_path + "data/" + location_settings.database;
    
    // Needed later
    bool solved = false;
    OCOutputData output_data;
    double total_input_content = 0.0;
    std::vector<InputComponent> components =
    OCUtilsCoupling::buildInputComponents(selected_elements, sciantix_variable, sciantix_system, total_input_content, location);
    const std::vector<std::string> valid_elements(selected_elements.begin(), selected_elements.end());

    // Attempt for each solver
    for (const auto& solver : solvers)
    { 
        const auto solver_start = TimingClock::now();

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

        appendThermochemistryTiming(location,
                                    "solver_attempt",
                                    solverName(solver),
                                    case_success,
                                    has_usable_output,
                                    elapsedMilliseconds(solver_start),
                                    components.size(),
                                    total_input_content,
                                    output_data.solution_phases.size());

        if (!case_success || !has_usable_output)
            continue;

        // Mark as solved since we got results
        solved = true;
        break;
    }

    if (!solved)
        std::cout << "Warning: all OpenCalphad attempts failed for location: " << location << std::endl;

    appendThermochemistryTiming(location,
                                "solver_loop_total",
                                "all",
                                solved,
                                solved,
                                elapsedMilliseconds(module_start),
                                components.size(),
                                total_input_content,
                                output_data.solution_phases.size());
    
    if (location != "matrix")
    {
        const auto extract_start = TimingClock::now();
        const bool extracted = OCUtilsCoupling::getOpenCalphadResults(location, output_data);
        appendThermochemistryTiming(location,
                                    "extract_results",
                                    "all",
                                    extracted,
                                    !output_data.solution_phases.empty(),
                                    elapsedMilliseconds(extract_start),
                                    components.size(),
                                    total_input_content,
                                    output_data.solution_phases.size());
    }

    // // Debug
    // OCUtilsCoupling::dumpParsedOcOutput(output_data);

    if (Sciantix_thermochemistry_settings.output_phase_sublattice_composition)
    {
        const std::string sublattice_output_path =
            TestPath + "phase_sublattice_composition.txt";
        const auto write_start = TimingClock::now();
        const bool written = OCUtilsCoupling::writePhaseSublatticeCompositionOutput(
                sublattice_output_path,
                history_variable["Time"].getFinalValue(),
                location,
                output_data,
                total_input_content);
        appendThermochemistryTiming(location,
                                    "write_phase_sublattice_composition",
                                    "all",
                                    written,
                                    !output_data.solution_phases.empty(),
                                    elapsedMilliseconds(write_start),
                                    components.size(),
                                    total_input_content,
                                    output_data.solution_phases.size());
        if (!written)
        {
            std::cout << "Warning: cannot write phase sublattice composition output: "
                      << sublattice_output_path << std::endl;
        }
    }

    appendThermochemistryTiming(location,
                                "module_total",
                                "all",
                                solved,
                                !output_data.solution_phases.empty(),
                                elapsedMilliseconds(module_start),
                                components.size(),
                                total_input_content,
                                output_data.solution_phases.size());

    if (location == "matrix")
    {
        const auto update_start = TimingClock::now();
        OCUtilsCoupling::updateMatrixFromOutput(
            output_data, history_variable["Temperature"].getFinalValue(), sciantix_variable);
        appendThermochemistryTiming(location,
                                    "update_matrix_from_output",
                                    "all",
                                    true,
                                    !output_data.solution_phases.empty(),
                                    elapsedMilliseconds(update_start),
                                    components.size(),
                                    total_input_content,
                                    output_data.solution_phases.size());
        return;
    }
    else if (location == "at grain boundary")
    {
        const auto variable_update_start = TimingClock::now();
        OCUtilsCoupling::updateThermochemistryVariablesFromOutput(
            output_data.solution_phases,
            location,
            total_input_content,
            thermochemistry_variable,
            sciantix_variable);
        appendThermochemistryTiming(location,
                                    "update_thermochemistry_variables",
                                    "all",
                                    true,
                                    !output_data.solution_phases.empty(),
                                    elapsedMilliseconds(variable_update_start),
                                    components.size(),
                                    total_input_content,
                                    output_data.solution_phases.size());

        const auto grain_boundary_update_start = TimingClock::now();
        OCUtilsCoupling::updateGrainBoundaryFromOutput(
            output_data.solution_phases,
            selected_elements,
            total_input_content,
            sciantix_variable,
            sciantix_system);
        sciantix_variable["Fuel oxygen potential"].setFinalValue(fuel_oxygen_potential);
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(fuel_oxygen_partial_pressure);
        sciantix_variable["Fuel oxygen potential - CALPHAD"].setFinalValue(calphad_oxygen_potential);
        sciantix_variable["Fuel oxygen partial pressure - CALPHAD"].setFinalValue(calphad_oxygen_partial_pressure);
        appendThermochemistryTiming(location,
                                    "update_grain_boundary_from_output",
                                    "all",
                                    true,
                                    !output_data.solution_phases.empty(),
                                    elapsedMilliseconds(grain_boundary_update_start),
                                    components.size(),
                                    total_input_content,
                                    output_data.solution_phases.size());
        return;
    }
}
