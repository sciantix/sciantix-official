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

#include <algorithm>
#include <cmath>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <vector>

namespace
{
// Caches the last OpenCalphad equilibrium actually solved for a location, so 
// SetPhaseDiagram() can skip the GEM on timesteps where the driving conditions 
// have not moved meaningfully.
//
struct PhaseDiagramCache
{
    bool                           has_result = false;
    std::string                    database;
    double                         temperature = 0.0;
    double                         pressure = 0.0;
    double                         oxygen_potential = 0.0;
    std::map<std::string, double>  fractions;
    OCOutputData                   output_data;  // normalized, NOT scaled by total content
    int                            stale_steps = 0;
};

PhaseDiagramCache matrix_cache;
PhaseDiagramCache grain_boundary_cache;

bool withinTolerance(double current_value, double cached_value, double relative_tolerance)
{
    const double reference = std::max(std::abs(current_value), std::abs(cached_value));
    if (reference <= 0.0)
        return true;

    return std::abs(current_value - cached_value) <= relative_tolerance * reference;
}

bool compositionMatches(const std::map<std::string, double>& cached_fractions,
                        const std::vector<InputComponent>&   current_components,
                        double                                tolerance)
{
    if (cached_fractions.size() != current_components.size())
        return false;

    for (const auto& component : current_components)
    {
        const auto cached_fraction = cached_fractions.find(component.name);
        if (cached_fraction == cached_fractions.end())
            return false;

        if (!withinTolerance(component.fraction, cached_fraction->second, tolerance))
            return false;
    }

    return true;
}

std::map<std::string, double> fractionsOf(const std::vector<InputComponent>& components)
{
    std::map<std::string, double> fractions;
    for (const auto& component : components)
        fractions[component.name] = component.fraction;
    return fractions;
}

bool cacheIsFresh(const PhaseDiagramCache&            cache,
                  const std::string&                   database,
                  double                                temperature,
                  double                                pressure,
                  double                                oxygen_potential,
                  bool                                  oxygen_potential_matters,
                  const std::vector<InputComponent>&   components,
                  const ThermochemistrySettings&        settings)
{
    if (!cache.has_result)
        return false;

    if (cache.database != database)
        return false;

    if (cache.stale_steps >= settings.coupling_max_stale_steps)
        return false;

    if (std::abs(temperature - cache.temperature) > settings.coupling_temperature_tolerance)
        return false;

    if (!withinTolerance(pressure, cache.pressure, settings.coupling_composition_tolerance))
        return false;

    if (oxygen_potential_matters &&
        !withinTolerance(oxygen_potential, cache.oxygen_potential, settings.coupling_composition_tolerance))
        return false;

    return compositionMatches(cache.fractions, components, settings.coupling_composition_tolerance);
}
}  // namespace

void Simulation::SetPhaseDiagram() // qui tutti eccetto i gas.
{
    auto moveFissionProductsWithoutThermochemistry = [&]()
    {
        for (auto& system : sciantix_system)
        {
            if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
            {
                sciantix_variable[system.getFissionProductName() + " at grain boundary"].setFinalValue(
                    sciantix_variable[system.getFissionProductName() + " reacted"].getFinalValue());
                sciantix_variable[system.getFissionProductName() + " reacted"].setFinalValue(0.0);
            }
            if (system.getRestructuredMatrix() == 0 && (system.isMetallicFP() || system.isCeramicFP()))
            {
                sciantix_variable[system.getFissionProductName() + " in solution"].setFinalValue(
                    sciantix_variable[system.getFissionProductName() + " produced"].getFinalValue());
                sciantix_variable[system.getFissionProductName() + " reacted"].setFinalValue(0.0);
            }
        }
    };

    // iThermochimica, exactly as set in input_settings.txt: 0 = off, 1 = smart
    // mode (all radial nodes of the outer/last axial slice run full
    // OpenCalphad, every other axial slice uses the simplified fixed
    // speciation in JOGFormation()), 2 = full OpenCalphad at every node.
    // Which call this is for comes from the separate iThermochimicaOuterNode
    // flag (set per call by FisPro3.f95).
    #if defined(COUPLING_TU)
        const int  thermochimica_mode = (int)input_variable["iThermochimica"].getValue();
        const bool is_outer_node       = input_variable["iThermochimicaOuterNode"].getValue() != 0;
        const bool run_full_opencalphad =
            thermochimica_mode == 2 || (thermochimica_mode == 1 && is_outer_node);
    #else
        const bool run_full_opencalphad = true;
    #endif

    if (!run_full_opencalphad || thermochemistry_settings == nullptr)
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

                solvers.push_back(OCSolver::FreshRecordRecovery);
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

        double total_input_content = 0.0;
        std::vector<InputComponent> components =
            OCUtilsCoupling::buildInputComponents(
                selected_elements,
                sciantix_variable,
                sciantix_system,
                total_input_content,
                location);

        if (total_input_content <= 0.0 || components.empty())
        {
            moveFissionProductsWithoutThermochemistry();
            return;
        }

        std::set<std::string> active_elements;
        for (const auto& component : components)
            active_elements.insert(component.name);
        const std::vector<std::string> valid_elements(active_elements.begin(), active_elements.end());

        PhaseDiagramCache& cache =
            (location_case == PhaseDiagramLocation::Matrix) ? matrix_cache : grain_boundary_cache;

        const double current_temperature      = history_variable["Temperature"].getFinalValue();
        const double current_pressure         = history_variable["System pressure"].getFinalValue();
        const double current_oxygen_potential = sciantix_variable["Fuel oxygen potential"].getFinalValue();
        const bool oxygen_potential_matters = (location == "at grain boundary");

        bool solved = false;
        OCOutputData output_data;

        if (cacheIsFresh(cache, location_settings->database, current_temperature, current_pressure,
                        current_oxygen_potential, oxygen_potential_matters, components,
                        Sciantix_thermochemistry_settings))
        {
            // Reuse the cached, normalized equilibrium as-is: it will be rescaled by the
            // *current* total_input_content below, exactly like a fresh solve would be.
            output_data = cache.output_data;
            solved = true;
            ++cache.stale_steps;
        }
        else
        {
            for (const auto& solver : solvers)
            {
                output_data.solution_phases.clear();
                output_data.components.clear();

                const bool case_success = OCUtilsCoupling::runOpenCalphadCaseOCASI(
                    data_path,
                    current_temperature,
                    current_pressure,
                    components,
                    valid_elements,
                    solver,
                    location,
                    current_oxygen_potential,
                    output_data);
                const bool has_usable_output = !output_data.solution_phases.empty();

                if (case_success && has_usable_output)
                {
                    solved = true;
                    break;
                }
            }

            if (solved)
            {
                cache.has_result      = true;
                cache.database         = location_settings->database;
                cache.temperature      = current_temperature;
                cache.pressure         = current_pressure;
                cache.oxygen_potential = current_oxygen_potential;
                cache.fractions        = fractionsOf(components);
                cache.output_data      = output_data;
                cache.stale_steps      = 0;
            }
            else
            {
                // The cache may only be reused while the last real solve succeeded.
                cache = PhaseDiagramCache{};
            }
        }

        if (!solved)
        {
            std::cerr << "Warning: all OpenCalphad attempts failed for " << location << std::endl;
            if (location_case == PhaseDiagramLocation::Matrix)
                continue;
        }

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
                    current_temperature,
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
