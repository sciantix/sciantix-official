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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef THERMOCHEMISTRY_SETTINGS_H
#define THERMOCHEMISTRY_SETTINGS_H

#include <algorithm>
#include <string>
#include <vector>

struct ThermochemistryPhaseSettings
{
    std::string              module;
    std::string              database;
    std::vector<std::string> elements;
    std::vector<std::string> locations;

    bool hasElement(const std::string& element) const
    {
        return std::find(elements.begin(), elements.end(), element) != elements.end();
    }
};

struct ThermochemistrySettings
{
    std::string                  opencalphad_path                    = "";
    std::string                  thermochimica_path                  = "";
    bool                         output_phase_sublattice_composition = false;
    ThermochemistryPhaseSettings fission_products;
    ThermochemistryPhaseSettings matrix;

    // Lazy re-equilibration: SetPhaseDiagram() skips the OpenCalphad solve and reuses the
    // last converged (normalized) result whenever the driving conditions have not moved
    // by more than these tolerances since that solve, up to coupling_max_stale_steps.
    double coupling_temperature_tolerance = 5.0;     // K, absolute
    double coupling_composition_tolerance = 1.0e-3;  // relative
    int    coupling_max_stale_steps       = 20;

    // Matrix vaporisation (GrainVaporisation), e.g. annealing in a Knudsen cell: active from
    // matrix_vaporisation_start_time (h) on, when matrix_vaporisation is true.
    bool   matrix_vaporisation            = false;
    double matrix_vaporisation_start_time = 0.0;  // h
    // Knudsen cell: the vapour effuses through an orifice of diameter matrix_vaporisation_orifice_diameter (m),
    // Clausing factor matrix_vaporisation_clausing_factor, from a sample of mass matrix_vaporisation_sample_mass (kg).
    // Without orifice diameter or sample mass, the matrix vaporises freely from the grain surface.
    double matrix_vaporisation_orifice_diameter = 0.0;  // m
    double matrix_vaporisation_sample_mass      = 0.0;  // kg
    double matrix_vaporisation_clausing_factor  = 1.0;  // (/)
};

ThermochemistrySettings LoadThermochemistrySettings(const std::string& path);

#endif
