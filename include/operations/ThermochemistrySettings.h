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

#ifndef THERMOCHEMISTRY_SETTINGS_H
#define THERMOCHEMISTRY_SETTINGS_H

#include <string>
#include <vector>

struct ThermochemistryPhaseSettings
{
    std::string              module;
    std::string              database;
    std::vector<std::string> elements;
    std::vector<std::string> locations;
    bool                     gap_settings    = false;
    double                   gap_temperature = 0.0;
    double                   gap_pressure    = 0.0;
};

struct ThermochemistrySettings
{
    std::string                  opencalphad_path      = "";
    bool                         kc                   = false;
    double                       kc_time              = 0.0;
    bool                         langmuir             = false;
    double                       langmuir_coefficient = 0.0;
    bool                         output_phase_sublattice_composition = false;
    ThermochemistryPhaseSettings fission_products;
    ThermochemistryPhaseSettings matrix;

    // Lazy re-equilibration: SetPhaseDiagram() skips the OpenCalphad solve and reuses the
    // last converged (normalized) result whenever the driving conditions have not moved
    // by more than these tolerances since that solve, up to coupling_max_stale_steps.
    double                       coupling_temperature_tolerance = 5.0;    // K, absolute
    double                       coupling_composition_tolerance = 1.0e-3; // relative
    int                          coupling_max_stale_steps       = 20;
};

ThermochemistrySettings loadThermochemistrySettings(const std::string& path);

#endif
