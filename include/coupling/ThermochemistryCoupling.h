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

#ifndef THERMOCHEMISTRY_COUPLING_H
#define THERMOCHEMISTRY_COUPLING_H

// Entry point of SetPhaseDiagram() to the thermochemistry codes. Each call is routed to the module
// set in input_thermochemistry_settings.txt (OPENCALPHAD -> OCUtilsCoupling, THERMOCHIMICA -> TCUtilsCoupling).
// Both modules store their results in the data structures declared in OCUtilsCoupling.h.
#include "OCUtilsCoupling.h"
#include "ThermochemistrySettings.h"

#include <map>
#include <set>
#include <string>
#include <vector>

namespace ThermochemistryCoupling
{
    // True when the module is compiled in (USE_OPENCALPHAD, USE_THERMOCHIMICA).
    bool isAvailable(const std::string& module);

    std::string
    databasePath(const std::string& module, const ThermochemistrySettings& settings, const std::string& database);

    // True when the equilibrium depends on the fuel oxygen potential: only the OpenCalphad grain-boundary
    // system is open to oxygen (imposed matrix potential), the Thermochimica system is closed.
    bool isOpenToOxygen(const std::string& module, const std::string& location);

    std::vector<InputComponent> buildInputComponents(const std::string&               module,
                                                     const std::set<std::string>&     selected_elements,
                                                     SciantixArray<SciantixVariable>& sciantix_variable,
                                                     SciantixArray<System>&           sciantix_system,
                                                     double&                          total_content,
                                                     const std::string&               location);

    // Runs the module solvers in order (warm start first, then the more robust and expensive fallbacks)
    // until one returns a usable equilibrium.
    bool solveEquilibrium(const std::string&                 module,
                          const std::string&                 database_path,
                          double                             temperature,
                          double                             pressure,
                          const std::vector<InputComponent>& components,
                          const std::vector<std::string>&    valid_elements,
                          const std::string&                 location,
                          double                             oxygen_potential,
                          OCOutputData&                      output_data);

    bool writePhaseSublatticeCompositionOutput(const std::string&  module,
                                               const std::string&  file_path,
                                               double              time_hours,
                                               const std::string&  location,
                                               const OCOutputData& output_data,
                                               double              content_scaling_factor);

    void updateThermochemistryVariablesFromOutput(const std::string&                        module,
                                                  const std::map<std::string, OCPhaseData>& solution_phases,
                                                  const std::string&                        location,
                                                  double                                    content_scaling_factor,
                                                  SciantixArray<ThermochemistryVariable>&   thermochemistry_variable,
                                                  SciantixArray<SciantixVariable>&          sciantix_variable);

    void updateMatrixFromOutput(const std::string&               module,
                                const OCOutputData&              output_data,
                                double                           temperature,
                                SciantixArray<SciantixVariable>& sciantix_variable);

    void updateGrainBoundaryFromOutput(const std::string&                        module,
                                       const std::map<std::string, OCPhaseData>& solution_phases,
                                       const std::set<std::string>&              selected_elements,
                                       double                                    content_scaling_factor,
                                       SciantixArray<SciantixVariable>&          sciantix_variable,
                                       SciantixArray<System>&                    sciantix_system);
}  // namespace ThermochemistryCoupling

#endif
