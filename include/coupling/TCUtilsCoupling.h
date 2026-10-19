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

#ifndef TC_UTILS_COUPLING_H
#define TC_UTILS_COUPLING_H

// Thermochimica coupling (fission-product chemistry at the grain boundary only).
//
// It mirrors the OpenCalphad coupling: Thermochimica is called in-process through its
// TCAPI C interface and its results are stored in the same backend-neutral data
// structures declared in OCUtilsCoupling.h (OCOutputData, OCPhaseData, InputComponent, ...),
// so SetPhaseDiagram() handles both couplings with a single output type and cache.
#include "OCUtilsCoupling.h"
#include <set>
#include <string>
#include <vector>

namespace TCUtilsCoupling
{
    enum class ThermochimicaSolveMode
    {
        // Warm start from the last converged assemblage (Thermochimica reinit data).
        ReinitWarmStart,
        // Full leveling + GEM solve, without reinit data.
        ColdStart,
        // Last-chance fallback: all Thermochimica state is reset and the database is
        // parsed again before a cold solve, so the retry is not biased by a failed history.
        FreshParseRecovery
    };

    bool writePhaseSublatticeCompositionOutput(const std::string&  file_path,
                                               double              time_hours,
                                               const std::string&  location,
                                               const OCOutputData& output_data,
                                               double              content_scaling_factor);

    std::vector<InputComponent> buildInputComponents(const std::set<std::string>&     selected_elements,
                                                     SciantixArray<SciantixVariable>& sciantix_variable,
                                                     SciantixArray<System>&           sciantix_system,
                                                     double&                          total_content,
                                                     const std::string&               location);

    bool runThermochimicaCase(const std::string&                 database_path,
                              double                             temperature,
                              double                             pressure,
                              const std::vector<InputComponent>& components,
                              ThermochimicaSolveMode             solve_mode,
                              OCOutputData&                      output_data);

    bool validateThermochimicaOutput(const OCOutputData&                output_data,
                                     const std::vector<InputComponent>& input_components,
                                     const std::string&                 location);

    void updateThermochemistryVariablesFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                                  const std::string&                        location,
                                                  double                                    content_scaling_factor,
                                                  SciantixArray<ThermochemistryVariable>&   thermochemistry_variable);

    void updateGrainBoundaryFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                       const std::set<std::string>&              selected_elements,
                                       double                                    content_scaling_factor,
                                       SciantixArray<SciantixVariable>&          sciantix_variable,
                                       SciantixArray<System>&                    sciantix_system);
}  // namespace TCUtilsCoupling

#endif
