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

#ifndef OC_UTILS_COUPLING_H
#define OC_UTILS_COUPLING_H

#include "Simulation.h"

#include <map>
#include <set>
#include <string>
#include <vector>

struct OCSublatticeData
{
    int                           index = 0;
    int                           constituents_count = 0;
    double                        sites = 0.0;
    double                        phase_moles = 0.0;
    double                        phase_form_units = 0.0;
    std::string                   phase_instance;
    std::map<std::string, double> composition;
};

struct OCSpeciesData
{
    double                   moles               = 0.0;
    double                   atom_equivalent_moles = 0.0;
    double                   volume              = 0.0;
    double                   stoichiometric_size = 1.0;
    std::map<std::string, double> elements;
    std::vector<OCSublatticeData> sublattices;
};

struct OCPhaseData
{
    double                   moles  = 0.0;
    double                   volume = 0.0;
    double                   form_units = 0.0;
    std::map<std::string, OCSpeciesData> species;
    std::map<std::string, double>        elements;
    std::vector<OCSublatticeData>        sublattices;
};

struct OCComponentData
{
    double      moles                        = 0.0;
    double      mole_fraction                = 0.0;
    double      chemical_potential_over_rt   = 0.0;
    double      activity                     = 0.0;
};

struct OCOutputData
{
    std::map<std::string, OCPhaseData>     solution_phases;
    std::map<std::string, OCComponentData> components;
};

struct InputComponent
{
    std::string name;
    double      content  = 0.0;
    double      fraction = 0.0;
};


OCOutputData parseOCOutputFile(const std::string& filepath, const std::vector<std::string>& valid_elements);

namespace OCUtilsCoupling
{
enum class OpenCalphadSolveMode
{
    SaveReadWarmStart,
    GlobalEquilibrium,
    PressureAxisStepGlobalEquilibrium,
    FixedOxygenMolesFromInvalidPotentialSolve,
    OnlyC1MO2
};

std::string solveModeLabel(OpenCalphadSolveMode mode);

std::string readTextFile(const std::string& file_path);
bool fileExists(const std::string& file_path);

bool hasInvalidEquilibriumResult(const std::string& output_text);
bool tryGetOxygenMolesFromOutput(const std::string& output_file_path,
                                 const std::set<std::string>& active_elements,
                                 double& oxygen_moles);

// Debug
void dumpParsedOcOutput(const OCOutputData& output_data);
bool writePhaseSublatticeCompositionOutput(const std::string& file_path,
                                           double             time_hours,
                                           const std::string& location,
                                           const OCOutputData& output_data,
                                           double             content_scaling_factor);
std::vector<InputComponent> buildInputComponents(
     const std::set<std::string>&     selected_elements,
     SciantixArray<SciantixVariable>& sciantix_variable,
     SciantixArray<System>&           sciantix_system,
     double&                          total_content);
bool writeOpenCalphadInput(const std::string& state_file_path,
                           const std::string& data_path,
                           double             pressure,
                           double             temperature,
                           OpenCalphadSolveMode solve_mode,
                           const std::string& location,
                           std::vector<InputComponent> components,
                           SciantixArray<SciantixVariable>& sciantix_variable,
                           double                 fixed_oxygen_moles);
bool runOpenCalphadCase(const std::string& executable);
void updateThermochemistryVariablesFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                              const std::string&                         location,
                                              double                                     content_scaling_factor,
                                              SciantixArray<ThermochemistryVariable>&    thermochemistry_variable);
void updateMatrixFromOutput(const OCOutputData&              output_data,
                            double                           temperature,
                            SciantixArray<SciantixVariable>& sciantix_variable);
void updateGrainBoundaryFromOutput(const std::map<std::string, OCPhaseData>& solution_phases,
                                   const std::set<std::string>&               selected_elements,
                                   double                                     content_scaling_factor,
                                   SciantixArray<SciantixVariable>&           sciantix_variable,
                                   SciantixArray<System>&                     sciantix_system);
}  // namespace OCUtilsCoupling

#endif
