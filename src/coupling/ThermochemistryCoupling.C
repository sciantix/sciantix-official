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

#include "ThermochemistryCoupling.h"

#ifdef USE_THERMOCHIMICA
#include "TCUtilsCoupling.h"
#endif

namespace ThermochemistryCoupling
{
    namespace
    {
        // Silences -Wunused-parameter for the arguments of the modules not compiled in
        template <typename... Args> void ignoreUnused(const Args&...)
        {
        }
    }  // namespace

    bool isAvailable(const std::string& module)
    {
#ifdef USE_OPENCALPHAD
        if (module == "OPENCALPHAD")
            return true;
#endif
#ifdef USE_THERMOCHIMICA
        if (module == "THERMOCHIMICA")
            return true;
#endif
        ignoreUnused(module);
        return false;
    }

    std::string
    databasePath(const std::string& module, const ThermochemistrySettings& settings, const std::string& database)
    {
        if (module == "THERMOCHIMICA")
            return settings.thermochimica_path + "data/" + database;
        return settings.opencalphad_path + "data/" + database;
    }

    bool isOpenToOxygen(const std::string& module, const std::string& location)
    {
        return module == "OPENCALPHAD" && location == "at grain boundary";
    }

    std::vector<InputComponent> buildInputComponents(const std::string&               module,
                                                     const std::set<std::string>&     selected_elements,
                                                     SciantixArray<SciantixVariable>& sciantix_variable,
                                                     SciantixArray<System>&           sciantix_system,
                                                     double&                          total_content,
                                                     const std::string&               location)
    {
        total_content = 0.0;
#ifdef USE_OPENCALPHAD
        if (module == "OPENCALPHAD")
            return OCUtilsCoupling::buildInputComponents(
                selected_elements, sciantix_variable, sciantix_system, total_content, location);
#endif
#ifdef USE_THERMOCHIMICA
        if (module == "THERMOCHIMICA")
            return TCUtilsCoupling::buildInputComponents(
                selected_elements, sciantix_variable, sciantix_system, total_content, location);
#endif
        ignoreUnused(module, selected_elements, sciantix_variable, sciantix_system, location);
        return {};
    }

    bool solveEquilibrium(const std::string&                 module,
                          const std::string&                 database_path,
                          double                             temperature,
                          double                             pressure,
                          const std::vector<InputComponent>& components,
                          const std::vector<std::string>&    valid_elements,
                          const std::string&                 location,
                          double                             oxygen_potential,
                          OCOutputData&                      output_data)
    {
#ifdef USE_OPENCALPHAD
        if (module == "OPENCALPHAD")
        {
            using OCSolver = OCUtilsCoupling::OpenCalphadSolveMode;
            std::vector<OCSolver> solvers;
#if !defined(COUPLING_TU)
            solvers.push_back(OCSolver::SaveReadWarmStart);
#endif
            solvers.push_back(OCSolver::GlobalEquilibrium);
            solvers.push_back(location == "matrix" ? OCSolver::OnlyC1MO2 : OCSolver::FreshRecordRecovery);

            for (const auto& solver : solvers)
            {
                output_data.solution_phases.clear();
                output_data.components.clear();

                const bool case_success      = OCUtilsCoupling::runOpenCalphadCaseOCASI(database_path,
                                                                                   temperature,
                                                                                   pressure,
                                                                                   components,
                                                                                   valid_elements,
                                                                                   solver,
                                                                                   location,
                                                                                   oxygen_potential,
                                                                                   output_data);
                const bool has_usable_output = !output_data.solution_phases.empty();

                if (case_success && has_usable_output)
                    return true;
            }
            return false;
        }
#endif
#ifdef USE_THERMOCHIMICA
        if (module == "THERMOCHIMICA")
        {
            using TCSolver = TCUtilsCoupling::ThermochimicaSolveMode;
            std::vector<TCSolver> solvers;
#if !defined(COUPLING_TU)
            solvers.push_back(TCSolver::ReinitWarmStart);
#endif
            solvers.push_back(TCSolver::ColdStart);
            solvers.push_back(TCSolver::FreshParseRecovery);

            for (const auto& solver : solvers)
            {
                output_data.solution_phases.clear();
                output_data.components.clear();

                const bool case_success =
                    TCUtilsCoupling::runThermochimicaCase(
                        database_path, temperature, pressure, components, solver, output_data) &&
                    TCUtilsCoupling::validateThermochimicaOutput(output_data, components, location);
                const bool has_usable_output = !output_data.solution_phases.empty();

                if (case_success && has_usable_output)
                    return true;
            }
            output_data.solution_phases.clear();
            output_data.components.clear();
            return false;
        }
#endif
        ignoreUnused(module,
                     database_path,
                     temperature,
                     pressure,
                     components,
                     valid_elements,
                     location,
                     oxygen_potential,
                     output_data);
        return false;
    }

    bool writePhaseSublatticeCompositionOutput(const std::string&  module,
                                               const std::string&  file_path,
                                               double              time_hours,
                                               const std::string&  location,
                                               const OCOutputData& output_data,
                                               double              content_scaling_factor)
    {
#ifdef USE_OPENCALPHAD
        if (module == "OPENCALPHAD")
            return OCUtilsCoupling::writePhaseSublatticeCompositionOutput(
                file_path, time_hours, location, output_data, content_scaling_factor);
#endif
#ifdef USE_THERMOCHIMICA
        if (module == "THERMOCHIMICA")
            return TCUtilsCoupling::writePhaseSublatticeCompositionOutput(
                file_path, time_hours, location, output_data, content_scaling_factor);
#endif
        ignoreUnused(module, file_path, time_hours, location, output_data, content_scaling_factor);
        return false;
    }

    void updateThermochemistryVariablesFromOutput(const std::string&                        module,
                                                  const std::map<std::string, OCPhaseData>& solution_phases,
                                                  const std::string&                        location,
                                                  double                                    content_scaling_factor,
                                                  SciantixArray<ThermochemistryVariable>&   thermochemistry_variable,
                                                  SciantixArray<SciantixVariable>&          sciantix_variable)
    {
#ifdef USE_OPENCALPHAD
        if (module == "OPENCALPHAD")
            OCUtilsCoupling::updateThermochemistryVariablesFromOutput(
                solution_phases, location, content_scaling_factor, thermochemistry_variable, sciantix_variable);
#endif
#ifdef USE_THERMOCHIMICA
        if (module == "THERMOCHIMICA")
            TCUtilsCoupling::updateThermochemistryVariablesFromOutput(
                solution_phases, location, content_scaling_factor, thermochemistry_variable);
#endif
        ignoreUnused(
            module, solution_phases, location, content_scaling_factor, thermochemistry_variable, sciantix_variable);
    }

    void updateMatrixFromOutput(const std::string&               module,
                                const OCOutputData&              output_data,
                                double                           temperature,
                                SciantixArray<SciantixVariable>& sciantix_variable)
    {
        // Thermochimica is coupled at the grain boundary only
#ifdef USE_OPENCALPHAD
        if (module == "OPENCALPHAD")
            OCUtilsCoupling::updateMatrixFromOutput(output_data, temperature, sciantix_variable);
#endif
        ignoreUnused(module, output_data, temperature, sciantix_variable);
    }

    void updateGrainBoundaryFromOutput(const std::string&                        module,
                                       const std::map<std::string, OCPhaseData>& solution_phases,
                                       const std::set<std::string>&              selected_elements,
                                       double                                    content_scaling_factor,
                                       SciantixArray<SciantixVariable>&          sciantix_variable,
                                       SciantixArray<System>&                    sciantix_system)
    {
#ifdef USE_OPENCALPHAD
        if (module == "OPENCALPHAD")
            OCUtilsCoupling::updateGrainBoundaryFromOutput(
                solution_phases, selected_elements, content_scaling_factor, sciantix_variable, sciantix_system);
#endif
#ifdef USE_THERMOCHIMICA
        if (module == "THERMOCHIMICA")
            TCUtilsCoupling::updateGrainBoundaryFromOutput(
                solution_phases, selected_elements, content_scaling_factor, sciantix_variable, sciantix_system);
#endif
        ignoreUnused(
            module, solution_phases, selected_elements, content_scaling_factor, sciantix_variable, sciantix_system);
    }
}  // namespace ThermochemistryCoupling
