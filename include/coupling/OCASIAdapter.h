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

#ifndef OCASI_ADAPTER_H
#define OCASI_ADAPTER_H

#include "OCUtilsCoupling.h"

#include <map>
#include <string>
#include <vector>

namespace OCASIAdapter
{
    enum class OpenCalphadContext
    {
        Matrix,
        FissionProducts
    };

    class OpenCalphadInterface
    {
    public:
        OpenCalphadInterface();
        ~OpenCalphadInterface();

        bool loadDatabase(const std::string &tdb_file_path,
                          const std::vector<std::string> &selected_elements);
        bool ensureDatabaseLoaded(const std::string &tdb_file_path,
                                  const std::vector<std::string> &selected_elements);
        bool setConditions(double temperature,
                           double pressure,
                           const std::map<std::string, double> &components);
        bool setPressure(double pressure);
        bool setReferenceState(const std::string &component_name,
                               const std::string &phase_name,
                               double temperature,
                               double pressure);
        bool removeComponentCondition(const std::string &component_name);
        bool setComponentPotential(const std::string &component_name,
                                   double chemical_potential);
        bool setPhaseStatus(const std::string &phase_name,
                            int status,
                            double value);
        bool calculateEquilibrium(int grid_minimizer);
        bool calculateEquilibriumChecked();
        bool extractResults(OCOutputData &output_data);
        void reset(bool clear_database);

    private:
        int getComponentIndex(const std::string &component_name) const;
        std::string getPhaseNameAtIndex(int phase_index) const;
        int getPhaseIndex(const std::string &phase_name) const;

        void *ceq_ = nullptr;
        bool database_loaded_ = false;
        std::string loaded_database_path_;
        std::vector<std::string> loaded_selected_elements_;
        int nel_ = 0;
        std::vector<std::string> element_names_;
    };

    OpenCalphadInterface &getOpenCalphadInterface(OpenCalphadContext context);

} // namespace OCASIAdapter

#endif
