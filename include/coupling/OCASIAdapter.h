//////////////////////////////////////////////////////////////////////////////////////
//       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
//      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
//     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
//      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
//  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
//  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
//                                                                                  //
//  OCASI Adapter                                                                  //
//  Direct C++ interface to OpenCalphad Fortran core via OCASI bindings             //
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

class OpenCalphadInterface
{
public:
    OpenCalphadInterface();
    ~OpenCalphadInterface();

    bool loadDatabase(const std::string& tdb_file_path,
                      const std::vector<std::string>& selected_elements);
    bool setConditions(double temperature,
                       double pressure,
                       const std::map<std::string, double>& components);
    bool setReferenceState(const std::string& component_name,
                           const std::string& phase_name,
                           double temperature,
                           double pressure);
    bool removeComponentCondition(const std::string& component_name);
    bool setComponentPotential(const std::string& component_name,
                               double chemical_potential);
    bool setPhaseStatus(const std::string& phase_name,
                        int status,
                        double value);
    bool calculateEquilibrium(bool suspend_gas);
    bool extractResults(OCOutputData& output_data);
    void reset(bool clear_database);

private:
    int getComponentIndex(const std::string& component_name) const;
    std::string getPhaseNameAtIndex(int phase_index) const;
    int getPhaseIndex(const std::string& phase_name) const;

    void* ceq_ = nullptr;
    bool database_loaded_ = false;
    int nel_ = 0;
    std::vector<std::string> element_names_;
};

OpenCalphadInterface& getOpenCalphadInterface();

}  // namespace OCASIAdapter

#endif
