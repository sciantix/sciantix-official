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
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, G. Zullo, G. Nicodemo                                     //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"
#include "MainVariables.h"
#include "OCOutputParser.h"
#include "ThermochemistryManifest.h"
#include "ThermochemistrySettings.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <map>     
#include <cmath>
#include <limits>
#include <string>
#include <set>

namespace
{
std::string stripTdbExtension(const std::string& database_name)
{
    if (database_name.size() >= 4)
    {
        const std::string suffix = database_name.substr(database_name.size() - 4);
        if (suffix == ".TDB" || suffix == ".tdb")
            return database_name.substr(0, database_name.size() - 4);
    }

    return database_name;
}
}

namespace
{


double computeCalphadOxygenPartialPressure(const OCOutputData& output_data, double total_pressure_pa)
{
    const auto oxygen_component = output_data.components.find("O");
    if (oxygen_component != output_data.components.end() && oxygen_component->second.activity > 0.0)
    {
        return reference_oxygen_pressure * std::pow(oxygen_component->second.activity, 2.0);
    }

    const auto gas_phase = output_data.solution_phases.find("gas");
    if (gas_phase == output_data.solution_phases.end() || gas_phase->second.moles <= 0.0)
        return 0.0;

    const auto oxygen_species = gas_phase->second.species.find("O2");
    if (oxygen_species == gas_phase->second.species.end() || oxygen_species->second.moles <= 0.0)
        return 0.0;

    return oxygen_species->second.moles / gas_phase->second.moles * total_pressure_pa * 1.0e-6;
}
}  // namespace

void Simulation::SetPhaseDiagram(std::string location)
{
    if (location == "at grain boundary")
    {
        if (input_variable["iThermochimica"].getValue() == 0 || sciantix_variable["Xe at grain boundary"].getInitialValue() <= 0.0) 
        {
            for (auto &system : sciantix_system)
            {
                if (system.getRestructuredMatrix() == 0 && system.getGas().getChemicallyActive() == 1.0)
                {
                    sciantix_variable[system.getGasName() + " at grain boundary"].addValue(sciantix_variable[system.getGasName() + " reacted - GB"].getFinalValue());
                    sciantix_variable[system.getGasName() + " reacted - GB"].setFinalValue(0);
                }
            }
        }
        else 
        {
            double temperature = history_variable["Temperature"].getFinalValue();
            double pressure = history_variable["System pressure"].getFinalValue();

            CallThermochemistryModule(pressure, temperature, location, sciantix_variable, std::nan(""));
        }

    }
    else if (location == "matrix")
    {
        if (input_variable["iThermochimica"].getValue() != 0)
        {
            double temperature = history_variable["Temperature"].getFinalValue();
            double pressure = history_variable["System pressure"].getFinalValue();
            double stoicdev = sciantix_variable["Stoichiometry deviation"].getFinalValue();
            double oxygenfraction = (stoicdev + 2.0)/(1 + stoicdev + 2.0);
            CallThermochemistryModule(pressure, temperature, location, sciantix_variable, oxygenfraction);
        }
    }
    else
    {
        std::cout<<"Location not yet modelled: "<<location<<std::endl;
    }

    return;
}


void Simulation::CallThermochemistryModule(double pressure, double temperature, std::string location, SciantixArray<SciantixVariable> &sciantix_variable, double oxygenfraction)
{
    const std::vector<ThermochemistryManifestEntry> manifest =
        loadThermochemistryManifest(TestPath + "input_thermochemistry.txt");
    const ThermochemistrySettings settings = loadThermochemistrySettings(TestPath + "input_thermochemistry_settings.txt");
    const std::vector<ThermochemistryManifestEntry> filtered_manifest = filterThermochemistryManifest(manifest, settings);
    const std::string category = (location == "matrix") ? "matrix" : "fission_products";
    const std::set<std::string> manifest_elements = getThermochemistryElements(filtered_manifest, category, location);
    const ThermochemistryPhaseSettings& location_settings =
        (location == "matrix") ? settings.matrix : settings.fission_products;
    const std::string module = location_settings.module;
    
    if (module == "OPENCALPHAD")
    {
        std::string directoryPath = "./../../../opencalphad/";
        std::string inputPath = "inputs/thermoin.OCM";
        std::string outputPath = "outputs/thermoout";
        std::string dataPath = "./../data/" + stripTdbExtension(location_settings.database);
        std::string exePath = directoryPath + "oc6P " + directoryPath + inputPath;
        
        // 1. Write input file
        std::ofstream inputFile(directoryPath + inputPath);
        if (!inputFile) 
        {
            std::cerr << "Error: Cannot create input file: " << inputPath << std::endl;
            return;
        }

        inputFile << "@$ Initialize variables:\n";
        inputFile << "r t " <<dataPath<<"\n";
        inputFile << "\nset c t=" << temperature << " p="<<pressure<<" ";

        bool noatoms = true;

        for (const auto& elementName : manifest_elements)
        {
            double atomsavailable(0);
            if (location =="at grain boundary")
            {
                if (elementName == "O")
                {
                    atomsavailable = 0.1; // placeholder
                    // double chem_pot_oxy = 8.314*1e-3*history_variable["Temperature"].getFinalValue()*log(sciantix_variable["Fuel oxygen partial pressure"].getFinalValue()/0.1013);
                    // inputFile << "mu(" << elementName << ")=" << chem_pot_oxy << " ";
                    double activity = sciantix_variable["Fuel oxygen partial pressure"].getFinalValue()/pressure;
                    inputFile << "ac(" << elementName << ")=" << activity << " ";
                }
                else
                {
                    atomsavailable = (
                        sciantix_variable[elementName + " produced"].getFinalValue() -
                        sciantix_variable[elementName + " decayed"].getFinalValue() -
                        sciantix_variable[elementName + " in grain"].getFinalValue() -
                        sciantix_variable[elementName + " released"].getInitialValue()
                    );
                    inputFile << "n(" << elementName << ")=" << atomsavailable/avogadro_number << " ";
                }
            }
            else if (location == "matrix")
            {
                atomsavailable = 0.1; // placeholder
                inputFile << "n=1 x(o)=" << oxygenfraction << " ";
                // sciantix_variable["Uranium content"].getFinalValue() + sciantix_variable["Oxygen content"].getFinalValue(); 
                // For matrix, we consider Uranium and Oxygen content, these are MOLES/grain not atoms
                noatoms = false;
                break;
            }
            else
                std::cout<<"Location not yet modelled: "<<location<<std::endl;
            
            if (atomsavailable > 0.0) noatoms = false;
        }

        if (noatoms) return;

        inputFile << "\n\nc e\n\n";
        inputFile << "l /out=./../"<<outputPath<<".DAT r 1\n\n"; 
        inputFile << "fin"; 
        inputFile.close();

        // 2. Execute the external script: OPENCALPHAD
        int status = std::system(exePath.c_str());
        if (status != 0) 
        {
            std::cerr << "Error: Execution of  OPENCALPHAD failed!\n";
            return;
        }

        const std::vector<std::string> valid_elements(manifest_elements.begin(), manifest_elements.end());
        const OCOutputData output_data = parseOCOutputFile(directoryPath + outputPath + ".DAT", valid_elements);
        const auto& SolutionPhases = output_data.solution_phases;

        for (const auto& phase_entry : SolutionPhases)
        {
            const std::string& phase_name = phase_entry.first;
            const OCPhaseData& phase_data = phase_entry.second;

            if (!phase_data.species.empty())
            {
                for (const auto& species_entry : phase_data.species)
                {
                    const std::string& compound = species_entry.first;
                    const double       moles    = species_entry.second.moles;

                    if (thermochemistry_variable.isElementPresent(compound + " (" + phase_name + ", " + location + ")"))
                        thermochemistry_variable[compound + " (" + phase_name + ", " + location + ")"].setFinalValue(moles);
                }
            }
            else
            {
                for (const auto& element_entry : phase_data.elements)
                {
                    const std::string& compound = element_entry.first;
                    const double       moles    = element_entry.second;

                    if (thermochemistry_variable.isElementPresent(compound + " (" + phase_name + ", " + location + ")"))
                        thermochemistry_variable[compound + " (" + phase_name + ", " + location + ")"].setFinalValue(moles);
                }
            }
        }

        if (location == "matrix")
        {
            const double calphad_oxygen_partial_pressure =
                computeCalphadOxygenPartialPressure(output_data, pressure);

            sciantix_variable["Fuel oxygen partial pressure - CALPHAD"].setFinalValue(calphad_oxygen_partial_pressure);
            // Fuel oxygen potential
            if (sciantix_variable["Fuel oxygen partial pressure - CALPHAD"].getFinalValue() == 0.0)
                sciantix_variable["Fuel oxygen potential - CALPHAD"].setFinalValue(0.0);
            else
                sciantix_variable["Fuel oxygen potential - CALPHAD"].setFinalValue(
                    8.314 * 1.0e-3 * history_variable["Temperature"].getFinalValue() *
                    log(sciantix_variable["Fuel oxygen partial pressure - CALPHAD"].getFinalValue() / reference_oxygen_pressure));

            if (calphad_oxygen_partial_pressure > 0.0)
            {
                sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(calphad_oxygen_partial_pressure);
                sciantix_variable["Fuel oxygen potential"].setFinalValue(
                    sciantix_variable["Fuel oxygen potential - CALPHAD"].getFinalValue());
            }

            Matrix fuel(matrices[0]);
            const auto gas_phase = SolutionPhases.find("gas");
            if (gas_phase != SolutionPhases.end())
            {
                double total_moles = sciantix_variable["Uranium content"].getFinalValue() + sciantix_variable["Oxygen content"].getFinalValue();
                double mol_u = gas_phase->second.elements.count("U") ? gas_phase->second.elements.at("U") * total_moles : 0.0;
                double mol_o = gas_phase->second.elements.count("O") ? gas_phase->second.elements.at("O") * total_moles : 0.0;
                
                sciantix_variable["Grain radius"].setFinalValue(sciantix_variable["Grain radius"].getFinalValue() * pow((total_moles - mol_u - mol_o)/total_moles, 1.0/3.0));
            }
            return;
        }

        // elements: update gas atoms and grain boundary variables
        for (const auto& element : manifest_elements) {
            double moles(0);
            const auto gas_phase = SolutionPhases.find("gas");
            if (gas_phase != SolutionPhases.end() && gas_phase->second.elements.count(element) > 0)
                moles = gas_phase->second.elements.at(element);
        
            double available(0);
            if (location =="at grain boundary")
            {
                if (element == "O")
                {
                    available = 0.1;
                } 
                else
                {
                    available = (sciantix_variable[element + " produced"].getFinalValue()
                        - sciantix_variable[element + " decayed"].getFinalValue()
                        - sciantix_variable[element + " in grain"].getFinalValue()
                        - sciantix_variable[element + " released"].getInitialValue()
                    );
                }
            }
            else
                std::cout<<"Location not yet modelled: "<<location<<std::endl;
            
            double updateAtoms = std::min(available, moles * avogadro_number);

            if (location =="at grain boundary")
            {
                if (element =="O") continue;

                sciantix_variable[element + " at grain boundary"].setFinalValue(updateAtoms);
                sciantix_variable[element + " reacted - GB"].setFinalValue(available - updateAtoms);    
            }
            else
                std::cout<<"Location not yet modelled: "<<location<<std::endl;    
        }
    }
    else
    {
        if (location == "at grain boundary")
        {
            for (auto &system : sciantix_system)
            {
                if (system.getRestructuredMatrix() == 0 && system.getGas().getChemicallyActive() == 1.0)
                {
                    sciantix_variable[system.getGasName() + " at grain boundary"].addValue(sciantix_variable[system.getGasName() + " reacted - GB"].getFinalValue());
                    sciantix_variable[system.getGasName() + " reacted - GB"].setFinalValue(0);
                }
            }
            return;
        }
        else
        {
            std::cout<<"Location not yet modelled: "<<location<<std::endl;
            return;
        }
        return;
    }
}
