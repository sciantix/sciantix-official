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
#include <json/json.h>
#include <set>

void Simulation::SetPhaseDiagram(std::string location)
{
    double oxygenfraction(std::nan("")); // Oxygen fraction defined only for location = matrix
    
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

            return;
        }
    }
    else if (location == "matrix")
    {
        double stoicdev = sciantix_variable["Stoichiometry deviation"].getFinalValue();
        oxygenfraction = (stoicdev + 2.0)/(1 + stoicdev + 2.0);
    }
    else
    {
        std::cout<<"Location not yet modelled: "<<location<<std::endl;
        return;
    }

    if (input_variable["iThermochimica"].getValue() == 0) return;

    double temperature = history_variable["Temperature"].getFinalValue();
    double pressure = history_variable["System pressure"].getFinalValue();

    CallThermochemistryModule(pressure, temperature, location, sciantix_variable, oxygenfraction);

    return;

}


void Simulation::CallThermochemistryModule(double pressure, double temperature, std::string location, SciantixArray<SciantixVariable> &sciantix_variable, double oxygenfraction)
{
    const std::vector<ThermochemistryManifestEntry> manifest =
        loadThermochemistryManifest(TestPath + "input_thermochemistry.txt");
    const ThermochemistrySettings settings =
        loadThermochemistrySettings(TestPath + "input_thermochemistry_settings.txt");
    const std::string category = (location == "matrix") ? "matrix" : "fission_products";
    const std::set<std::string> manifest_elements = getThermochemistryElements(manifest, category, location);
    const ThermochemistryPhaseSettings& location_settings =
        (location == "matrix") ? settings.matrix : settings.fission_products;
    const std::string module = location_settings.module;
    
    if (module == "OPENCALPHAD")
    {
        std::string directoryPath = "./../../../opencalphad/";
        std::string inputPath = "inputs/thermoin.OCM";
        std::string outputPath = "outputs/thermoout";
        std::string parserPath = "parse_oc_output_to_json.py";
        std::string dataPath = "./../data/" + location_settings.database;
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
                inputFile << "n=1 x(o)=" << oxygenfraction << " ";
                // sciantix_variable["Uranium content"].getFinalValue() + sciantix_variable["Oxygen content"].getFinalValue(); 
                // For matrix, we consider Uranium and Oxygen content, these are MOLES/grain not atoms
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

        // 3. Call Python script to preprocess the output file
        std::vector<std::string> elementNames(manifest_elements.begin(), manifest_elements.end());

        std::string elements_str = "[";
        for (size_t i = 0; i < elementNames.size(); ++i) {
            elements_str += "\"" + elementNames[i] + "\"";
            if (i != elementNames.size() - 1)
                elements_str += ",";
        }
        elements_str += "]";
        std::cout << "Elements: " << elements_str << std::endl;

        std::string pythonCommand = "python3 "+ directoryPath + parserPath + " " + directoryPath + outputPath +".DAT " + directoryPath + outputPath + ".json "+ elements_str;
        int pyStatus = std::system(pythonCommand.c_str());
        if (pyStatus != 0) 
        {
            std::cerr << "Error: Execution of Python preprocessing script failed!\n";
            return;
        }

        // 3. Read from DAT file output    
        std::ifstream jsonFile(directoryPath + outputPath + ".json");
        if (!jsonFile) {
            std::cerr << "Error: Cannot open thermochemistry output file: " << outputPath << std::endl;
            return;
        }

        Json::Value outputfile;
        jsonFile >> outputfile;

        const Json::Value& SolutionPhases = outputfile["1"]["solution phases"];
        for (auto& phase : SolutionPhases.getMemberNames())
        {
            if ((SolutionPhases[phase].isMember("species")) && (SolutionPhases[phase]["species"].isObject()) && (!SolutionPhases[phase]["species"].empty()))
            {
                for (auto& compound : SolutionPhases[phase]["species"].getMemberNames())
                {
                    double moles = SolutionPhases[phase]["species"][compound]["moles"].asDouble();
                    thermochemistry_variable[compound + " (" + phase + ", " + location + ")"].setFinalValue(moles);
                }
            }
            else
            {
                for (auto& compound : SolutionPhases[phase]["elements"].getMemberNames())
                {
                    double moles = SolutionPhases[phase]["elements"][compound]["moles of element in phase"].asDouble();
                    thermochemistry_variable[compound + " (" + phase + ", " + location + ")"].setFinalValue(moles);
                }
            }
        }

        if (location == "matrix")
        {
            Matrix fuel(matrices[0]);
            if ((SolutionPhases.isMember("gas")) && (SolutionPhases["gas"].isObject()) && (!SolutionPhases["gas"].empty()))
            {
                double total_moles = sciantix_variable["Uranium content"].getFinalValue() + sciantix_variable["Oxygen content"].getFinalValue();
                double mol_u = SolutionPhases["gas"]["elements"]["U"]["moles of element in phase"].asDouble()*total_moles;
                double mol_o = SolutionPhases["gas"]["elements"]["O"]["moles of element in phase"].asDouble()*total_moles;
                
                sciantix_variable["Grain radius"].setFinalValue(sciantix_variable["Grain radius"].getFinalValue() * pow((total_moles - mol_u - mol_o)/total_moles, 1.0/3.0));
            }
            return;
        }

        // elements: update gas atoms and grain boundary variables
        for (const auto& element : manifest_elements) {
            double moles(0);
            if ((SolutionPhases.isMember("gas")) && (SolutionPhases["gas"].isObject()) && (!SolutionPhases["gas"].empty()))
                moles = SolutionPhases["gas"]["elements"][element]["moles of element in phase"].asDouble();
        
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
