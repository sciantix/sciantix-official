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
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <map>     
#include <cmath>
#include <limits>
#include <json/json.h>
#include <cctype>

/**
 * @brief Main entry point to set the thermodynamic phase diagram for Mo-Pd-Rh-Ru-Tc.
 * @param location The physical domain: "in grain", "at grain boundary", or "matrix".
 */

void Simulation::SetPhaseDiagram5metals(std::string location)
{
    // Skip if thermochemistry coupling is not enabled (iThermochimica < 2)
    if (input_variable["iThermochimica"].getValue() < 2) return;

    // Local variables for boundary conditions
    double temperature = history_variable["Temperature"].getFinalValue();
    double pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue() * scaling_factors["Dummy"].getValue();
    
    // Domain-specific checks
    if (location == "in grain" || location == "at grain boundary")
    {
        if (sciantix_variable["Grain radius"].getFinalValue() <= 0.0) return;
    }
    else if (location != "matrix")
    {
        std::cout << "Location not yet modelled: " << location << std::endl;
        return;
    }

    CallNobleMetalsModule(pressure, temperature, location, sciantix_variable);
}

/**
 * @brief Interfaces with OpenCalphad to calculate the chemical equilibrium of noble metals.
 */

void Simulation::CallNobleMetalsModule(double pressure, double temperature, std::string location, SciantixArray<SciantixVariable> &sciantix_variable)
{
    // 1. Load thermochemistry settings from JSON
    std::string thermochemistryJsonPath = "./input_thermochemistry.json";
    std::ifstream jsonFile(thermochemistryJsonPath);
    if (!jsonFile.is_open()) return;

    Json::Value root;
    jsonFile >> root;

    // Determine which settings block to use based on location
    std::string settingsBlock = (location == "matrix") ? "matrix" : "noble_metals";
    std::string module = root["Settings"][settingsBlock]["module"].asString();

    if (module == "OPENCALPHAD")
    {
        // Define paths for OpenCalphad interface
        std::string directoryPath = "./../../../opencalphad/";
        std::string ocmInputPath = directoryPath + "inputs/noble_metals.OCM";
        std::string datOutputPath = directoryPath + "outputs/noble_metals_out.DAT";
        std::string jsonOutputPath = directoryPath + "outputs/noble_metals_out.json";
        std::string parserScript = directoryPath + "parse_oc_output_to_json.py";
        std::string databasePath = "./../data/" + root["Settings"][settingsBlock]["database"].asString();
        
        // Ensure database extension is correct
        if (location != "matrix") databasePath += ".TDB";
        
        std::string ocExecutable = directoryPath + "oc6P " + ocmInputPath;
        
        // Retrieve element list from JSON settings
        Json::Value& elements = root["Settings"][settingsBlock]["elements"];
        std::vector<std::string> elementNames = elements.getMemberNames();

        // 2. Check if there is enough material to justify a thermodynamic call
        double total_moles = 0.0;
        for (auto& elementName : elementNames)
        {
            std::string sciantix_elem = elementName;
            if (sciantix_elem.length() > 1) sciantix_elem[1] = std::tolower(sciantix_elem[1]);

            double atomsAvailable = sciantix_variable[sciantix_elem + " produced"].getFinalValue();
            total_moles += atomsAvailable / avogadro_number;
        }

        // If total moles are negligible skip OpenCalphad
        if (total_moles < 1e-10) return;

        // 3. Generate OpenCalphad Command Macro (.OCM)
        std::ofstream inputFile(ocmInputPath);
        if (!inputFile) return;

        inputFile << "@$ Initialize variables:\n";
        inputFile << "r t " << databasePath << "\n\n\n\n"; // Skip database warnings
        inputFile << "set c t=" << temperature << "\n";
        inputFile << "set c p=" << pressure << "\n";

        for (auto& elementName : elementNames)
        {
            std::string sciantix_elem = elementName;
            if (sciantix_elem.length() > 1) sciantix_elem[1] = std::tolower(sciantix_elem[1]);

            double moles = sciantix_variable[sciantix_elem + " produced"].getFinalValue() / avogadro_number;
            
            // Numerical safety: avoid absolute zero to prevent logarithm errors in OC
            if (moles < 1e-12) moles = 1e-12; 
            inputFile << "set c n(" << elementName << ")=" << moles << "\n"; 
        }

        inputFile << "c e\n"; // Calculate Equilibrium
        inputFile << "l /out=./../outputs/noble_metals_out.DAT r 1\n"; // Save output
        inputFile << "fin\n"; 
        inputFile.close();

        // 4. Run OpenCalphad
        if (std::system(ocExecutable.c_str()) != 0) 
        {
            std::cerr << "Error: OpenCalphad execution failed.\n";
            return;
        }

        // 5. Convert OC output to JSON via Python parser
        std::string elements_str = "[";
        for (size_t i = 0; i < elementNames.size(); ++i) {
            elements_str += "\"" + elementNames[i] + "\"";
            if (i != elementNames.size() - 1) elements_str += ",";
        }
        elements_str += "]";

        std::string pythonCmd = "python3 " + parserScript + " " + datOutputPath + " " + jsonOutputPath + " " + elements_str;
        if (std::system(pythonCmd.c_str()) != 0) 
        {
            std::cerr << "Error: Python parser execution failed.\n";
            return;
        }

        // 6. Read JSON results back into Sciantix variables
        std::ifstream resultFile(jsonOutputPath);
        Json::Value outputData;
        resultFile >> outputData;

        const Json::Value& solutionPhases = outputData["1"]["solution phases"];
        for (auto& phase : solutionPhases.getMemberNames())
        {   
            // Filter out summary or non-crystalline phases
            if (phase == "condensed" || phase == "gas") continue;
            
            for (auto& element : solutionPhases[phase]["elements"].getMemberNames())
            {
                double molesInPhase = solutionPhases[phase]["elements"][element]["moles of element in phase"].asDouble();
                thermochemistry_variable[element + " (" + phase + ", " + location + ")"].setFinalValue(molesInPhase);
            }
        }

        if (location == "matrix") return;

        // 7. Transition from total produced to reacted/unreacted atoms
        for (auto& element : elementNames)
        {
            std::string sciantix_elem = element;
            if (sciantix_elem.length() > 1) sciantix_elem[1] = std::tolower(sciantix_elem[1]);

            // Sum element moles across all stable solution phases (BCC, HCP, SIGMA, etc.)
            double equilibrium_moles = 0.0;
            for (auto& phase : solutionPhases.getMemberNames())
            {
                if (solutionPhases[phase]["elements"].isMember(element))
                {
                    equilibrium_moles += solutionPhases[phase]["elements"][element]["moles of element in phase"].asDouble();
                }
            }

            double availableAtoms = sciantix_variable[sciantix_elem + " produced"].getFinalValue();
            double precipitatedAtoms = std::min(availableAtoms, equilibrium_moles * avogadro_number);

            if (location == "in grain") 
            {
                sciantix_variable[sciantix_elem + " reacted - IG"].setFinalValue(precipitatedAtoms);
                sciantix_variable[sciantix_elem + " in grain"].setFinalValue(std::max(0.0, availableAtoms - precipitatedAtoms));
            }            
            else if (location == "at grain boundary")
            {
                sciantix_variable[sciantix_elem + " reacted - GB"].setFinalValue(precipitatedAtoms);   
                sciantix_variable[sciantix_elem + " at grain boundary"].setFinalValue(std::max(0.0, availableAtoms - precipitatedAtoms));
            }
        }
    }
}