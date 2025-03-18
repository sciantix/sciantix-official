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
#include <string>
#include <json/json.h>

void Simulation::SetPhaseDiagram1()
{
    if (input_variable["iThermochimica"].getValue() < 3) return;

    double Temperature = history_variable["Temperature"].getFinalValue();
    double bubble_pressure = scaling_factors["Dummy"].getValue();

    // 1. Create the input file
    std::ofstream inputFile("./../../thermochimica-master/inputs/input.ti");
    if (!inputFile) {
        std::cerr << "Error: Cannot create input file!" << std::endl;
        return;
    }

    inputFile << "! Initialize variables:\n";
    inputFile << "pressure          = " << bubble_pressure << "\n";
    inputFile << "temperature       = " << history_variable["Temperature"].getFinalValue() << "\n";

    for (auto &system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0 && system.getGas().getChemicallyActive() == 1)
        {
            double equilibrium_fraction(1.0);
            if ((system.getResolutionRate() + system.getTrappingRate()) > 0.0)
                equilibrium_fraction = system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate());
            
            double gasatomsavailable = (sciantix_variable[system.getGasName() + " produced"].getFinalValue() -
                    sciantix_variable[system.getGasName() + " decayed"].getFinalValue() -
                    sciantix_variable[system.getGasName() + " reacted - GB"].getFinalValue() - 
                    sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() -
                    sciantix_variable[system.getGasName() + " released"].getInitialValue()) * 
                    (4 * M_PI * pow( sciantix_variable["Grain radius"].getFinalValue(),3) / 3.0);

            gasatomsavailable *= (1.0 - equilibrium_fraction);

            inputFile << "mass(" << system.getGas().getAtomicNumber() << ")           = " << gasatomsavailable << "\n";
        }
    }

    inputFile <<  "temperature unit  = K \n";
    inputFile <<  "pressure unit     = Pa\n";
    inputFile <<  "mass unit         = atoms\n";
     
    inputFile << "data file         = ../../thermochimica-master/data/CsI-Pham.dat\n";
    inputFile << "\n! Specify output and debug modes:\nprint mode        = 0\ndebug mode        = .FALSE.\nreinit            = .TRUE.\nwrite json        = .TRUE.";

    inputFile.close();

    std::cout << "Input file written successfully.\n";

    // 2. Execute the external script
    int status = std::system("./../../thermochimica-master/bin/InputScriptMode ../../thermochimica-master/inputs/input.ti");
    if (status != 0) {
        std::cerr << "Error: Execution of ./InputScriptMode failed!\n";
        return;
    }

    // 3a. Read from JSON file 
    std::ifstream jsonFile("../../thermochimica-master/outputs/thermoout.json");
    if (!jsonFile) {
        std::cerr << "Error: Cannot open json!" << std::endl;
        return;
    }

    Json::Value root;
    jsonFile >> root;
    
    std::cout << "INTRAGRANULAR BEHAVIOUR" << std::endl;
    std::cout << "------gas ideal----" << std::endl; 
    Json::Value &solution = root["1"]["solution phases"]["gas_ideal"];

    if (solution["moles"].asDouble() != 0.0) {

        const Json::Value &species = solution["species"];
        for (const auto &specie : species.getMemberNames()) {
            double moles = species[specie]["moles"].asDouble();
            if (moles == 0.0) continue;
            std::cout << specie << ": " << moles << " moles\n";
        }
        std::cout << "*******************" << std::endl; 
        const Json::Value &elements = solution["elements"];
        for (const auto &element : elements.getMemberNames()) {
            double moles = elements[element]["moles of element in phase"].asDouble();
            if (moles == 0.0) continue;
            std::cout << element << ": " << moles << " moles\n";

            double equilibrium_fraction(1.0);

            for (auto &system : sciantix_system)
            {
                if (system.getGasName() == element)
                {
                    if ((system.getResolutionRate() + system.getTrappingRate()) > 0.0)
                        equilibrium_fraction = system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate());
                }
            }

            double gasatomsavailable = (sciantix_variable[element + " produced"].getFinalValue() -
                    sciantix_variable[element + " decayed"].getFinalValue() -
                    sciantix_variable[element + " reacted - GB"].getFinalValue() - 
                    sciantix_variable[element + " at grain boundary"].getFinalValue() -
                    sciantix_variable[element + " released"].getInitialValue()) * 
                    (4 * M_PI * pow( sciantix_variable["Grain radius"].getFinalValue(),3) / 3.0);

            gasatomsavailable *= (1.0 - equilibrium_fraction);
            double gasatomsupdate = moles*avogadro_number; 
            sciantix_variable["x reacted - IG"].setFinalValue((gasatomsavailable - gasatomsupdate)/ gasatomsavailable);
            sciantix_variable[ element + " reacted - IG"].setFinalValue((gasatomsavailable - gasatomsupdate)/(4 * M_PI * pow( sciantix_variable["Grain radius"].getFinalValue(),3) / 3.0));
            sciantix_variable[ element + " in grain"].setFinalValue((gasatomsupdate)/(4 * M_PI * pow( sciantix_variable["Grain radius"].getFinalValue(),3) / 3.0));
        }
        std::cout << "*******************" << std::endl; 
    }

    std::cout << "------liquid----" << std::endl; 
    Json::Value &liquidsolution = root["1"]["solution phases"]["LIQUID"];

    if (liquidsolution["moles"].asDouble() != 0.0) {

        const Json::Value &species = liquidsolution["species"];
        for (const auto &specie : species.getMemberNames()) {
            double moles = species[specie]["moles"].asDouble();
            if (moles == 0.0) continue;
            std::cout << specie << ": " << moles << " moles\n";
            sciantix_variable[specie + " - LIQUID - IG"].setFinalValue(moles);
        }
        std::cout << "*******************" << std::endl; 
        const Json::Value &elements = liquidsolution["elements"];
        for (const auto &element : elements.getMemberNames()) {
            double moles = elements[element]["moles of element in phase"].asDouble();
            if (moles == 0.0) continue;
            std::cout << element << ": " << moles << " moles\n";
        }
        std::cout << "*******************" << std::endl; 

    }
        
    std::cout << "------PURE CONDENSED PHASES----" << std::endl; 
    
    Json::Value &condensed = root["1"]["pure condensed phases"];
    for (const auto &phase : condensed.getMemberNames()) {
        
        if (condensed[phase]["moles"].asDouble() == 0.0) continue;
        
        std::cout << phase << ": " << condensed[phase]["moles"].asDouble() << " moles\n";
        std::cout << "*******************" << std::endl; 
        sciantix_variable[phase + " - pure condensed phases - IG"].setFinalValue(condensed[phase]["moles"].asDouble());
    
        // Access the "elements" for this phase
        const Json::Value &elements = condensed[phase]["elements"];
        for (const auto &element : elements.getMemberNames()) {
            double moles = elements[element]["moles of element in phase"].asDouble();
            if (moles == 0.0) continue;
            std::cout << element << ": " << moles << " moles\n";
        }
        std::cout << "*******************" << std::endl; 
    }
    std::cout << "-----------------" << std::endl;
    return;
}
