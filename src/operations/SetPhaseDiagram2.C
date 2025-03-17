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

void Simulation::SetPhaseDiagram2()
{
    if (input_variable["iThermochimica"].getValue() == 0) return;

    double Temperature = history_variable["Temperature"].getFinalValue();
    double bubble_pressure;
    if(sciantix_variable["Intergranular vacancies per bubble"].getInitialValue())
        bubble_pressure = ( boltzmann_constant *  Temperature * sciantix_variable["Intergranular atoms per bubble"].getInitialValue()/(sciantix_variable["Intergranular vacancies per bubble"].getInitialValue() * matrices["UO2"].getSchottkyVolume()));
    else
        bubble_pressure = 1.0;


    // 1. Create the input file
    std::ofstream inputFile("./../../thermochimica-master/inputs/input.ti");
    if (!inputFile) {
        std::cerr << "Error: Cannot create input file!" << std::endl;
        return;
    }
    inputFile << "! Initialize variables:\n";

    if (input_variable["iThermochimica"].getValue() == 1)
    {
        inputFile << "pressure          = " << bubble_pressure << "\n";
    }
    else if (input_variable["iThermochimica"].getValue() == 2)
    {
        inputFile << "pressure          = " << scaling_factors["Dummy"].getValue() << "\n";
    }
    
    inputFile << "temperature       = " << history_variable["Temperature"].getFinalValue() << "\n";

    for (auto &system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0 && system.getGas().getChemicallyActive() == 1)
        {
            double gasatomsavailable = ((sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() + sciantix_variable[system.getGasName() + " reacted"].getFinalValue())  /
                                        (sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));
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
    
    std::cout << "------gas ideal----" << std::endl; 
    Json::Value &solution = root["1"]["solution phases"]["gas_ideal"];

    if (solution["moles"].asDouble() != 0.0) {

        const Json::Value &species = solution["species"];
        for (const auto &specie : species.getMemberNames()) {
            double moles = species[specie]["moles"].asDouble();
            if (moles == 0.0) continue;
            std::cout << specie << ": " << moles << " moles\n";
            sciantix_variable[specie + " - gas_ideal"].setFinalValue(moles);
        }
        std::cout << "*******************" << std::endl; 
        const Json::Value &elements = solution["elements"];
        for (const auto &element : elements.getMemberNames()) {
            double moles = elements[element]["moles of element in phase"].asDouble();
            if (moles == 0.0) continue;
            std::cout << element << ": " << moles << " moles\n";
            double gasatomsavailable = ((sciantix_variable[element + " at grain boundary"].getFinalValue() + sciantix_variable[element + " reacted"].getFinalValue())  /
                                        (sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));
            double gasatomsupdate = moles*avogadro_number; 
            sciantix_variable[ element + " reacted"].setFinalValue((gasatomsavailable - gasatomsupdate)*(sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));
            sciantix_variable[ element + " at grain boundary"].setFinalValue(gasatomsupdate*(sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));
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
            sciantix_variable[specie + " - LIQUID"].setFinalValue(moles);
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
        sciantix_variable[phase + " - pure condensed phases"].setFinalValue(condensed[phase]["moles"].asDouble());
    
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
