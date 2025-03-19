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

    double pressure = 0.0;
    if (input_variable["iThermochimica"].getValue() == 1)
    {
        if(sciantix_variable["Intergranular vacancies per bubble"].getInitialValue())
            pressure = ( boltzmann_constant *  Temperature * sciantix_variable["Intergranular atoms per bubble"].getInitialValue()/(sciantix_variable["Intergranular vacancies per bubble"].getInitialValue() * matrices["UO2"].getSchottkyVolume()));
        else
            pressure = 1.0;
    }
    else if (input_variable["iThermochimica"].getValue() > 1)
    {
        pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue();
    }


    // 1. Create the input file
    std::ofstream inputFile("./../../thermochimica-master/inputs/input_GB.ti");
    if (!inputFile) {
        std::cerr << "Error: Cannot create input file!" << std::endl;
        return;
    }
    inputFile << "! Initialize variables:\n";
    inputFile << "pressure          = " << pressure << "\n";
    inputFile << "temperature       = " << history_variable["Temperature"].getFinalValue() << "\n";

    bool nogas = true;

    for (auto &system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0 && system.getGas().getChemicallyActive() == 1)
        {
            double gasatomsavailable = ((sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() + sciantix_variable[system.getGasName() + " reacted - GB"].getFinalValue())  /
                                        (sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));

            if (gasatomsavailable > 0.0){
                nogas = false;
            }

            inputFile << "mass(" << system.getGas().getAtomicNumber() << ")           = " << gasatomsavailable << "\n";
        }
    }

    if (nogas) return;

    inputFile <<  "temperature unit  = K \n";
    inputFile <<  "pressure unit     = Pa\n";
    inputFile <<  "mass unit         = atoms\n";
     
    inputFile << "data file         = ../../thermochimica-master/data/CsI-Pham.dat\n";
    inputFile << "\n! Specify output and debug modes:\nprint mode        = 0\ndebug mode        = .FALSE.\nreinit            = .FALSE.\nwrite json        = .TRUE.";

    inputFile.close();

    //std::cout << "Input file written successfully.\n";

    // 2. Execute the external script
    int status = std::system("./../../thermochimica-master/bin/InputScriptMode ../../thermochimica-master/inputs/input_GB.ti");
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
    
    //std::cout << "INTERGRANULAR BEHAVIOUR" << std::endl;
    //std::cout << "------gas ideal----" << std::endl; 
    Json::Value &solution = root["1"]["solution phases"]["gas_ideal"];

    const Json::Value &species = solution["species"];
    for (const auto &specie : species.getMemberNames()) {
        double moles = species[specie]["moles"].asDouble();
        if (moles == 0.0) continue;
        //std::cout << specie << ": " << moles << " moles\n";
        sciantix_variable[specie + " - gas_ideal - GB"].setFinalValue(moles);
    }
    //std::cout << "*******************" << std::endl; 
    const Json::Value &elements = solution["elements"];
    for (const auto &element : elements.getMemberNames()) {
        double moles = elements[element]["moles of element in phase"].asDouble();
        //std::cout << element << ": " << moles << " moles\n";
        double gasatomsavailable = ((sciantix_variable[element + " at grain boundary"].getFinalValue() + sciantix_variable[element + " reacted - GB"].getFinalValue())  /
                                    (sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));
        double gasatomsupdate = moles*avogadro_number; 
        sciantix_variable[ element + " reacted - GB"].setFinalValue((gasatomsavailable - gasatomsupdate)*(sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));
        sciantix_variable[ element + " at grain boundary"].setFinalValue(gasatomsupdate*(sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));
    }
    //std::cout << "*******************" << std::endl; 

    //std::cout << "------liquid----" << std::endl; 
    Json::Value &liquidsolution = root["1"]["solution phases"]["LIQUID"];

    if (liquidsolution["moles"].asDouble() != 0.0) {

        const Json::Value &species = liquidsolution["species"];
        for (const auto &specie : species.getMemberNames()) {
            double moles = species[specie]["moles"].asDouble();
            if (moles == 0.0) continue;
            //std::cout << specie << ": " << moles << " moles\n";
            sciantix_variable[specie + " - LIQUID - GB"].setFinalValue(moles);
        }
        //std::cout << "*******************" << std::endl; 
        const Json::Value &elements = liquidsolution["elements"];
        for (const auto &element : elements.getMemberNames()) {
            double moles = elements[element]["moles of element in phase"].asDouble();
            if (moles == 0.0) continue;
            //std::cout << element << ": " << moles << " moles\n";
        }
        //std::cout << "*******************" << std::endl; 

    }
        
    //std::cout << "------PURE CONDENSED PHASES----" << std::endl; 
    
    Json::Value &condensed = root["1"]["pure condensed phases"];
    for (const auto &phase : condensed.getMemberNames()) {
        
        if (condensed[phase]["moles"].asDouble() == 0.0) continue;
        
        //std::cout << phase << ": " << condensed[phase]["moles"].asDouble() << " moles\n";
        //std::cout << "*******************" << std::endl; 
        sciantix_variable[phase + " - pure condensed phases - GB"].setFinalValue(condensed[phase]["moles"].asDouble());
    
        // Access the "elements" for this phase
        const Json::Value &elements = condensed[phase]["elements"];
        for (const auto &element : elements.getMemberNames()) {
            double moles = elements[element]["moles of element in phase"].asDouble();
            if (moles == 0.0) continue;
            //std::cout << element << ": " << moles << " moles\n";
        }
        //std::cout << "*******************" << std::endl; 
    }
    //std::cout << "-----------------" << std::endl;

    const char* oldFileName = "../../thermochimica-master/outputs/thermoout.json";
    const char* newFileName = "../../thermochimica-master/outputs/thermoout_GB.json";

    // Rename the file
    if (std::rename(oldFileName, newFileName) != 0) {
        std::cerr << "Error: Renaming file failed!\n";
        return;
    }

    return;
}
