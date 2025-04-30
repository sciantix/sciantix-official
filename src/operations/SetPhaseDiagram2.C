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
            pressure = 1e5;
    }
    else if (input_variable["iThermochimica"].getValue() > 1)
    {
        pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue();
        pressure *= scaling_factors["Dummy"].getValue();
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
            double gasatomsavailable = (sciantix_variable[system.getGasName() + " produced"].getFinalValue() -
                sciantix_variable[system.getGasName() + " decayed"].getFinalValue() -
                sciantix_variable[system.getGasName() + " reacted - IG"].getFinalValue() -
                sciantix_variable[system.getGasName() + " in grain"].getFinalValue() -
                sciantix_variable[system.getGasName() + " released"].getInitialValue());
            
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
     
    inputFile << "data file         = ../../thermochimica-master/data/CsITe.dat\n";
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
    Json::Value &gsolution = root["1"]["solution phases"]["GAS_IDEAL"];

    const Json::Value &species = gsolution["species"];
    for (const auto &specie : species.getMemberNames()) {
        sciantix_variable[specie + " - GAS_IDEAL - GB"].setFinalValue(species[specie]["moles"].asDouble());
    }
    
    const Json::Value &elements = gsolution["elements"];
    for (const auto &element : elements.getMemberNames()) {
        double moles = elements[element]["moles of element in phase"].asDouble();
        //std::cout << element << ": " << moles << " moles\n";
        double gasatomsavailable = (sciantix_variable[element + " produced"].getFinalValue() -
                    sciantix_variable[element + " decayed"].getFinalValue() -
                    sciantix_variable[element + " reacted - IG"].getFinalValue() -
                    sciantix_variable[element + " in grain"].getFinalValue() -
                    sciantix_variable[element + " released"].getInitialValue());
        double gasatomsupdate = moles*avogadro_number; 

        double alpha(0.00);
        //sciantix_variable[ element + " precipitated"].setFinalValue(alpha*(gasatomsavailable - gasatomsupdate));
        sciantix_variable[ element + " reacted - GB"].setFinalValue((1-alpha)*(gasatomsavailable - gasatomsupdate));
        sciantix_variable[ element + " at grain boundary"].setFinalValue((1-alpha)*gasatomsupdate);
        //sciantix_variable[ element + " released"].addValue((alpha)*gasatomsupdate);
    }
    
    Json::Value &lsolution = root["1"]["solution phases"]["LIQUID"];

    if (lsolution["moles"].asDouble() != 0.0) {
        const Json::Value &species = lsolution["species"];
        for (const auto &specie : species.getMemberNames()) {
            sciantix_variable[specie + " - LIQUID - GB"].setFinalValue(species[specie]["moles"].asDouble());
        }
    }

    Json::Value &lisolution = root["1"]["solution phases"]["LIQUID_IONIC"];

    if (lisolution["moles"].asDouble() != 0.0) {
        const Json::Value &species = lisolution["species"];
        for (const auto &specie : species.getMemberNames()) {
            sciantix_variable[specie + " - LIQUID_IONIC - GB"].setFinalValue(species[specie]["moles"].asDouble());
        }
    }

    Json::Value &fccsolution = root["1"]["solution phases"]["FCC_A1"];

    if (fccsolution["moles"].asDouble() != 0.0) {
        const Json::Value &species = fccsolution["species"];
        for (const auto &specie : species.getMemberNames()) {
            sciantix_variable[specie + " - FCC_A1 - GB"].setFinalValue(species[specie]["moles"].asDouble());
        }
    }

    Json::Value &condensed = root["1"]["pure condensed phases"];
    for (const auto &phase : condensed.getMemberNames()) {
        sciantix_variable[phase + " - pure condensed phases - GB"].setFinalValue(condensed[phase]["moles"].asDouble());
    }

    const char* oldFileName = "../../thermochimica-master/outputs/thermoout.json";
    const char* newFileName = "../../thermochimica-master/outputs/thermoout_GB.json";

    // Rename the file
    if (std::rename(oldFileName, newFileName) != 0) {
        std::cerr << "Error: Renaming file failed!\n";
        return;
    }

    return;
}
