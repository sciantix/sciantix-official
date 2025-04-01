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

    double pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue();
    pressure *= scaling_factors["Dummy"].getValue();

    // 1. Create the input file
    std::ofstream inputFile("./../../thermochimica-master/inputs/input_IG.ti");
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
            // double equilibrium_fraction(1.0);
            // if ((system.getResolutionRate() + system.getTrappingRate()) > 0.0)
            //     equilibrium_fraction = system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate());
            
            double gasatomsavailable = (sciantix_variable[system.getGasName() + " produced"].getFinalValue() -
                    sciantix_variable[system.getGasName() + " decayed"].getFinalValue() -
                    sciantix_variable[system.getGasName() + " reacted - GB"].getInitialValue() - 
                    sciantix_variable[system.getGasName() + " at grain boundary"].getInitialValue() -
                    sciantix_variable[system.getGasName() + " released"].getInitialValue()) * 
                    (4 * M_PI * pow( sciantix_variable["Grain radius"].getFinalValue(),3) / 3.0);

            // gasatomsavailable *= (1.0 - equilibrium_fraction);

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
    int status = std::system("./../../thermochimica-master/bin/InputScriptMode ../../thermochimica-master/inputs/input_IG.ti");
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
    
    Json::Value &gsolution = root["1"]["solution phases"]["GAS_IDEAL"];

    const Json::Value &species = gsolution["species"];
    for (const auto &specie : species.getMemberNames()) {
        sciantix_variable[specie + " - GAS_IDEAL - IG"].setFinalValue(species[specie]["moles"].asDouble());
    }
    
    const Json::Value &elements = gsolution["elements"];
    for (const auto &element : elements.getMemberNames()) {
        double moles = elements[element]["moles of element in phase"].asDouble();
        //std::cout << element << ": " << moles << " moles\n";

        // double equilibrium_fraction(1.0);

        // for (auto &system : sciantix_system)
        // {
        //     if (system.getGasName() == element)
        //     {
        //         if ((system.getResolutionRate() + system.getTrappingRate()) > 0.0)
        //             equilibrium_fraction = system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate());
        //     }
        // }
        double gasatomsavailable = (sciantix_variable[element + " produced"].getFinalValue() -
                sciantix_variable[element + " decayed"].getFinalValue() -
                sciantix_variable[element + " reacted - GB"].getInitialValue() - 
                sciantix_variable[element + " at grain boundary"].getInitialValue() -
                sciantix_variable[element + " released"].getInitialValue()) * 
                (4 * M_PI * pow( sciantix_variable["Grain radius"].getFinalValue(),3) / 3.0);

        // gasatomsavailable *= (1.0 - equilibrium_fraction);
        double gasatomsupdate = moles*avogadro_number; 
        double x = 1 - gasatomsupdate / gasatomsavailable; // reacted
        if (x<0) x = 0;
        sciantix_variable[ element + " reacted fraction - IG"].setFinalValue(x);
    }
    
    Json::Value &lsolution = root["1"]["solution phases"]["LIQUID"];

    if (lsolution["moles"].asDouble() != 0.0) {
        const Json::Value &species = lsolution["species"];
        for (const auto &specie : species.getMemberNames()) {
            sciantix_variable[specie + " - LIQUID - IG"].setFinalValue(species[specie]["moles"].asDouble());
        }
    }

    Json::Value &lisolution = root["1"]["solution phases"]["LIQUID_IONIC"];

    if (lisolution["moles"].asDouble() != 0.0) {
        const Json::Value &species = lisolution["species"];
        for (const auto &specie : species.getMemberNames()) {
            sciantix_variable[specie + " - LIQUID_IONIC - IG"].setFinalValue(species[specie]["moles"].asDouble());
        }
    }

    Json::Value &fccsolution = root["1"]["solution phases"]["FCC_A1"];

    if (fccsolution["moles"].asDouble() != 0.0) {
        const Json::Value &species = fccsolution["species"];
        for (const auto &specie : species.getMemberNames()) {
            sciantix_variable[specie + " - FCC_A1 - IG"].setFinalValue(species[specie]["moles"].asDouble());
        }
    }

    Json::Value &condensed = root["1"]["pure condensed phases"];
    for (const auto &phase : condensed.getMemberNames()) {
        sciantix_variable[phase + " - pure condensed phases - IG"].setFinalValue(condensed[phase]["moles"].asDouble());
    }

    const char* oldFileName = "../../thermochimica-master/outputs/thermoout.json";
    const char* newFileName = "../../thermochimica-master/outputs/thermoout_IG.json";

    // Rename the file
    if (std::rename(oldFileName, newFileName) != 0) {
        std::cerr << "Error: Renaming file failed!\n";
        return;
    }

    // Model declaration
    for (auto &system : sciantix_system)
    {
        if (system.getGas().getChemicallyActive() == 1.0 && system.getRestructuredMatrix() == 0)
        {
            double gasatomsavailable = (sciantix_variable[system.getGasName() + " produced"].getFinalValue() -
                sciantix_variable[system.getGasName() + " decayed"].getFinalValue() -
                sciantix_variable[system.getGasName() + " reacted - GB"].getInitialValue() - 
                sciantix_variable[system.getGasName() + " at grain boundary"].getInitialValue() -
                sciantix_variable[system.getGasName() + " released"].getInitialValue());

            double xi = sciantix_variable[ system.getGasName() + " reacted - IG"].getInitialValue()/gasatomsavailable;
            if (std::isnan(xi)) xi = 0.0; 
            double xf = sciantix_variable[ system.getGasName() + " reacted fraction - IG"].getFinalValue();
            if (std::isnan(xf)) xf = 0.0; 
            
            sciantix_variable[system.getGasName() + " reacted - IG"].setFinalValue(
                solver.Decay(
                    sciantix_variable[system.getGasName() + " reacted - IG"].getInitialValue(),
                    0,
                    gasatomsavailable,
                    (xf - xi)
                ));
        }
    }

    return;
}
