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

    std::string location = "in grain";
    
    double temperature = history_variable["Temperature"].getFinalValue();

    double pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue() * scaling_factors["Dummy"].getValue();

    // 1. Write input file
    const std::string inputPath = "./../../thermochimica-master/inputs/input_IG.ti";
    std::ofstream inputFile(inputPath);
    if (!inputFile) 
    {
        std::cerr << "Error: Cannot create input file: " << inputPath << std::endl;
        return;
    }
    inputFile << "! Initialize variables:\n";
    inputFile << "pressure          = " << pressure << "\n";
    inputFile << "temperature       = " << temperature << "\n";

    bool nogas = true;

    for (auto &system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0 && system.getGas().getChemicallyActive() == 1)
        {
            // double equilibrium_fraction(1.0);
            // if ((system.getResolutionRate() + system.getTrappingRate()) > 0.0)
            //     equilibrium_fraction = system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate());
            
            std::string gasName = system.getGasName();
            double gasatomsavailable = (
                sciantix_variable[gasName + " produced"].getInitialValue() -
                sciantix_variable[gasName + " decayed"].getInitialValue() -
                sciantix_variable[gasName + " reacted - GB"].getInitialValue() -
                sciantix_variable[gasName + " at grain boundary"].getInitialValue() -
                sciantix_variable[gasName + " released"].getInitialValue()
            );
            // gasatomsavailable *= (1.0 - equilibrium_fraction);

            if (gasatomsavailable > 0.0)
            {
                nogas = false;
            }

            inputFile << "mass(" << system.getGas().getAtomicNumber() << ")           = " << gasatomsavailable << "\n";
        }
    }

    if (nogas) return;

    inputFile << "temperature unit  = K\n"
              << "pressure unit     = Pa\n"
              << "mass unit         = atoms\n"
              << "data file         = ../../thermochimica-master/data/CsITe_Clean.dat\n"
              << "! Specify output and debug modes:\n"
              << "print mode        = 0\n"
              << "debug mode        = .FALSE.\n"
              << "reinit            = .FALSE.\n"
              << "write json        = .TRUE.\n";
    inputFile.close();

    // 2. Execute the external script: THERMOCHIMICA
    int status = std::system("./../../thermochimica-master/bin/InputScriptMode ../../thermochimica-master/inputs/input_IG.ti");
    if (status != 0) 
    {
        std::cerr << "Error: Execution of ./InputScriptMode failed!\n";
        return;
    }

    // 3. Read from JSON file output
    const std::string jsonPath = "../../thermochimica-master/outputs/thermoout.json";
    std::ifstream jsonFile(jsonPath);
    if (!jsonFile) {
        std::cerr << "Error: Cannot open JSON output: " << jsonPath << std::endl;
        return;
    }

    Json::Value root;
    jsonFile >> root;

    const Json::Value& SolutionPhases = root["1"]["solution phases"];
    for (const auto& phase : SolutionPhases.getMemberNames())
    {
        for (const auto& compound : SolutionPhases[phase]["species"].getMemberNames())
        {
            double moles = SolutionPhases[phase]["species"][compound]["moles"].asDouble();
            thermochemistry_variable[compound + " (" + phase + ", " + location + ")"].setFinalValue(moles);
        }
    }

    const Json::Value& Condensed = root["1"]["pure condensed phases"];
    for (const auto& compound : Condensed.getMemberNames())
    {
        double moles = Condensed[compound]["moles"].asDouble();
        std::string phase = "pure_condensed";
        thermochemistry_variable[compound + " (" + phase + ", " + location + ")"].setFinalValue(moles);

    }

    // elements: update gas atoms and grain boundary variables
    for (const auto& element : SolutionPhases["gas"]["elements"].getMemberNames()) {
        double moles = SolutionPhases["gas"]["elements"][element]["moles of element in phase"].asDouble();
        double available = (sciantix_variable[element + " produced"].getInitialValue()
                            - sciantix_variable[element + " decayed"].getInitialValue()
                            - sciantix_variable[element + " reacted - GB"].getInitialValue()
                            - sciantix_variable[element + " at grain boundary"].getInitialValue()
                            - sciantix_variable[element + " released"].getInitialValue());
        double updateAtoms = std::min(available, moles * avogadro_number);

        sciantix_variable[ element + " reacted - IG"].setFinalValue(available - updateAtoms);
    }

    // Rename output file for grain boundary
    std::rename(
        "../../thermochimica-master/outputs/thermoout.json",
        "../../thermochimica-master/outputs/thermoout_IG.json"
    );

    return;
}
