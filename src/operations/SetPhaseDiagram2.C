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

void Simulation::SetPhaseDiagram2()
{
    if (input_variable["iThermochimica"].getValue() == 0) 
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

    Matrix fuel(matrices[0]);
    std::string location = "at grain boundary";

    // Set state variables: temperature, pressure
    double temperature = history_variable["Temperature"].getFinalValue();

    double pressure = 0.0;
    if (input_variable["iThermochimica"].getValue() == 1)
    {
        if (sciantix_variable["Intergranular vacancies per bubble"].getInitialValue())
            pressure = ( boltzmann_constant *  temperature * sciantix_variable["Intergranular atoms per bubble"].getInitialValue()/(sciantix_variable["Intergranular vacancies per bubble"].getInitialValue() * matrices["UO2"].getSchottkyVolume()));
        else
            pressure = 1e5;
    }
    else
    {
        pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue() * scaling_factors["Dummy"].getValue();
    }

    // 1. Write input file
    const std::string inputPath = "./../../thermochimica-master/inputs/input_GB.ti";
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
            std::string gasName = system.getGasName();
            double gasatomsavailable = (
                sciantix_variable[gasName + " produced"].getFinalValue() -
                sciantix_variable[gasName + " decayed"].getFinalValue() -
                sciantix_variable[gasName + " reacted - IG"].getFinalValue() -
                sciantix_variable[gasName + " in grain"].getFinalValue() -
                sciantix_variable[gasName + " released"].getInitialValue()
            );
            
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
    int status = std::system("./../../thermochimica-master/bin/InputScriptMode ../../thermochimica-master/inputs/input_GB.ti");
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
        double available = (sciantix_variable[element + " produced"].getFinalValue()
                            - sciantix_variable[element + " decayed"].getFinalValue()
                            - sciantix_variable[element + " reacted - IG"].getFinalValue()
                            - sciantix_variable[element + " in grain"].getFinalValue()
                            - sciantix_variable[element + " released"].getInitialValue());
        double updateAtoms = std::min(available, moles * avogadro_number);

        bool kinetics = false;
        if (kinetics)
        {
            // Trapping kinetics 
            double D_gb = 0.0;
            double MM = 0.0;
            for (auto& system : sciantix_system) {
                if (system.getGasName() == element) {
                    D_gb = system.getFissionGasDiffusivity();
                    MM = system.getGas().getMassNumber() * 1e-3;
                    break;
                }
            }

            // double conc = sciantix_variable["Intergranular bubble concentration"].getFinalValue();
            // double radius = sciantix_variable["Intergranular bubble radius"].getFinalValue();
            // double trapRate = 2.0 * M_PI * D_gb * conc/ log(1.0 / (radius * sqrt(M_PI * conc)));
                    
            // Evaporation kinetics 
            double relativeVolume = (available-updateAtoms)*boltzmann_constant*temperature/pressure;
            double Langmuir(0);
            if (relativeVolume)
                Langmuir = pow(gas_constant*temperature/(2*M_PI*MM), 0.5)*4*M_PI*pow(sciantix_variable["Grain radius"].getFinalValue(),2)/relativeVolume;

            double excesssol = (sciantix_variable[element + " reacted - GB"].getFinalValue() - (available - updateAtoms));
            double excessgb = (sciantix_variable[element + " at grain boundary"].getFinalValue() - (updateAtoms));
            
            double Rate = Langmuir;
            if (Rate*physics_variable["Time step"].getFinalValue()>1) 
            {
                std::cout<<"WARNING: Langmuir rate cut"<<std::endl;
                if (physics_variable["Time step"].getFinalValue())
                    Rate = 1/physics_variable["Time step"].getFinalValue();
                else
                    Rate = 0;
            }

            double newReacted = 0.0;
            if (excesssol > 0)
            {
                newReacted = solver.Decay(
                    sciantix_variable[element + " reacted - GB"].getFinalValue(),
                    Rate,
                    Rate * (available - updateAtoms),
                    physics_variable["Time step"].getFinalValue()
                );
            }
            else if (excessgb > 0)
            {
                newReacted = solver.Decay(
                    sciantix_variable[element + " reacted - GB"].getFinalValue(),
                    0.0,
                    Rate * (excessgb),
                    physics_variable["Time step"].getFinalValue()
                );
            }

            sciantix_variable[element + " reacted - GB"].setFinalValue(newReacted);
            sciantix_variable[element + " at grain boundary"].setFinalValue(available - newReacted);
        }
        else
        {
            // No kinetics, just update the variable
            sciantix_variable[element + " at grain boundary"].setFinalValue(updateAtoms);
            sciantix_variable[element + " reacted - GB"].setFinalValue(available - updateAtoms);
        }
    }

    // Rename output file for grain boundary
    std::rename(
        "../../thermochimica-master/outputs/thermoout.json",
        "../../thermochimica-master/outputs/thermoout_GB.json"
    );

    return;
}
