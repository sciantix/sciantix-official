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

void Simulation::SetPhaseDiagram(std::string location)
{
    double temperature(0);
    double pressure(0);
    double oxygenfraction(std::nan("")); // Oxygen fraction defined only for location = matrix
    
    if (location == "in grain")
    {
        if (input_variable["iThermochimica"].getValue() < 3) return;

        temperature = history_variable["Temperature"].getFinalValue();

        pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue() * scaling_factors["Dummy"].getValue();
    }
    else if (location == "at grain boundary")
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

        temperature = history_variable["Temperature"].getFinalValue();
        
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
    }
    else if (location == "matrix")
    {
        if (input_variable["iThermochimica"].getValue() < 1) 
        {
            GrainVaporisation(true); 
            return; 
        }

        temperature = history_variable["Temperature"].getFinalValue();
        pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue() * scaling_factors["Dummy"].getValue();
        double stoicdev = sciantix_variable["Stoichiometry deviation"].getFinalValue();
        oxygenfraction = (stoicdev + 2.0)/(1 + stoicdev + 2.0);
    }
    else
    {
        std::cout<<"Location not yet modelled: "<<location<<std::endl;
        return;
    }

    CallThermochemistryModule(pressure, temperature, location, sciantix_variable, oxygenfraction);

    return;

}


void Simulation::CallThermochemistryModule(double pressure, double temperature, std::string location, SciantixArray<SciantixVariable> &sciantix_variable, double oxygenfraction)
{
    std::string inputPath = "./input_thermochemistry.json";
    
    std::ifstream jsonFile(inputPath);
    if (!jsonFile) {
        std::cerr << "Error: Cannot open thermochemistry input file: " << inputPath << std::endl;
    }

    Json::Value root;
    jsonFile >> root;

    std::string module = root["Settings"]["fission_products"]["module"].asString();
    if (location == "matrix") module = root["Settings"]["matrix"]["module"].asString();

    if (module == "THERMOCHIMICA")
    {
        std::string directoryPath = "./../../thermochimica-master";
        std::string inputPath = "/inputs/thermoin.ti";
        std::string outputPath = "/outputs/thermoout";
        std::string dataPath = "../../thermochimica-master/data/" + root["Settings"]["fission_products"]["database"].asString();
        if (location == "matrix") dataPath = "../../thermochimica-master/data/" + root["Settings"]["matrix"]["database"].asString();
        std::string exePath = directoryPath + "/bin/InputScriptMode " + directoryPath + inputPath;
        
        // 1. Write input file
        std::ofstream inputFile(directoryPath + inputPath);
        if (!inputFile) 
        {
            std::cerr << "Error: Cannot create input file: " << inputPath << std::endl;
            return;
        }
        inputFile << "! Initialize variables:\n";
        inputFile << "pressure          = " << pressure << "\n";
        inputFile << "temperature       = " << temperature << "\n";

        bool noatoms = true;

        Json::Value& elements = root["Settings"]["fission_products"]["elements"];
        if (location == "matrix")
            elements = root["Settings"]["matrix"]["elements"];

        for (auto& elementName : elements.getMemberNames())
        {
            double atomsavailable(0);
            if (location == "in grain") 
                atomsavailable = (
                    sciantix_variable[elementName + " produced"].getInitialValue() -
                    sciantix_variable[elementName + " decayed"].getInitialValue() -
                    sciantix_variable[elementName + " reacted - GB"].getInitialValue() -
                    sciantix_variable[elementName + " at grain boundary"].getInitialValue() -
                    sciantix_variable[elementName + " released"].getInitialValue()
                );
            else if (location =="at grain boundary")
                atomsavailable = (
                    sciantix_variable[elementName + " produced"].getFinalValue() -
                    sciantix_variable[elementName + " decayed"].getFinalValue() -
                    sciantix_variable[elementName + " reacted - IG"].getFinalValue() -
                    sciantix_variable[elementName + " in grain"].getFinalValue() -
                    sciantix_variable[elementName + " released"].getInitialValue()
                );
            else if (location == "matrix")
                atomsavailable = 1; 
                // (sciantix_variable["Uranium content"].getFinalValue() + sciantix_variable["Oxygen content"].getFinalValue()); 
                // For matrix, we consider Uranium and Oxygen content, these are MOLES/grain not atoms
            else
                std::cout<<"Location not yet modelled: "<<location<<std::endl;

            if (atomsavailable > 0.0) noatoms = false;

            if (location == "matrix")
            {
                inputFile << "n="<<atomsavailable<<" x(o)=" << oxygenfraction << " ";
                std::cout<<"WARNING: to be developed"<<std::endl;
                break;
            }
            else
                inputFile << "mass(" << elements[elementName]["atomicnumber"].asInt() << ")           = " << atomsavailable << "\n";
        }

        if (noatoms) return;

        inputFile << "temperature unit  = K\n"
                << "pressure unit     = Pa\n"
                << "mass unit         = atoms\n"
                << "data file         = "<< dataPath <<".dat\n"
                << "! Specify output and debug modes:\n"
                << "print mode        = 0\n"
                << "debug mode        = .FALSE.\n"
                << "reinit            = .FALSE.\n"
                << "write json        = .TRUE.\n";
        inputFile.close();

        // 2. Execute the external script: THERMOCHIMICA
        int status = std::system(exePath.c_str());
        if (status != 0) 
        {
            std::cerr << "Error: Execution of THERMOCHIMICA failed!\n";
            return;
        }

        // 3. Read from JSON file output    
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

        const Json::Value& Condensed = outputfile["1"]["pure condensed phases"];
        for (auto& compound : Condensed.getMemberNames())
        {
            double moles = Condensed[compound]["moles"].asDouble();
            std::string phase = "pure_condensed";
            thermochemistry_variable[compound + " (" + phase + ", " + location + ")"].setFinalValue(moles);

        }
        if (location == "matrix")
        {
            Matrix fuel(matrices[0]);
            if ((SolutionPhases.isMember("gas")) && (SolutionPhases["gas"].isObject()) && (!SolutionPhases["gas"].empty()))
            {
                double total_moles = sciantix_variable["Uranium content"].getFinalValue() + sciantix_variable["Oxygen content"].getFinalValue();
                double mol_u = SolutionPhases["gas"]["elements"]["U"]["moles of element in phase"].asDouble()*total_moles;
                double mol_o = SolutionPhases["gas"]["elements"]["O"]["moles of element in phase"].asDouble()*total_moles;
                sciantix_variable["Uranium content"].addValue(- mol_u);
                sciantix_variable["Oxygen content"].addValue(- mol_o);

                sciantix_variable["Grain radius"].setFinalValue(sciantix_variable["Grain radius"].getFinalValue() * pow((total_moles - mol_u - mol_o)/total_moles, 1.0/3.0));

                GrainVaporisation(false);
            }
            std::cout<<"Vaporized matrix! active vaporisation module"<<std::endl;
            return;
        }
        // elements: update gas atoms and grain boundary variables
        for (auto& element : elements.getMemberNames()) {
            double moles(0);
            if ((SolutionPhases.isMember("gas")) && (SolutionPhases["gas"].isObject()) && (!SolutionPhases["gas"].empty()))
                moles = SolutionPhases["gas"]["elements"][element]["moles of element in phase"].asDouble();

            double available(0);
            if (location == "in grain") 
                available = (sciantix_variable[element + " produced"].getInitialValue()
                    - sciantix_variable[element + " decayed"].getInitialValue()
                    - sciantix_variable[element + " reacted - GB"].getInitialValue()
                    - sciantix_variable[element + " at grain boundary"].getInitialValue()
                    - sciantix_variable[element + " released"].getInitialValue()
                );
            
            else if (location =="at grain boundary")
                available = (sciantix_variable[element + " produced"].getFinalValue()
                    - sciantix_variable[element + " decayed"].getFinalValue()
                    - sciantix_variable[element + " reacted - IG"].getFinalValue()
                    - sciantix_variable[element + " in grain"].getFinalValue()
                    - sciantix_variable[element + " released"].getInitialValue()
                );
            else
                std::cout<<"Location not yet modelled: "<<location<<std::endl;
            
            double updateAtoms = std::min(available, moles * avogadro_number);
            
            if (location == "in grain") 
                sciantix_variable[ element + " reacted - IG"].setFinalValue(available - updateAtoms);            
            else if (location =="at grain boundary")
            {
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
                        Langmuir = pow(gas_constant*temperature*2*M_PI*MM, -0.5)*4*M_PI*pow(sciantix_variable["Grain radius"].getFinalValue(),2)*pressure*avogadro_number/(available-updateAtoms);

                    double excesssol = (sciantix_variable[element + " reacted - GB"].getFinalValue() - (available - updateAtoms));
                    double excessgb = (sciantix_variable[element + " at grain boundary"].getFinalValue() - (updateAtoms));
                    
                    double Rate = Langmuir;
                    if (Rate*physics_variable["Time step"].getFinalValue()>1) 
                    {
                        std::cout<<"WARNING: Langmuir rate cut at "<<temperature<<std::endl;
                        if (physics_variable["Time step"].getFinalValue())
                            Rate = 1/physics_variable["Time step"].getFinalValue();
                        else
                            Rate = 0;
                    }

                    double newReacted = 0.0;
                    if (excesssol > 0)
                        newReacted = sciantix_variable[element + " reacted - GB"].getFinalValue() - Rate*physics_variable["Time step"].getFinalValue()*excesssol;
                    else if (excessgb > 0)
                        newReacted = sciantix_variable[element + " reacted - GB"].getFinalValue() + Rate*physics_variable["Time step"].getFinalValue()*excessgb;   
                    
                    if (newReacted < 0) newReacted = 0;
                    if (newReacted > available) newReacted = available;

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
            else
                std::cout<<"Location not yet modelled: "<<location<<std::endl;
            
        }

    }
    else if (module == "OPENCALPHAD")
    {
        std::string directoryPath = "./../../../opencalphad/";
        std::string inputPath = "inputs/thermoin.OCM";
        std::string outputPath = "outputs/thermoout";
        std::string parserPath = "parse_oc_output_to_json.py";
        std::string dataPath = "./../data/" + root["Settings"]["fission_products"]["database"].asString();
        if (location == "matrix") dataPath = "./../data/" + root["Settings"]["matrix"]["database"].asString();
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
        
        Json::Value& elements = root["Settings"]["fission_products"]["elements"];
        if (location == "matrix")
            elements = root["Settings"]["matrix"]["elements"];

        for (auto& elementName : elements.getMemberNames())
        {
            double atomsavailable(0);
            if (location == "in grain") 
                atomsavailable = (
                    sciantix_variable[elementName + " produced"].getInitialValue() -
                    sciantix_variable[elementName + " decayed"].getInitialValue() -
                    sciantix_variable[elementName + " reacted - GB"].getInitialValue() -
                    sciantix_variable[elementName + " at grain boundary"].getInitialValue() -
                    sciantix_variable[elementName + " released"].getInitialValue()
                );
            else if (location =="at grain boundary")
                atomsavailable = (
                    sciantix_variable[elementName + " produced"].getFinalValue() -
                    sciantix_variable[elementName + " decayed"].getFinalValue() -
                    sciantix_variable[elementName + " reacted - IG"].getFinalValue() -
                    sciantix_variable[elementName + " in grain"].getFinalValue() -
                    sciantix_variable[elementName + " released"].getInitialValue()
                );
            else if (location == "matrix")
                atomsavailable = 1;// sciantix_variable["Uranium content"].getFinalValue() + sciantix_variable["Oxygen content"].getFinalValue(); 
                // For matrix, we consider Uranium and Oxygen content, these are MOLES/grain not atoms
            else
                std::cout<<"Location not yet modelled: "<<location<<std::endl;
            
            if (atomsavailable > 0.0) noatoms = false;

            if (location == "matrix")
            {
                inputFile << "n="<<atomsavailable<<" x(o)=" << oxygenfraction << " ";
                break;
            }
            else
                inputFile << "n(" << elementName << ")=" << atomsavailable/avogadro_number << " ";
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
        std::vector<std::string> elementNames = elements.getMemberNames();

        std::string elements_str = "[";
        for (size_t i = 0; i < elementNames.size(); ++i) {
            elements_str += "\"" + elementNames[i] + "\"";
            if (i != elementNames.size() - 1)
                elements_str += ",";
        }
        elements_str += "]";

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
                sciantix_variable["Uranium content"].addValue(- mol_u);
                sciantix_variable["Oxygen content"].addValue(- mol_o);

                sciantix_variable["Grain radius"].setFinalValue(sciantix_variable["Grain radius"].getFinalValue() * pow((total_moles - mol_u - mol_o)/total_moles, 1.0/3.0));

                GrainVaporisation(false);
            }
            std::cout<<"Vaporized matrix! active vaporisation module"<<std::endl;
            return;
        }

        // elements: update gas atoms and grain boundary variables
        for (auto& element : elements.getMemberNames()) {
            double moles(0);
            if ((SolutionPhases.isMember("gas")) && (SolutionPhases["gas"].isObject()) && (!SolutionPhases["gas"].empty()))
                moles = SolutionPhases["gas"]["elements"][element]["moles of element in phase"].asDouble();
        
            double available(0);
            if (location == "in grain") 
                available = (sciantix_variable[element + " produced"].getInitialValue()
                    - sciantix_variable[element + " decayed"].getInitialValue()
                    - sciantix_variable[element + " reacted - GB"].getInitialValue()
                    - sciantix_variable[element + " at grain boundary"].getInitialValue()
                    - sciantix_variable[element + " released"].getInitialValue()
                );
            else if (location =="at grain boundary")
                available = (sciantix_variable[element + " produced"].getFinalValue()
                    - sciantix_variable[element + " decayed"].getFinalValue()
                    - sciantix_variable[element + " reacted - IG"].getFinalValue()
                    - sciantix_variable[element + " in grain"].getFinalValue()
                    - sciantix_variable[element + " released"].getInitialValue()
                );
            else
                std::cout<<"Location not yet modelled: "<<location<<std::endl;
            
            double updateAtoms = std::min(available, moles * avogadro_number);

            if (location == "in grain") 
                sciantix_variable[ element + " reacted - IG"].setFinalValue(available - updateAtoms);            
            else if (location =="at grain boundary")
            {
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
                        Langmuir = pow(gas_constant*temperature*2*M_PI*MM, -0.5)*4*M_PI*pow(sciantix_variable["Grain radius"].getFinalValue(),2)*pressure*avogadro_number/(available-updateAtoms);

                    double excesssol = (sciantix_variable[element + " reacted - GB"].getFinalValue() - (available - updateAtoms));
                    double excessgb = (sciantix_variable[element + " at grain boundary"].getFinalValue() - (updateAtoms));
                    
                    double Rate = Langmuir;
                    if (Rate*physics_variable["Time step"].getFinalValue()>1) 
                    {
                        std::cout<<"WARNING: Langmuir rate cut at "<<temperature<<std::endl;
                        if (physics_variable["Time step"].getFinalValue())
                            Rate = 1/physics_variable["Time step"].getFinalValue();
                        else
                            Rate = 0;
                    }

                    double newReacted = 0.0;
                    if (excesssol > 0)
                        newReacted = sciantix_variable[element + " reacted - GB"].getFinalValue() - Rate*physics_variable["Time step"].getFinalValue()*excesssol;
                    else if (excessgb > 0)
                        newReacted = sciantix_variable[element + " reacted - GB"].getFinalValue() + Rate*physics_variable["Time step"].getFinalValue()*excessgb;   
                    
                    if (newReacted < 0) newReacted = 0;
                    if (newReacted > available) newReacted = available;

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
            else
                std::cout<<"Location not yet modelled: "<<location<<std::endl;    
        }
    }
    else
    {
        if (location == "in grain") return;
        else if (location == "at grain boundary")
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
        else if (location == "matrix")
        {
            GrainVaporisation(true);  
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
