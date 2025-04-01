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
//  Version: 2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: G.Nicodemo                                                             //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "OCcall.h"
#include "Simulation.h"
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <iostream>
#include <cstdlib>
#include <sys/stat.h>

void Simulation::OCcall() {

    std::string oc_dir = "../../OC";
    std::string oc_exec = oc_dir + "/oc6P";
    std::string oc_input_path = oc_dir + "/oc_input.OCM";
    std::string oc_output_path = oc_dir + "/oc_output.txt";

    // Ensure OC directory exists
    struct stat info;
    if (stat(oc_dir.c_str(), &info) != 0) { // Check if directory exists
        std::cerr << "Error: OC directory not found at " << oc_dir << std::endl;
        throw std::runtime_error("OpenCalphad directory is missing.");
    }

    double temperature = history_variable["Temperature"].getFinalValue();
    double stoichiometryDeviation = sciantix_variable["Stoichiometry deviation"].getFinalValue();

    double oxygen_fraction = (2 + stoichiometryDeviation) / (2 + stoichiometryDeviation + 1);

    // Create OpenCalphad input script
    std::ofstream oc_script(oc_input_path);
    if (!oc_script.is_open()) {
        throw std::runtime_error("Failed to create OpenCalphad input script.");
    }

    oc_script << "r t ./OU\n\n";
    oc_script << "set c t=" << temperature << " p=1e5 n=1 x(o)=" << oxygen_fraction << "\n\n";
    oc_script << "c e\n\n";
    oc_script << "list results 2\n\n";  // Add this line to list results
    oc_script << "fin";
    oc_script.close();

    // Execute OpenCalphad with the correct path
    std::string command1 = "cd " + oc_dir;
    std::system(command1.c_str());

    std::string command2 = " ./oc6P ";
    int returnCode = std::system(command2.c_str());

    if (returnCode != 0) {
        std::cerr << "Error: OpenCalphad execution failed." << std::endl;
        throw std::runtime_error("OpenCalphad execution failed.");
    }

    // Read OpenCalphad output
    std::ifstream oc_output(oc_output_path);
    if (!oc_output.is_open()) {
        std::cerr << "Error: Cannot read OpenCalphad output file at " << oc_output_path << std::endl;
        throw std::runtime_error("Failed to read OpenCalphad output file.");
    }

    std::string line;
    while (std::getline(oc_output, line)) {
        std::cout << line << std::endl;  // Print each line of the output
    }

    oc_output.close();
}


