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
#include <unistd.h>
#include <filesystem>
#include <regex>

namespace fs = std::filesystem;

void Simulation::OCcall() {

    fs::path oc_dir = fs::canonical("../../OC");
    fs::path oc_exec = oc_dir / "oc6P";
    fs::path oc_input_path = oc_dir / "oc_input.OCM";
    fs::path oc_output_path = fs::current_path() / "OC_summary.txt";  

    if (!fs::exists(oc_dir)) {
        throw std::runtime_error("OC directory not found at " + oc_dir.string());
    }
    if (!fs::exists(oc_exec) || access(oc_exec.c_str(), X_OK) != 0) {
        throw std::runtime_error("oc6P not found or not executable at " + oc_exec.string());
    }

    double time = history_variable["Time"].getFinalValue();
    double temperature = history_variable["Temperature"].getFinalValue();
    double stoichiometryDeviation = sciantix_variable["Stoichiometry deviation"].getFinalValue();
    double oxygen_fraction = (2 + stoichiometryDeviation) / (2 + stoichiometryDeviation + 1);

    // Write OC input script in OC/
    {
        std::ofstream oc_script(oc_input_path);
        if (!oc_script.is_open()) {
            throw std::runtime_error("Cannot create input file: " + oc_input_path.string());
        }
        oc_script << "r t ./OU\n\n";
        oc_script << "set c t=" << temperature << " p=1e5 n=1 x(o)=" << oxygen_fraction << "\n\n";
        oc_script << "c e\n\n";
        oc_script << "list results 2\n\n";
        oc_script << "fin";
    }

    // Run OpenCalphad
    std::string raw_output_path = (oc_dir / "oc_output_raw.txt").string();
    std::string command = "cd " + oc_dir.string() +
                          " && ./oc6P " + oc_input_path.filename().string() +
                          " > oc_output_raw.txt";
    int returnCode = std::system(command.c_str());
    if (returnCode != 0) {
        throw std::runtime_error("OpenCalphad execution failed, return code " + std::to_string(returnCode));
    }

    // Parse raw output
    std::ifstream oc_output_in(oc_dir / "oc_output_raw.txt");
    if (!oc_output_in.is_open()) {
        throw std::runtime_error("Failed to read OpenCalphad raw output.");
    }

    std::ofstream summary(oc_output_path, std::ios::app);
    if (!summary.is_open()) {
        throw std::runtime_error("Failed to write results summary file.");
    }

    summary << "\n==============================" << std::endl;
    summary << "Time = " << time << " s" << std::endl;
    summary << "Temperature = " << temperature << " K" << std::endl;
    summary << "Stoichiometry deviation = " << stoichiometryDeviation << std::endl;
    summary << "Oxygen fraction = " << oxygen_fraction << std::endl;

    std::regex re_energy(R"(G=\s*([-0-9.E+]+)\s*J.*H=([-0-9.E+]+)\s*J)");
    std::regex re_component(R"(^\s*([A-Za-z0-9_]+)\s+([0-9.E+-]+)\s+([0-9.E+-]+)\s+([0-9.E+-]+)\s+([0-9.E+-]+))");
    std::regex re_phase(R"(^([A-Za-z0-9_#]+)\.*\s+E\s+([0-9.E+-]+))");

    std::string line;
    while (std::getline(oc_output_in, line)) {
        std::smatch match;

        if (std::regex_search(line, match, re_energy)) {
            double G = std::stod(match[1]);
            double H = std::stod(match[2]);
            summary << ">> Gibbs free energy G = " << G
                    << " J, Enthalpy H = " << H << " J" << std::endl;
        }

        if (std::regex_search(line, match, re_component)) {
            std::string name = match[1];
            double moles = std::stod(match[2]);
            double x = std::stod(match[3]);
            double muRT = std::stod(match[4]);
            double activity = std::stod(match[5]);
            summary << ">> Component " << name
                    << " : Moles=" << moles
                    << " , x=" << x
                    << " , mu/RT=" << muRT
                    << " , activity=" << activity
                    << std::endl;
        }

        if (std::regex_search(line, match, re_phase)) {
            std::string phaseName = match[1];
            double phaseMoles = std::stod(match[2]);
            summary << ">> Phase " << phaseName
                    << " : mole fraction = " << phaseMoles << std::endl;
        }
    }

    oc_output_in.close();
    summary.close();
}
