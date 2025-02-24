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

#include "SetPhaseDiagram.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cmath>
#include <limits>

struct DataPoint {
    double log_P_Cs;
    double log_P_I;
    double log_P_O2;
    std::string fase;
};

std::vector<DataPoint> loadData(const std::string& filename) {
    std::vector<DataPoint> data;
    std::ifstream file(filename);
    std::string line;

    if (!file) {
        std::cerr << "Error opening file: " << filename << std::endl;
        return data;
    }

    // Read and ignore the header line
    std::getline(file, line);

    while (std::getline(file, line)) {
        std::stringstream ss(line);
        DataPoint dp;
        std::string temp;

        // Read values from the line
        ss >> dp.log_P_O2 >> dp.log_P_I >> dp.log_P_Cs >> dp.fase;

        // Handle cases where 'fase' might have spaces (if the format includes quotes or spaces)
        if (ss.fail()) {
            ss.clear();
            ss >> std::ws; // Remove leading whitespace
            std::getline(ss, dp.fase); // Read the full remaining part as a string
        }

        data.push_back(dp);
    }

    return data;
}

std::string phase_finding(double log_P_I, double log_P_Cs, double log_P_O2, const std::vector<DataPoint>& dataset) {
    if (dataset.empty()) {
        std::cerr << "Error: The dataset is empty!" << std::endl;
        return "Unknown";
    }

    double min_distance = std::numeric_limits<double>::max();
    std::string closest_phase;

    for (const auto& dp : dataset) {
        double distance = std::sqrt(std::pow(dp.log_P_I - log_P_I, 2) +
                                    std::pow(dp.log_P_Cs - log_P_Cs, 2) +
                                    std::pow(dp.log_P_O2 - log_P_O2, 2));

        if (distance < min_distance) {
            min_distance = distance;
            closest_phase = dp.fase;
        }
    }

    return closest_phase;
}
void Simulation::setPhaseDiagram()
{
    std::string filename = "../../output.txt";
    std::vector<DataPoint> dataset = loadData(filename);

    double Temperature = history_variable["Temperature"].getFinalValue();
    double Cs = sciantix_variable["Cs at grain boundary"].getFinalValue();
    double I = sciantix_variable["I at grain boundary"].getFinalValue();

    double equilibrium_constant = exp(-25300.0 / history_variable["Temperature"].getFinalValue() + 4.64 + 1.04 * (0.0007 * history_variable["Temperature"].getFinalValue() - 0.2));
    double steam_pressure = 1;
    double gap_oxygen_partial_pressure = pow(pow(equilibrium_constant, 2) * pow(steam_pressure, 2) / 4, 1.0 / 3.0);

    double log_P_I = log10(I*boltzmann_constant*Temperature);
    double log_P_O2 = log10(gap_oxygen_partial_pressure);
    std::cout << log_P_O2 << std::endl;
    double log_P_Cs = log10(Cs*boltzmann_constant*Temperature);

    std::string phase = phase_finding(log_P_I, log_P_Cs, log_P_O2, dataset);
    std::cout << "The dominant phase is: " << phase << std::endl;

    // bool converged = false;
    // int iterations = 0;
    // while (!converged && iterations < 100) 
    // {
    //     iterations++;
        
    //     std::string newPhase = getStablePhase(log_P_I, log_P_O2, log_P_Cs, phases);
    //     if (newPhase == stablePhase) 
    //     {
    //         converged = true;
    //     } 
    //     else 
    //     {
    //         if (newPhase == "UO2") 
    //         {
    //             O2 *= 0.9; // Oxygen is consumed
    //         } 
    //         else if (newPhase == "CsI + U4O9") 
    //         {
    //             Cs *= 0.8; // Cs reacts into a solid phase
    //         }
            
    //         double log_P_I = log10(I*boltzmann_constant*Temperature);
    //         double log_P_O2 = log10(O2*boltzmann_constant*Temperature);
    //         double log_P_Cs = log10(Cs*boltzmann_constant*Temperature);
            
    //         stablePhase = newPhase;
    //     }
    // }

    // sciantix_variable["Cs at grain boundary"].setFinalValue(0);
    // sciantix_variable["Cs reacted"].addValue(Cs);
    // sciantix_variable["I at grain boundary"].setFinalValue(0);
    // sciantix_variable["I reacted"].addValue(I);
    // sciantix_variable["CsI at grain boundary"].addValue(Cs+I);
    // sciantix_variable["CsI produced"].addValue(Cs+I);

    return;
}