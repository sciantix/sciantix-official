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

void Simulation::SetPhaseDiagram()
{
    if (input_variable["iStoichiometryDeviation"].getValue() != 7) return;
    // std::string filename = "../output.txt";
    // std::vector<DataPoint> dataset = loadData(filename);

    double Temperature = history_variable["Temperature"].getFinalValue();
    double Cs = sciantix_variable["Cs at grain boundary"].getFinalValue() + sciantix_variable["Cs reacted"].getFinalValue();
    double I = sciantix_variable["I at grain boundary"].getFinalValue() + sciantix_variable["I reacted"].getFinalValue();

    // Chevalier 2002
    //double dG_O2 = - 1084.9112*1e3 - Temperature * 77.02744; // J mol-1
    double log_P_O2 = log10(sciantix_variable["Fuel oxygen partial pressure"].getFinalValue()*10); //bar
    std::cout << log_P_O2 << std::endl;

    double log_P_I = log10(I*boltzmann_constant*Temperature*10);
    double log_P_Cs = log10(Cs*boltzmann_constant*Temperature*10);

    // std::string phase = phase_finding(log_P_I, log_P_Cs, log_P_O2, dataset);
    // std::cout << "The dominant phase is: " << phase << std::endl;

    StablePhaseResult Update = Simulation::SetStablePhase(Temperature, log_P_Cs, log_P_I, log_P_O2);
    
    sciantix_variable["Cs at grain boundary"].setFinalValue(Update.new_set[0]);
    sciantix_variable["I at grain boundary"].setFinalValue(Update.new_set[1]);
    sciantix_variable["Cs reacted"].setFinalValue(Cs - Update.new_set[0]);
    sciantix_variable["I reacted"].setFinalValue(I - Update.new_set[1]);
    if (!Update.right_bounded.empty()) {
        std::cout << Update.right_bounded[0] << std::endl;
        sciantix_variable[Update.right_bounded[0]].setFinalValue(Update.compound);
    } else {
        std::cout << "right_bounded è vuoto" << std::endl;
    }
    

    return;
}

// Reaction data structure
struct Reaction {
    std::vector<std::string> left;
    std::vector<std::string> right_bounded;
    std::vector<std::string> right_gas;
    double Cs;
    double O2;
    double I;
    std::vector<double> G_parameters; // {A, B, C, D, E}
    double T_min;
    double T_max;
};

// ΔG(T) = A + B*T + C*T*ln(T) + D*T² + E/T
double delta_g(double T, double A, double B, double C, double D, double E) 
{
    return A + B * T + C * T * std::log(T) + D * pow(T,2) + E / T;
}

// Equilibrium constant K_eq(T) = exp(-ΔG/(R*T))
double K_eq(double T, double A, double B, double C, double D, double E) 
{
    return std::exp(- delta_g(T, A, B, C, D, E) / (boltzmann_constant * T));
}

Simulation::StablePhaseResult Simulation::SetStablePhase(double Temperature, double logCs, double logO2, double logI)
{
    // Build the equilibria dictionary (using a map)
    std::map<std::string, Reaction> equilibria;

    equilibria["2Cs+0.5O2=Cs2O_s"] = {
        {"Cs", "O2"},         // left
        {"Cs2O"},             // right_bounded (solid)
        {},                   // right_gas
        -2,                   // Cs coefficient
        -0.5,                 // O2 coefficient
         0,                   // I coefficient
        {-503991, 389.398, -11.058, -0.01456, 0}, // G_parameters (J mol⁻¹)
         298.15,              // T_min
         768                  // T_max
    };

    equilibria["2Cs+0.5O2=Cs2O_l"] = {
        {"Cs", "O2"},
        {"Cs2O"}, 
        {},
        -2,
        -0.5,
         0,
        {-499808, 590.341, -43.980, 0.001495, 0},
         768,
         1000
    };

    equilibria["2Cs+O2=Cs2O2_s"] = {
        {"Cs", "O2"},
        {"Cs2O2"},
        {},
        -2,
        -1,
         0,
        {-598839, 486.279, -15.111, -0.015, 0},
         298.15,
         867
    };

    equilibria["2Cs+O2=Cs2O2_l"] = {
        {"Cs", "O2"},
        {"Cs2O2"},
        {},
        -2,
        -1,
         0,
        {-606006, 801.579, -62.936, 0.002695, 0},
         867,
         1000
    };

    equilibria["Cs+O2=CsO2_s1"] = {
        {"Cs", "O2"},
        {"CsO2"},
        {},
        -1,
        -1,
         0,
        {-267731, 299.194, -5.564, -0.03906, 0},
         298.15,
         403
    };

    equilibria["Cs+O2=CsO2_s2"] = {
        {"Cs", "O2"},
        {"CsO2"},
        {},
        -1,
        -1,
         0,
        {-374811, 510.230, -40.751, 0.004648, 0},
         403,
         723
    };

    equilibria["Cs+O2=CsO2_l"] = {
        {"Cs", "O2"},
        {"CsO2"},
        {},
        -1,
        -1,
         0,
        {-364308, 561.647, -50.606, 0.003176, 0},
         723,
         1000
    };

    equilibria["Cs+I=CsI_s"] = {
        {"Cs", "I"},
        {"CsI"},
        {},
        -1,
         0,
        -1,
        {-533585, 266.381, -3.953, -0.011305, 0},
         298.15,
         903.69
    };

    equilibria["Cs+I=CsI_l"] = {
        {"Cs", "I"},
        {"CsI"},
        {},
        -1,
         0,
        -1,
        {-523534, 419.454, -29.576, 0.0000675, 0},
         903.69,
         1000
    };

    // Filter reactions valid at the given Temperature
    std::map<std::string, Reaction> equilibria_Temperature;
    for (const auto& pair : equilibria) {
        const Reaction& rxn = pair.second;
        if (Temperature >= rxn.T_min && Temperature <= rxn.T_max)
            equilibria_Temperature[pair.first] = rxn;
    }

    // Calculate the "K_eq value" for each reaction
    std::map<std::string, double> K_eq_values;
    for (const auto& pair : equilibria_Temperature) {
        const std::string& name = pair.first;
        const Reaction& rxn = pair.second;
        double keq = K_eq(Temperature,
                          rxn.G_parameters[0], rxn.G_parameters[1],
                          rxn.G_parameters[2], rxn.G_parameters[3],
                          rxn.G_parameters[4]);
        double value = (std::log10(keq) - (rxn.Cs * logCs + rxn.I * logI + rxn.O2 * logO2)) / (-rxn.Cs);
        K_eq_values[name] = value;
    }

    // Choose the reaction with the maximum computed value
    std::string min_phase;
    double max_value = -std::numeric_limits<double>::infinity();
    for (const auto& pair : K_eq_values) {
        if (pair.second > max_value) {
            max_value = pair.second;
            min_phase = pair.first;
        }
    }

    double K_eq_phase = 0.0;
    if (equilibria_Temperature.find(min_phase) != equilibria_Temperature.end()) {
        const Reaction& rxn = equilibria_Temperature[min_phase];
        K_eq_phase = K_eq(Temperature,
                          rxn.G_parameters[0], rxn.G_parameters[1],
                          rxn.G_parameters[2], rxn.G_parameters[3],
                          rxn.G_parameters[4]);
    }

    // If the computed value is negative, default to "Cs + I + O2"
    if (max_value < 0.0) {
        min_phase = "Cs + I + O2";
        K_eq_phase = std::numeric_limits<double>::quiet_NaN();
    }

    std::vector<double> new_set(3);
    double L_max = 0;
    // If a valid phase is found, compute BG_values, L_max and new_set
    if (min_phase != "Cs + I + O2") {
        // BG_values: [ (10^logCs)/(R*T), (10^logI)/(R*T), (10^logO2)/(R*T) ]
        std::vector<double> BG_values(3);
        BG_values[0] = std::pow(10.0, logCs) / (10 * boltzmann_constant * Temperature);
        BG_values[1] = std::pow(10.0, logI)  / (10 * boltzmann_constant * Temperature);
        BG_values[2] = std::pow(10.0, logO2) / (10 * boltzmann_constant * Temperature);

        // Stoichiometric coefficients for the chosen reaction
        const Reaction& rxn = equilibria_Temperature[min_phase];
        std::vector<double> v_values = { rxn.Cs, rxn.I, rxn.O2 };

        // Compute L_max values for each element (taking care of division by zero)
        std::vector<double> L_max_values(3);
        for (int i = 0; i < 3; ++i) {
            if (v_values[i] != 0)
                L_max_values[i] = -BG_values[i] / v_values[i];
            else
                L_max_values[i] = std::numeric_limits<double>::infinity();
        }

        // L_max is the minimum of the L_max_values
        L_max = std::numeric_limits<double>::infinity();
        for (double val : L_max_values) {
            if (val < L_max)
                L_max = val;
        }

        // new_set = BG_values + v_values * L_max (elementwise)
        for (int i = 0; i < 3; ++i) {
            new_set[i] = BG_values[i] + v_values[i] * L_max;
        }

        // Output the new_set values
        std::cout << "Min_phase: " << min_phase << std::endl;
        std::cout << "New set values: ";
        for (double val : new_set) {
            std::cout << val << " ";
        }
        std::cout << std::endl;
    } 
    else 
    {
        std::cout << "No stable phase found, min_phase: " << min_phase << std::endl;
        std::vector<double> BG_values(3);
        BG_values[0] = std::pow(10.0, logCs) / (10 * boltzmann_constant * Temperature);
        BG_values[1] = std::pow(10.0, logI)  / (10 * boltzmann_constant * Temperature);
        BG_values[2] = std::pow(10.0, logO2) / (10 * boltzmann_constant * Temperature);
        
        // new_set = BG_values + v_values * L_max (elementwise)
        for (int i = 0; i < 3; ++i) {
            new_set[i] = BG_values[i];
        }
    }
    
    // Build the result structure
    StablePhaseResult result;
    result.new_set = new_set;
    
    // If a valid reaction was found, include its right_bounded species; otherwise, return an empty vector.
    if (min_phase != "Cs + I + O2")
    {
        result.right_bounded = equilibria_Temperature[min_phase].right_bounded;
        result.compound = L_max;
    }
    else
    {
        result.right_bounded = {};
        result.compound = 0.0;  
    }
    
    return result;
}

// struct DataPoint {
//     double log_P_Cs;
//     double log_P_I;
//     double log_P_O2;
//     std::string fase;
// };

// std::vector<DataPoint> loadData(const std::string& filename) {
//     std::vector<DataPoint> data;
//     std::ifstream file(filename);
//     std::string line;

//     if (!file) {
//         std::cerr << "Error opening file: " << filename << std::endl;
//         return data;
//     }

//     // Read and ignore the header line
//     std::getline(file, line);

//     while (std::getline(file, line)) {
//         std::stringstream ss(line);
//         DataPoint dp;
//         std::string temp;

//         // Read values from the line
//         ss >> dp.log_P_O2 >> dp.log_P_I >> dp.log_P_Cs >> dp.fase;

//         // Handle cases where 'fase' might have spaces (if the format includes quotes or spaces)
//         if (ss.fail()) {
//             ss.clear();
//             ss >> std::ws; // Remove leading whitespace
//             std::getline(ss, dp.fase); // Read the full remaining part as a string
//         }

//         data.push_back(dp);
//     }

//     return data;
// }

// std::string phase_finding(double log_P_I, double log_P_Cs, double log_P_O2, const std::vector<DataPoint>& dataset) {
//     if (dataset.empty()) {
//         std::cerr << "Error: The dataset is empty!" << std::endl;
//         return "Unknown";
//     }

//     double min_distance = std::numeric_limits<double>::max();
//     std::string closest_phase;

//     for (const auto& dp : dataset) {
//         double distance = std::sqrt(std::pow(dp.log_P_I - log_P_I, 2) +
//                                     std::pow(dp.log_P_Cs - log_P_Cs, 2) +
//                                     std::pow(dp.log_P_O2 - log_P_O2, 2));

//         if (distance < min_distance) {
//             min_distance = distance;
//             closest_phase = dp.fase;
//         }
//     }

//     return closest_phase;
//}