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

    double Temperature = history_variable["Temperature"].getFinalValue();
    double Cs = sciantix_variable["Cs at grain boundary"].getFinalValue() + sciantix_variable["Cs reacted"].getFinalValue();
    double I = sciantix_variable["I at grain boundary"].getFinalValue() + sciantix_variable["I reacted"].getFinalValue();

    // Intergranular bubble pressure p = kTng/Onv (bar)
    double bubble_pressure;
    if(sciantix_variable["Intergranular vacancies per bubble"].getInitialValue())
        bubble_pressure = (1e-5 * boltzmann_constant *  Temperature * sciantix_variable["Intergranular atoms per bubble"].getInitialValue()/(sciantix_variable["Intergranular vacancies per bubble"].getInitialValue() * matrices["UO2"].getSchottkyVolume()));
    else
        bubble_pressure = 1.0;

    // Chevalier 2002
    //double dG_O2 = - 1084.9112*1e3 - Temperature * 77.02744; // J mol-1
    double log_P_O2 = log10(sciantix_variable["Fuel oxygen partial pressure"].getFinalValue()*10/bubble_pressure); // MPa -> bar
    double log_P_I = log10(I*boltzmann_constant*Temperature*1e-5/bubble_pressure); // Pa -> bar
    double log_P_Cs = log10(Cs*boltzmann_constant*Temperature*1e-5/bubble_pressure); // Pa -> bar

    std::cout << log_P_Cs << log_P_O2 << log_P_I <<std::endl;     

    StablePhaseResult Update = Simulation::SetStablePhase(Temperature, log_P_Cs, log_P_O2, log_P_I, bubble_pressure);
    
    sciantix_variable["Cs at grain boundary"].setFinalValue(Update.new_set[0]);
    sciantix_variable["I at grain boundary"].setFinalValue(Update.new_set[2]);
    sciantix_variable["Cs reacted"].setFinalValue(Cs - Update.new_set[0]);
    sciantix_variable["I reacted"].setFinalValue(I - Update.new_set[2]);
    if (!Update.right_bounded.empty()) 
    {
        std::cout << Update.right_bounded[0] << std::endl;
        sciantix_variable[Update.right_bounded[0]].setFinalValue(Update.compound);
    }
    

    return;
}

// Reaction data structure
struct Reaction {
    std::vector<std::string> left; // reactants
    std::vector<std::string> right_bounded; // products (solid, liquid)
    std::vector<std::string> right_gas; // products (gas)
    double Cs;
    double O2;
    double I;
    std::vector<double> G_parameters; // {A, B, C, D, E} , J/mol
    double T_min; //K
    double T_max; //K
};

// ΔG(T) = A + B*T + C*T*ln(T) + D*T² + E/T
double delta_g(double T, double A, double B, double C, double D, double E)
{
    return A + B * T + C * T * log(T) + D * pow(T,2) + E * pow(T, -1);
}

// Equilibrium constant K_eq(T) = exp(-ΔG/(R*T))
double K_eq(double T, double A, double B, double C, double D, double E) 
{
    return exp(- delta_g(T, A, B, C, D, E) / (gas_constant * T));
}


Simulation::StablePhaseResult Simulation::SetStablePhase(double Temperature, double logCs, double logO2, double logI, double Pressure)
{
    // Build the equilibria dictionary with the Gibbs energy for reactions from Walle (2005)
    std::map<std::string, Reaction> equilibria;

    equilibria["2Cs+0.5O2=Cs2O_s"] = {
        {"Cs", "O2"},         
        {"Cs2O"},             
        {},                   
        -2,                   
        -0.5,                 
         0,         
        {-503991, 389.398, -11.058, -0.01456, 0},
         298.15,         
         768
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
    for (const auto& pair : equilibria) 
    {
        const Reaction& rxn = pair.second;
        if (Temperature >= rxn.T_min && Temperature <= rxn.T_max)
            equilibria_Temperature[pair.first] = rxn;
    }

    // Calculate the equilibrium constant value for each reaction, with respect to the # of Cs atoms
    std::map<std::string, double> K_eq_values;
    for (const auto& pair : equilibria_Temperature) 
    {
        const std::string& name = pair.first;
        const Reaction& rxn = pair.second;
        double keq = K_eq(Temperature,
                          rxn.G_parameters[0], rxn.G_parameters[1],
                          rxn.G_parameters[2], rxn.G_parameters[3],
                          rxn.G_parameters[4]);
        double value = (log10(keq) - (rxn.Cs * logCs + rxn.I * logI + rxn.O2 * logO2)) / (-rxn.Cs);
        K_eq_values[name] = value;
    }

    // Choose the reaction with the maximum computed value
    std::string min_phase;
    double max_value = -std::numeric_limits<double>::infinity();
    for (const auto& pair : K_eq_values) 
    {
        if (pair.second > max_value) {
            max_value = pair.second;
            min_phase = pair.first;
        }
    }

    double K_eq_phase = 0.0;
    if (equilibria_Temperature.find(min_phase) != equilibria_Temperature.end()) 
    {
        const Reaction& rxn = equilibria_Temperature[min_phase];
        K_eq_phase = K_eq(Temperature,
                          rxn.G_parameters[0], rxn.G_parameters[1],
                          rxn.G_parameters[2], rxn.G_parameters[3],
                          rxn.G_parameters[4]);
    }

    // If the computed value is negative, default to "Cs + I + O2"
    if (max_value < 0.0) 
    {
        min_phase = "Cs + I + O2";
        K_eq_phase = std::numeric_limits<double>::quiet_NaN();
    }

    // Available atoms
    std::vector<double> BG_values(3);
    BG_values[0] = pow(10.0, logCs) * Pressure / (1e-5 * boltzmann_constant * Temperature);
    BG_values[1] = pow(10.0, logO2) * Pressure / (1e-5 * boltzmann_constant * Temperature);
    BG_values[2] = pow(10.0, logI) * Pressure / (1e-5 * boltzmann_constant * Temperature);

    std::vector<double> new_set(3);
    double L_max = 0;

    // Compute reactants amount for the chosen reaction
    if (min_phase != "Cs + I + O2") 
    {
        // Stoichiometric coefficients for the chosen reaction
        const Reaction& rxn = equilibria_Temperature[min_phase];
        std::vector<double> v_values = { rxn.Cs, rxn.O2, rxn.I };

        // Compute the maximum allowable amount of reactants
        std::vector<double> L_max_values(3);
        for (int i = 0; i < 3; ++i) {
            if (v_values[i] != 0)
                L_max_values[i] = - BG_values[i] / v_values[i];
            else
                L_max_values[i] = std::numeric_limits<double>::infinity();
        }

        // Find the minimum value
        L_max = std::numeric_limits<double>::infinity();
        for (double val : L_max_values) 
        {
            if (val < L_max)
                L_max = val;
        }

        // Non-reacted atoms = Initial atoms - Reacted ones according to reaction stoichiometry
        for (int i = 0; i < 3; ++i) 
        {
            new_set[i] = BG_values[i] + v_values[i] * L_max;
        }

        std::cout << "Min_phase: " << min_phase << std::endl;
        std::cout << "Before: ";
        for (double val : BG_values) {
            std::cout << val << " ";
        }
        std::cout << std::endl;
        std::cout << "After: ";
        for (double val : new_set) {
            std::cout << val << " ";
        }
        std::cout << std::endl;
    } 
    else 
    {
        std::cout << "No stable phase found, min_phase: " << min_phase << std::endl;
        // Non-reacted atoms = Initial atoms
        for (int i = 0; i < 3; ++i) 
        {
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