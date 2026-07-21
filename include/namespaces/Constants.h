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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef CONSTANTS_H
#define CONSTANTS_H

#include <map>
#include <string>

/**
 * @file Constants.h
 * @brief Defines fundamental physical constants used in calculations.
 *
 * This file contains definitions of fundamental constants such as the Boltzmann constant and
 * Avogadro's number. These constants are commonly used in various scientific and engineering
 * calculations.
 *
 * @author G. Zullo
 * @author G. Nicodemo
 * @author E. Cappellari
 *
 */

const double boltzmann_constant    = 1.380651e-23;   // (J/K)
const double boltzmann_constant_eV = 8.62e-5;        // (eV/K)
const double avogadro_number       = 6.02214076e23;  // (at/mol)
const double molar_mass_Oxygen     = 15.999;         // g/mol
const double molar_mass_Chromium   = 51.9961;        // g/mol
const double calorie               = 4.186;          // J
const double gas_constant          = 8.3143;         // J/(mol K)

// Blackburn (1973) correlation for the equilibrium constant of UO2 oxidation,
// ln(K) = 2 ln(x(x+2)/(1-x)) + 108 x^2 - blackburn_enthalpy/T + blackburn_entropy.
// Used in Solver::NewtonBlackburn and StoichiometryDeviation.
const double blackburn_enthalpy = 32700.0;  // (K)
const double blackburn_entropy  = 9.92;     // (/)

const double reference_oxygen_pressure_atm = 0.1013;  // MPa
const double reference_oxygen_pressure_bar = 0.1;     // MPa
const double max_prescribed_om_ratio = 1.9999;        // (/) keep prescribed O/M strictly below stoichiometry (2.0)

// Atomic masses (g/mol) of the elements/species.
const std::map<std::string, double> thermochemistry_atomic_masses = {
    {"Cs", 132.90545196},
    {"I", 126.90447},
    {"Mo", 95.95},
    {"Ba", 137.327},
    {"O", 15.999},
    {"Te", 127.60},
    {"U", 238.02891},
    {"Pu", 239.052},
    {"Va", 0.0},
    {"Pd", 106.42},
    {"Rh", 102.91},
    {"Ru", 101.07},
    {"Tc", 98.906}
};

#endif
