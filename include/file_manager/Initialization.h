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
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef INITIALIZATION_H
#define INITIALIZATION_H

#include <iostream>
#include <vector>
#include <cmath>

/**
 * @brief Initializes SCIANTIX internal variables with initial conditions.
 * 
 * @author D. Pizzocri
 * @author T. Barani
 * @author G. Zullo
 * 
 */
void Initialization(
	double Sciantix_history[],
	double Sciantix_variables[],
	double Sciantix_diffusion_modes[],
	std::vector<double> Temperature_input,
	std::vector<double> Fissionrate_input,
	std::vector<double> Hydrostaticstress_input,
	std::vector<double> Steampressure_input
);

#endif //INITIALIZATION_H