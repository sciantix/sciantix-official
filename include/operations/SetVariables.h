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

#ifndef SET_VARIABLE_H
#define SET_VARIABLE_H

#include <vector>

#include "InputVariable.h"
#include "SciantixArray.h"
#include "SetVariablesFunctions.h"

/**
 * @brief This routine initializes vectors that hold physics variables, history variables,
 * Sciantix variables, and input variables. It also sets up the diffusion modes,
 * maps, and scaling factors required for the simulation. The function ensures that
 * each variable is correctly initialized based on the model options provided by the user.
 *
 * @param Sciantix_options Array of integers specifying user-selected options for various simulation models.
 * @param Sciantix_history Array of doubles used to store historical variables of the simulation.
 * @param Sciantix_variables Array of doubles representing the current variables in the simulation.
 * @param Sciantix_scaling_factors Array of doubles used for scaling various parameters within the model.
 * @param Sciantix_diffusion_modes Array of doubles that describe diffusion modes used in gas behavior modeling.
 * @param Sciantix_thermochemistry Array of doubles representing the variables related to the thermochemistry module.
 * @param Sciantix_thermochemistry_options Array of string representing the options for the thermochemistry module.
 * 
 * @author D. Pizzocri
 * @author T. Barani
 * @author G. Zullo
 * @author F. Bastien
 * 
 */
void SetVariables(
    int Sciantix_options[], 
	double Sciantix_history[], 
	double Sciantix_variables[], 
	double Sciantix_scaling_factors[], 
	double Sciantix_diffusion_modes[],
	double Sciantix_thermochemistry[],
	std::string Sciantix_thermochemistry_options[],
    SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &history_variable,
    SciantixArray<SciantixVariable> &sciantix_variable,
	SciantixArray<SciantixVariable> &physics_variable,
	std::vector<double> &modes_initial_conditions,
	SciantixArray<Variable> &scaling_factors,
	SciantixArray<ThermochemistryVariable> &thermochemistry_variable
);

#endif