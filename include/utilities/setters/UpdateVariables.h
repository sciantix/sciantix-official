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
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////
#ifndef UPDATE_VARIABLES_H
#define UPDATE_VARIABLES_H

#include "SciantixVariableDeclaration.h"
#include "SciantixDiffusionModeDeclaration.h"

#include "MapSciantixVariable.h"

#include <vector>
#include <map>

/**
 * @brief Updates the Sciantix variables and diffusion modes arrays with the current values.
 * 
 * This function fills the Sciantix_variables and Sciantix_diffusion_modes arrays with the final values
 * from the sciantix_variable and modes_initial_conditions respectively.
 * 
 * @param Sciantix_variables[] Array to store the Sciantix variable values.
 * @param Sciantix_diffusion_modes[] Array to store the Sciantix diffusion modes.
 */
void UpdateVariables(double Sciantix_variables[], double Sciantix_diffusion_modes[]);

#endif // UPDATE_VARIABLES_H