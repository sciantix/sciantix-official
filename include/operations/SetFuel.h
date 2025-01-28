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

#ifndef SET_FUEL_H
#define SET_FUEL_H

#include "Fuel.h"
#include "Simulation.h"

/**
 * @brief Sets up the fuel properties.
 * 
 * This function initializes and maps properties for monolitic and composite fuels. 
 * Each fuel is set up with specific attributes and then mapped for easy access.
 * 
 * @author G. Nicodemo
 * 
 */

Fuel monolithic_UO2(SciantixArray<Fuel> &Fuels, SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable);

Fuel monolithic_MOX(SciantixArray<Fuel> &Fuels, SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable);

Fuel TRISO_in_slab(SciantixArray<Fuel> &Fuels, SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable);

#endif