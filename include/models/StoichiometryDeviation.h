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
//  Year: 2023                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef STOICHIOMETRY_DEVIATION_H
#define STOICHIOMETRY_DEVIATION_H

#include "Simulation.h"

/**
 * @brief Evaluates the deviation in stoichiometry within the nuclear material and its effects.
 * 
 * @author G. Petrosillo
 * @author G. Zullo
 * 
 */

double BlackburnThermochemicalModel(double stoichiometry_deviation, double temperature, SciantixArray<SciantixVariable> &sciantix_variable);

#endif // STOICHIOMETRY_DEVIATION_H