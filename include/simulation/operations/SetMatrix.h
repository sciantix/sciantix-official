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

#ifndef SET_MATRIX_H
#define SET_MATRIX_H

#include "Simulation.h"

/**
 * @brief Sets up the matrix properties in the simulation.
 * 
 * This function initializes and maps properties for matrices
 * used in the simulation. Each matrix is set up with specific attributes and then mapped
 * for easy access throughout the simulation.
 * 
 * @author G. Zullo
 * @author F. Bastien
 * 
 */

Matrix UO2(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, 
	SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable);

Matrix UO2HBS(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, 
	SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable);

#endif