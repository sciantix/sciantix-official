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

#ifndef SETSYSTEM_H
#define SETSYSTEM_H

#include "SciantixArray.h"
#include "InputVariable.h"
#include "System.h"
#include "Matrix.h"

/**
 * @brief Sets up the system properties in the simulation.
 * 
 * @author G. Zullo
 * @author F. Bastien
 * 
 */

System Xe_in_UO2(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors);

System Xe_in_UO2HBS(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors);

System Kr_in_UO2(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors);

System He_in_UO2(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors);

System Xe133_in_UO2(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors);

System Kr85m_in_UO2(SciantixArray<Matrix> matrices, SciantixArray<Gas> gas, SciantixArray<InputVariable> input_variable,
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> history_variable, SciantixArray<InputVariable> scaling_factors);


#endif // SETSYSTEM_H