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


void setSystem(SciantixArray<System> &system, SciantixArray<Matrix> matrix, SciantixArray<InputVariable> &input_variable);

System Xe_in_UO2(SciantixArray matrix, SciantixArray input_variable);
System Xe_in_UO2HBS(SciantixArray matrix, SciantixArray input_variable);
System Kr_in_UO2(SciantixArray matrix, SciantixArray input_variable);
System He_in_UO2(SciantixArray matrix, SciantixArray input_variable);
System Xe133_in_UO2(SciantixArray matrix, SciantixArray input_variable):
System Kr85m_in_UO2(SciantixArray matrix, SciantixArray input_variable);


#endif // SETSYSTEM_H