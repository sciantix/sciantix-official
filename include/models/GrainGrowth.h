
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

#ifndef GRAIN_GROWTH_H
#define GRAIN_GROWTH_H

#include "ModelDeclaration.h"
#include "MatrixDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "MapModel.h"
#include "MapMatrix.h"
#include "MapSciantixVariable.h"
#include "SetMatrix.h"
#include "Matrix.h"
#include "SetVariables.h"
#include <string>

void GrainGrowth();
/**
 * @brief This routine defines the model for the grain growth phenomenon.
 * The choice of the model depends on the input_value iGrainGrowth.
 * 
 */

#endif // GRAIN_GROWTH_H
