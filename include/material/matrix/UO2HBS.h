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

#ifndef UO2_HBS_H
#define UO2_HBS_H

#include "MatrixDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "MapSciantixVariable.h"
#include "HistoryVariableDeclaration.h"
#include "MapHistoryVariable.h"
#include "ModelDeclaration.h"
#include "SystemDeclaration.h"
#include "MapSystem.h"
#include "MapModel.h"
#include "SciantixScalingFactorDeclaration.h"
#include <cmath>

/**
 * @brief This routine defines the physical proprieties of the matrix UO2HBS.
 * UO2HBS: UO2 in case of 100% High Burnup Structure (HBS) conditions.
 *
 */
void UO2HBS();

#endif // UO2_HBS_H