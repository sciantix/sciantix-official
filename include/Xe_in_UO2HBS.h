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

#ifndef XE_IN_UO2HBS_H
#define XE_IN_UO2HBS_H

#include "SystemDeclaration.h"
#include "MatrixDeclaration.h"
#include "MapMatrix.h"
#include "SciantixScalingFactorDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "MapSciantixVariable.h"

/**
 * @brief Sets properties for the "Xenon in UO2-HBS" system.
 * 
 * This function initializes the properties of the "Xenon in UO2-HBS" system
 * and adds it to the sciantix_system vector.
 */

void Xe_in_UO2HBS();

#endif // XE_IN_UO2HBS_H