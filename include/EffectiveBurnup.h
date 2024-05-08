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

#ifndef EFFECTIVE_BURNUP_H
#define EFFECTIVE_BURNUP_H

#include "SciantixVariableDeclaration.h"
#include "MapSciantixVariable.h"
#include "ModelDeclaration.h"
#include "SetMatrix.h"

void EffectiveBurnup();
/**
 * @brief Defines the sciantix model "Effective burnup".
 * 
 * This function calculates the local effective burnup based on the local fission rate density and temperature.
 * 
 * @ref G. Khvostov et al., WRFPM-2005, Kyoto, Japan, 2005
 * 
 * @author
 * A. Magni
 * E. Redaelli
 * G. Zullo
*/

#endif // EFFECTIVE_BURNUP_H
