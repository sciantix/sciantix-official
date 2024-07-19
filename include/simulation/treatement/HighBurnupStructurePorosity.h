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


#ifndef HIGH_BURNUP_STRUCTURE_POROSITY_H
#define HIGH_BURNUP_STRUCTURE_POROSITY_H


#include "SciantixVariableDeclaration.h"
#include "MapSciantixVariable.h"
#include "ModelDeclaration.h"
#include "SetMatrix.h"
#include "Burnup.h"
#include "GasDeclaration.h"
#include "MapGas.h"
#include "SystemDeclaration.h"
#include "MapSystem.h"

/**
 * @brief This routine sets the model for High burnup structure porosity evolution
 *
 * @author
 * A. Magni
 * E. Redaelli
 * G. Zullo
 */
void HighBurnupStructurePorosity();

#endif // HIGH_BURNUP_STRUCTURE_POROSITY_H