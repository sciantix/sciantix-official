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

/// SetSystem

#ifndef SET_SYSTEM_H
#define SET_SYSTEM_H

#include "MapSystem.h"
#include "SystemDeclaration.h"
#include "MapPhysicsVariable.h"
#include "PhysicsVariableDeclaration.h"
#include "GasDeclaration.h"
#include "MapGas.h"
#include "Xe_in_UO2.h"
#include "Xe133_in_UO2.h"
#include "Kr_in_UO2.h"
#include "Kr85m_in_UO2.h"
#include "He_in_UO2.h"
#include "Xe_in_UO2HBS.h"

#include <vector>

/**
 * @brief Configures the system based on the selected fuel matrix type.
 * 
 * This function initializes and maps system properties depending on the
 * value set in the input variable "iFuelMatrix". The mappings and initializations
 * are specific to whether standard UO2 or UO2 with high burnup structure (UO2-HBS)
 * is being simulated.
 */
void SetSystem();

#endif // SET_SYSTEM_H