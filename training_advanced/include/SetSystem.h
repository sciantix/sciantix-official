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

void SetSystem();

// void System::setFissionGasDiffusivity(int input_value)
/** 
 * The intra-granular fission gas (xenon and krypton) diffusivity within the fuel grain is set according to the input_variable iFGDiffusionCoefficient
 * 
 */

#endif // SET_SYSTEM_H
