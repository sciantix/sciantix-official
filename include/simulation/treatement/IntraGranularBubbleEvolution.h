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


#ifndef INTRA_GRANULAR_BUBBLE_EVOLUTION_H
#define INTRA_GRANULAR_BUBBLE_EVOLUTION_H



#include "ModelDeclaration.h"
#include "MapModel.h"
#include "SciantixVariableDeclaration.h"
#include "MapSciantixVariable.h"
#include "SystemDeclaration.h"
#include "MapSystem.h"
#include "GasDeclaration.h"
#include "MapGas.h"
#include "MatrixDeclaration.h"
#include "MapMatrix.h"
#include "MapPhysicsVariable.h"
#include "PhysicsVariableDeclaration.h"

/**
 * @brief IntraGranularBubbleEvolution builds an object Model according to the input_variable "iIntraGranularBubbleEvolution".
 * The models available in this routine determine the calculation of local bubble density and average size.
 */
void IntraGranularBubbleEvolution();


#endif // INTRA_GRANULAR_BUBBLE_EVOLUTION_H