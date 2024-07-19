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


#ifndef INTER_GRANULAR_BUBBLE_EVOLUTION_H
#define INTER_GRANULAR_BUBBLE_EVOLUTION_H


#include "SciantixVariableDeclaration.h"
#include "MapSciantixVariable.h"
#include "ModelDeclaration.h"
#include "MapModel.h"
#include "MatrixDeclaration.h"
#include "MapMatrix.h"
#include "SystemDeclaration.h"
#include "MapSystem.h"
#include "GasDeclaration.h"
#include "MapGas.h"
#include <cmath>
#include "ConstantNumbers.h"

/**
 * @brief This function contains a choice among possible expressions for the bubble number density and
 * the bubble radius at grain boundaries. The model considers one-off nucleation,
 * growth of lenticular bubbles by vacancy absorption and coalescence of bubbles.
 * @see [2] <a href="../../references/pdf_link/White_2004.pdf" target="_blank">White, JNM, 325 (2004) 61-77</a>,
<a href="../../references/pdf_link/Pastore_et_al_2013.pdf" target="_blank">Pastore et al., NED, 256 (2013) 75-86</a>.
 */
void InterGranularBubbleEvolution();

#endif // INTER_GRANULAR_BUBBLE_EVOLUTION_H