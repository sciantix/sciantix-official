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
 * @see [2] <a href="../pdf_link/White_2004.pdf" target="_blank">White, JNM, 325 (2004) 61-77</a>.
 */
void InterGranularBubbleEvolution();
