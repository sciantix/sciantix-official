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


#ifndef STOICHIOMETRY_DEVIATION_H
#define STOICHIOMETRY_DEVIATION_H


#include "ModelDeclaration.h"
#include "InputVariableDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "MapSciantixVariable.h"
#include "HistoryVariableDeclaration.h"
#include "MapHistoryVariable.h"
#include "ConstantNumbers.h"

/**
 * @brief This routine sets the model to estimate the stoichiometry deviation of the fuel.
 * Currently, UO2+x is considered, under external oxidizing environment.
 *
 * @author
 * G. Petrosillo
 * G. Zullo
 *
 */
void StoichiometryDeviation( );


#endif // STOICHIOMETRY_DEVIATION_H