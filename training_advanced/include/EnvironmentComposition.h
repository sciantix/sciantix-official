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

#ifndef ENVIRONMENT_COMPOSITION_H
#define ENVIRONMENT_COMPOSITION_H

#include "SciantixVariableDeclaration.h"
#include "HistoryVariableDeclaration.h"
#include "ModelDeclaration.h"
#include "MapSciantixVariable.h"
#include "MapHistoryVariable.h"

void EnvironmentComposition();
/**
 * @brief Evaluates the oxygen partial pressure in an external environment.
 * 
 * This function calculates the oxygen partial pressure in an environment external to the fuel,
 * assuming the presence of pure steam without cladding or full oxidation.
 * This estimation is used to determine the UO2 equilibrium oxygen partial pressure and stoichiometry,
 * in connection with macroscopic models for UO2+x.
 * 
 * @ref Lewis et al. JNM 227 (1995) 83-109
 * 
 * @author
 * G. Petrosillo
 * G. Zullo
 * 
 */

#endif // ENVIRONMENT_COMPOSITION_H
