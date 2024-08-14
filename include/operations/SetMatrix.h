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

#ifndef SET_MATRIX_H
#define SET_MATRIX_H

#include "MatrixDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "MapSciantixVariable.h"
#include "MapMatrix.h"
#include "SetVariables.h"
#include "UO2.h"
#include "UO2HBS.h"
#include <cmath>
#include <string>
#include "GasDeclaration.h"
#include "MapGas.h"

/**
 * @brief Sets up the matrix properties in the simulation.
 * 
 * This function initializes and maps properties for matrices
 * used in the simulation. Each matrix is set up with specific attributes and then mapped
 * for easy access throughout the simulation.
 * 
 * @author G. Zullo
 * @author F. Bastien
 * 
 */

void SetMatrix();



#endif // SET_MATRIX_H
