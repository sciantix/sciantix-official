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
 * \brief Defines the available options for fuel matrices and their properties.
 *
 * This function handles the setup of different fuel matrix configurations based on
 * user input. It configures the properties for standard UO2 or high burnup structure (HBS) 
 * enhanced UO2 based on the simulation's requirements.
 */
void SetMatrix();


