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

#ifndef OUTPUT_H
#define OUTPUT_H

#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
#include <limits>
#include <map>
#include <sys/stat.h>
#include <string>
#include "MapSciantixVariable.h"
#include "Variable.h"
#include "ModelDeclaration.h"
#include "SystemDeclaration.h"
#include "HistoryVariableDeclaration.h"
#include "InputVariableDeclaration.h"
#include "MapHistoryVariable.h"
#include "MapInputVariable.h"
#include "MatrixDeclaration.h"

/**
 * @brief This function save the simulation results on output files.
 * It creates the file output.txt, with the calculations
 * and the overview.txt, with SCIANTIX model information
 * 
 * @author G. Zullo
 */

void Output();

#endif // OUTPUT_H
