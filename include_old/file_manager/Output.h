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
 * @brief This routine prints the output.txt file, that is the file with all the SCIANTIX code calculations.
 * The output.txt is organized in successive columns, each one starting with their header, name and unit of measure
 * of the figure of merit (e.g., Temperature (K)), and then including the temporal evolution of the figure of merit.
 * The first columns contain the input_history.txt temporal interpolation performed by InputInerpolation.
 * The other columns contain the evolution of the sciantix variables.
 * This function contains different formatting options to print the output.txt file, according to iOutput value.
 */
void Output();

#endif // OUTPUT_H
