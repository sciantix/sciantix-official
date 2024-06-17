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
//  Authors: D. Pizzocri, T. Barani                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "ErrorMessages.h"
#include "MainVariables.h"
#include <string>
#include <sstream>
#include <vector>
#include <numeric>


/**
 * \brief Handles all input processing for the simulation.
 * It opens necessary input files, reads configuration and initial condition data, 
 * logs this data for verification, and manages any missing file errors.
 * 
 * @param testFilePath The base path to the directory containing input files.
 */
void InputReading(std::string testFilePath);
