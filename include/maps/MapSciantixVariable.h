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

#ifndef MAP_SCIANTIX_VARIABLE_H
#define MAP_SCIANTIX_VARIABLE_H

#include <map>
#include <string>
#include "SciantixVariableDeclaration.h"

/**
 * @brief Contains the declaration of the MapSciantixVariable function and the sv map.
 * 
 * The sv map is used to quickly access a sciantix_variable by its name, storing the index of each sciantix_variable within the sciantix_variable vector.
 * 
 * @author G. Zullo
 */

extern std::map<std::string, int> sv;

void MapSciantixVariable();

#endif // MAP_SCIANTIX_VARIABLE_H
