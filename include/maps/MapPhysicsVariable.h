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

#ifndef MAP_PHYSICS_VARIABLE_H
#define MAP_PHYSICS_VARIABLE_H

#include <map>
#include <string>
#include "PhysicsVariableDeclaration.h"

/**
 * @brief Contains the declaration of the MapHistoryVariable function and the pv map.
 * 
 * The pv map is used to quickly access a physics_variable by its name, storing the index of each physics_variable within the physics_variable vector.
 * 
 * @author G. Zullo
 */

extern std::map<std::string, int> pv;

void MapPhysicsVariable();

#endif // MAP_PHYSICS_VARIABLE_H

