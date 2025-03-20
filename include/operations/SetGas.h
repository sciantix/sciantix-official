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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SET_GAS_H
#define SET_GAS_H

#include <vector>
#include "Gas.h"
#include "SciantixArray.h"

/**
 * @brief Sets up the gas properties in the simulation.
 * 
 * This function initializes and maps properties for Xenon, Krypton, and Helium gases
 * used in the simulation. Each gas is set up with specific attributes and then mapped
 * for easy access throughout the simulation.
 * 
 * @author G. Zullo
 * @author F. Bastien
 */

void xenon(SciantixArray<Gas> &gas);
void krypton(SciantixArray<Gas> &gas);
void helium(SciantixArray<Gas> &gas);
void caesium(SciantixArray<Gas> &gas);
void iodine(SciantixArray<Gas> &gas);
void tellurium(SciantixArray<Gas> &gas);

#endif // SET_GAS_H