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
//  Contributors: F. Bastien                                                        //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SET_GAS_H
#define SET_GAS_H

#include <vector>
#include "Gas.h"
#include "SciantixArray.h"

/**
 * \brief Sets up the gas properties in the simulation.
 * 
 * This function initializes and maps properties for Xenon, Krypton, and Helium gases
 * used in the simulation. Each gas is set up with specific attributes and then mapped
 * for easy access throughout the simulation.
 */
// void setGas(SciantixArray<Gas> &gas);

void xenon(SciantixArray<Gas> &gas);
void krypton(SciantixArray<Gas> &gas);
void helium(SciantixArray<Gas> &gas);

#endif // SET_GAS_H