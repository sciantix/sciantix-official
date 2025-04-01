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
//  Year: 2025                                                                      //
//  Authors: G.Nicodemo                                                             //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef OCCALL_H
#define OCCALL_H

#include <iostream>
#include <sstream>
#include <string>
#include <cstdlib>
#include <stdexcept>

/**
 * @class OCcall
 * @brief Class to call OpenCalphad from SCIANTIX and retrieve thermodynamic equilibrium results.
 *
 * This class provides an interface to execute OpenCalphad as an external process,
 * passing temperature and stoichiometry deviation as inputs, and capturing the output.
 */

void calculateEquilibrium(double temperature, double stoichiometryDeviation);

#endif // OCCALL_H
