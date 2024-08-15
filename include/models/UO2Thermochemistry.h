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

#ifndef UO2THERMOCHEMISTRY_H
#define UO2THERMOCHEMISTRY_H

#include <cmath>

/**
 * @brief The oxygen partial pressure in UO2+x fuel as a function of x, i.e., PO2 (x) (in atm) is calculated from Blackburn’s relation
 * @ref Blackburn (1973) J. Nucl. Mater., 46, 244–252.
 * 
 * Validity range:
 * - T: 1000 K - 2670 K
 * - x: 0 - 0.25
 * 
 * @author G. Zullo
 * @author G. Petrosillo
 */
double BlackburnThermochemicalModel(double stoichiometry_deviation, double temperature);

#endif
