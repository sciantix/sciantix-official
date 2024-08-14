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

#include "SciantixVariableDeclaration.h"
#include "HistoryVariableDeclaration.h"
#include "ModelDeclaration.h"
#include "MapSciantixVariable.h"
#include "MapHistoryVariable.h"

void UO2Thermochemistry();
/**
 * @brief This routine defines the model to evaluate the oxygen partial pressure (in atm) in hyperstoichiometric UO2+x fuel
 * as a function of:
 * 
 * @param[in] stoichiometry_deviation
 * @param[in] temperature
 * 
 * @param[out] PO2_x oxygen partial pressure in UO2+x (in atm) 
 * from Blackburn’s relation, @ref *Blackburn (1973) J. Nucl. Mater., 46, 244–252*
 * 
 * @author
 * G. Petrosillo
 * G. Zullo
 * 
 */

double BlackburnThermochemicalModel(double stoichiometry_deviation, double temperature);
/**
 * @brief The oxygen partial pressure in UO2+x fuel as a function of x, i.e., PO2 (x) (in atm) is calculated from Blackburn’s relation
 * @ref Blackburn (1973) J. Nucl. Mater., 46, 244–252.
 * 
 * Validity range:
 * - T: 1000 K - 2670 K
 * - x: 0 - 0.25
 */
