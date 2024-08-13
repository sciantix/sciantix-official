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

#include "Simulation.h"

/**
 * @brief GapPartialPressure() Calculates the oxygen partial pressure in the gap based on a thermo-chemical equilibrium model.
 * 
 * @authors 
 * Giacomo Petrosillo
 * Giovanni Zullo
 *
 * @details
 * The equilibrium constant is calculated using the law of mass action for water vapor decomposition.
 * The gap oxygen partial pressure is then calculated using this equilibrium constant and the steam pressure.
 * The final value is set in the `Gap oxygen partial pressure` variable.
 * 
 * @ref Morel et al., CEA, Report NT/DTP/SECC no. DR94-55 (1994)
 * @ref Lewis et al. JNM 227 (1995) 83-109, D.R. Olander, Nucl. Technol. 74 (1986) 215.
 */

void Simulation::GapPartialPressure()
{
    if (!input_variable["iStoichiometryDeviation"].getValue()) return;

    Model model_;
    model_.setName("Gap partial pressure");

    double equilibrium_constant = exp(-25300.0 / history_variable["Temperature"].getFinalValue() + 4.64 + 1.04 * (0.0007 * history_variable["Temperature"].getFinalValue() - 0.2));

    double steam_pressure = history_variable["Steam pressure"].getFinalValue();
    double gap_oxygen_partial_pressure = pow(pow(equilibrium_constant, 2) * pow(steam_pressure, 2) / 4, 1.0 / 3.0);

    sciantix_variable["Gap oxygen partial pressure"].setFinalValue(gap_oxygen_partial_pressure); // (atm)

    std::string reference = "Lewis et al. JNM 227 (1995) 83-109, D.R. Olander, Nucl. Technol. 74 (1986) 215.";
    model_.setRef(reference);
}
