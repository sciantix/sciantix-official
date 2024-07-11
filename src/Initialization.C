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

/// Initialization
/// This routine initializes Sciantix internal variables with
/// initial conditions/interface variables

#include "Initialization.h"

void Initialization()
{
	const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

	// Sciantix_history initialization
	Sciantix_history[0] = Temperature_input[0];
	Sciantix_history[1] = Temperature_input[0];

	Sciantix_history[2] = Xe_Release_rate_fuel_input[0];
	Sciantix_history[3] = Xe_Release_rate_fuel_input[0];

	Sciantix_history[4] = Xe133_Release_rate_fuel_input[0];
	Sciantix_history[5] = Xe133_Release_rate_fuel_input[0];

	Sciantix_history[6] = Kr85m_Release_rate_fuel_input[0];
	Sciantix_history[7] = Kr85m_Release_rate_fuel_input[0];
	
	Sciantix_history[8] = Time_input[0];
	Sciantix_history[9] = Time_step_number;

	Sciantix_history[11] = Defect_Time_input[0];

}
