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

/// This namespace contains relevant numeric constants that can be used in all the code.

#ifndef CONSTANT_NUMBERS_H
#define CONSTANT_NUMBERS_H

namespace MathConstants
{
	const double pi = 3.141592653589793;
}

namespace PhysicsConstants
{
	const double boltzmann_constant = 1.380651e-23; // J/K
	const double avogadro_number = 6.02214076e23; // at/mol
	const double molar_mass_Oxigen = 15.999; // g/mol
	const double molar_mass_Chromium = 51.9961; // g/mol
	const double calorie = 4.186; // J
	const double gas_constant = 8.3143; // J/(mol K)
}

#endif