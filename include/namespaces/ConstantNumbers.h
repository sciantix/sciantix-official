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

#ifndef CONSTANT_NUMBERS_H
#define CONSTANT_NUMBERS_H

/**
 * @file ConstantNumbers.h
 * @brief Defines mathematical and physical constants used throughout the simulation.
 *
 * This header file contains namespaces that define frequently used mathematical and 
 * physical constants, ensuring consistent usage across the entire codebase.
 * 
 * @author G. Zullo
 */

/**
 * @namespace MathConstants
 * @brief Namespace containing fundamental mathematical constants.
 */
namespace MathConstants
{
	/**
	 * @brief The value of pi (π).
	 */
	const double pi = 3.141592653589793;
}

/**
 * @namespace PhysicsConstants
 * @brief Namespace containing fundamental physical constants.
 */
namespace PhysicsConstants
{
	/**
	 * @brief The Boltzmann constant, measured in joules per kelvin (J/K).
	 */
	const double boltzmann_constant = 1.380651e-23;
	
	/**
	 * @brief Avogadro's number, measured in atoms per mole (at/mol).
	 */
	const double avogadro_number = 6.02214076e23; 
}

#endif // CONSTANT_NUMBERS_H
