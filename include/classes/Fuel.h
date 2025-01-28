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

#ifndef FUEL_H
#define FUEL_H

#include <vector>
#include <string>
#include <iterator>
#include <map>
#include <string>

#include "Material.h"
#include "Constants.h"
#include "ErrorMessages.h"
#include "Matrix.h"
#include "SciantixArray.h"
#include "SciantixVariable.h"

/**
 * @class Fuel
 * @brief Represents the whole fuel material, such as UO2, MOX, or even composite materials (TRISO), derived from the Matrix class.
 *
 * This class extends the Matrix class to incorporate material properties and homogenization rules for composite materials.
 * 
 * @author G. Nicodemo
 */

class Fuel : public Matrix
{
protected:
	double volumetric_fraction_one;
	double volumetric_fraction_two;
	double homogenized_elastic_modulus;

public:

	void setVolumetricFractionOne(double v)
	{
		/// Member function to set the volumetric fraction of the first fuel phase
		volumetric_fraction_one = v;
	}

	double getVolumetricFractionOne()
	{
		/// Member function to get the volumetric fraction of the first fuel phase
		return volumetric_fraction_one;
	}

	void setVolumetricFractionTwo(double v)
	{
		/// Member function to set the volumetric fraction of the second fuel phase
		volumetric_fraction_two = v;
	}

	double getVolumetricFractionTwo()
	{
		/// Member function to get the volumetric fraction of the second fuel phase
		return volumetric_fraction_two;
	}

	void setHomogenizedElasticModulus(int input_value);

	double getHomogenizedElasticModulus()
	{
		/// Member function to get the volumetric fraction of the second fuel phase
		return homogenized_elastic_modulus;
	}

};

#endif // FUEL_H