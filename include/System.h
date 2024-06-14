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

#ifndef SYSTEM_H
#define SYSTEM_H

#include "Gas.h"
#include "InputVariableDeclaration.h"
#include "MapInputVariable.h"
#include "HistoryVariableDeclaration.h"
#include "MapHistoryVariable.h"
#include "ErrorMessages.h"
#include <cmath>

/// Class derived from Gas and Matrix to include the properties that depend on both the fission gas and the fuel matrix (e.g., Xe covolume)

class System : virtual public Gas
{
protected:
	std::string gas_name;
	//std::vector<double> modes;
	//double production_rate;

public:

	void setGasName(std::string n)
	{
		/// Member function to set the name of the gas in the matrix
		gas_name = n;
	}

	std::string getGasName()
	{
		/// Member function to get the name of the gas in the matrix
		return gas_name;
	}

	// void setProductionRate(int input_value);
	// double getProductionRate()
	// {
    // /** @fn double getProductionRate()
    //  * @brief Member function to get the production rate of the sciantix_system.
	// 	 *  
    //  * @param production_rate: output
    //  */		
	// 	return production_rate;
	// }

	System() { }
	~System() { }
};

#endif // SYSTEM_H
