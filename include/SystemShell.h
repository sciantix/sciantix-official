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

#ifndef SYSTEMSHELL_H
#define SYSTEMSHELL_H

#include "Shell.h"
#include "Gas.h"
#include "InputVariableDeclaration.h"
#include "MapInputVariable.h"
#include "HistoryVariableDeclaration.h"
#include "MapHistoryVariable.h"
#include "ErrorMessages.h"
#include <cmath>

/// Class derived from Gas and Shell to include the properties that depend on both the fission gas and the fuel matrix (e.g., Xe covolume)

class SystemShell : virtual public Gas, virtual public Shell
{
protected:
	double diffusivity;
	std::string gas_name;
	std::string matrix_name;
	std::vector<double> modes;

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

	void setMatrixName(std::string n)
	{
		/// Member function to set the name of the matrix
		matrix_name = n;
	}

	std::string getMatrixName()
	{
		/// Member function to get the name of the matrix
		return matrix_name;
	}

	void setHeliumDiffusivity(int input_value);
	double getHeliumDiffusivity()
	{
		/// Member function to get the bubble diffusivity of the isotope in the fuel matrix
		return diffusivity;	
	}

	void setFissionGasDiffusivity(int input_value);	
	double getFissionGasDiffusivity()
	{
		/// Member function to get the diffusivity of the isotope in the fuel matrix
		return diffusivity;
	}

	void setModes(std::vector<double> eigenvalues){

		modes = eigenvalues;

	}

	std::vector<double> getModes(){

		return modes;

	}

	SystemShell() { }
	~SystemShell() { }
};

#endif // SYSTEMSHELL_H