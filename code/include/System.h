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

#include "Matrix.h"
#include "Gas.h"
#include "InputVariableDeclaration.h"
#include "MapInputVariable.h"
#include "HistoryVariableDeclaration.h"
#include "MapHistoryVariable.h"
#include "ErrorMessages.h"
#include <cmath>

/// Class derived from Gas and Matrix to include the properties that depend on both the fission gas and the fuel matrix (e.g., Xe covolume)

class System : virtual public Gas, virtual public Matrix
{
protected:
	double yield;
	double radius_in_lattice;
	double volume_in_lattice;
	double diffusivity;
	double bubble_diffusivity;
	double henry_constant;
	double resolution_rate;
	double trapping_rate;
	double nucleation_rate;
	std::string gas_name;
	std::string matrix_name;
	double pore_nucleation_rate;
	std::vector<double> modes;
	double production_rate;
	bool restructured_matrix;

public:
	void setRestructuredMatrix(bool y)
	{
		restructured_matrix = y;
	}

	bool getRestructuredMatrix()
	{
		return restructured_matrix;
	}

	void setYield(double y)
	{
		/// Member function to set the (cumulative) yield of the fission gas (at/fiss).
		yield = y;
	}

	double getYield()
	{
		/// Member function to get the (cumulative) yield of the fission gas (at/fiss).
		return yield;
	}

	void setRadiusInLattice(double r)
	{
		/// Member function to set the radius of the fission gas atom in the matrix lattice (m).
		radius_in_lattice = r;
	}

	double getRadiusInLattice()
	{
		/// Member function to get the radius of the fission gas atom in the matrix lattice (m).
		return radius_in_lattice;
	}

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

	double getVolumeInLattice()
	{
		/// Member function to get the volume occupied by the gas in matrix
		return volume_in_lattice;
	}

	void setVolumeInLattice(double v)
	{
		/// Member function to set the volume occupied by the gas in matrix
		volume_in_lattice = v;
	}
	
	void setBubbleDiffusivity(int input_value);
	double getBubbleDiffusivity()
	{
		/// Member function to get the bubble diffusivity of the isotope in the fuel matrix
		return bubble_diffusivity;	
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

	void setHenryConstant(double h)
	{
		/// Member function to set the value of the Henry constant
		henry_constant = h;
	}

	double getHenryConstant()
	{
		/// Member function to get the value of the Henry constant
		return henry_constant;
	}

	void setResolutionRate(int input_value);
	double getResolutionRate()
	{
		/// Member function to get the value of the resolution rate of the isotope from fuel matrix nanobubbles
		return resolution_rate;
	}

	void setTrappingRate(int input_value);
	double getTrappingRate()
	{
		/// Member function to get the value of the trapping rate of the isotope in the fuel matrix nanobubbles
		return trapping_rate;
	}

	void setNucleationRate(int input_value);
	double getNucleationRate()
	{
		return nucleation_rate;
	}

	void setPoreNucleationRate(double t)
	{
		pore_nucleation_rate = t;
	}

	double getPoreNucleationRate()
	{
		return pore_nucleation_rate;
	}

	void setProductionRate(int input_value);
	double getProductionRate()
	{
    /** @fn double getProductionRate()
     * @brief Member function to get the production rate of the sciantix_system.
		 *  
     * @param production_rate: output
     */		
		return production_rate;
	}

	System() { }
	~System() { }
};

#endif // SYSTEM_H
