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
#include <unordered_map>

/**
 * \class System
 * \brief Class derived from Gas and Matrix to include the properties that depend on both the fission gas and the fuel matrix (e.g., Xe covolume)
 */
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
	std::unordered_map<std::string, std::pair<double, double>> bounds;
	std::unordered_map<std::string, bool> bounded;

public:
	/**
	 * @brief Sets the matrix restructuring status.
	 * @param y True if the matrix is restructured, false otherwise.
	 */
	void setRestructuredMatrix(bool y)
	{
		restructured_matrix = y;
	}

	/**
	 * @brief Gets the restructuring status of the matrix.
	 * @return True if the matrix is restructured, false otherwise.
	 */
	bool getRestructuredMatrix()
	{
		return restructured_matrix;
	}

	/**
	 * @brief Sets the cumulative yield of the fission gas.
	 * @param y Yield to set (atoms per fission).
	 */
	void setYield(double y)
	{
		/// Member function to set the (cumulative) yield of the fission gas (at/fiss).
		yield = y;
	}

	/**
	 * @brief Gets the cumulative yield of the fission gas.
	 * @return Yield of the gas (atoms per fission).
	 */
	double getYield()
	{
		/// Member function to get the (cumulative) yield of the fission gas (at/fiss).
		return yield;
	}

	/**
	 * @brief Sets the radius of the fission gas atom in the matrix lattice.
	 * @param r Radius to set (meters).
	 */
	void setRadiusInLattice(double r)
	{
		/// Member function to set the radius of the fission gas atom in the matrix lattice (m).
		radius_in_lattice = r;
	}

	/**
	 * @brief Gets the radius of the fission gas atom in the matrix lattice.
	 * @return Radius of the gas atom (meters).
	 */
	double getRadiusInLattice()
	{
		/// Member function to get the radius of the fission gas atom in the matrix lattice (m).
		return radius_in_lattice;
	}

	/**
	 * @brief Sets the name of the gas.
	 * @param n Name of the gas to set.
	 */
	void setGasName(std::string n)
	{
		/// Member function to set the name of the gas in the matrix
		gas_name = n;
	}

	/**
	 * @brief Gets the name of the gas.
	 * @return Name of the gas.
	 */
	std::string getGasName()
	{
		/// Member function to get the name of the gas in the matrix
		return gas_name;
	}

	/**
	 * @brief Sets the name of the matrix.
	 * @param n Name of the matrix to set.
	 */
	void setMatrixName(std::string n)
	{
		/// Member function to set the name of the matrix
		matrix_name = n;
	}

	/**
	 * @brief Gets the name of the matrix.
	 * @return Name of the matrix.
	 */
	std::string getMatrixName()
	{
		/// Member function to get the name of the matrix
		return matrix_name;
	}

	/**
	 * @brief Gets the volume occupied by the gas in the matrix.
	 * @return Volume occupied by the gas.
	 */
	double getVolumeInLattice()
	{
		/// Member function to get the volume occupied by the gas in matrix
		return volume_in_lattice;
	}

	/**
	 * @brief Sets the volume occupied by the gas in the matrix.
	 * @param v Volume to set.
	 */
	void setVolumeInLattice(double v)
	{
		/// Member function to set the volume occupied by the gas in matrix
		volume_in_lattice = v;
	}

	/**
	 * @brief Sets the diffusivity of bubbles within the matrix based on input values.
	 * @param input_value The model selection index for bubble diffusivity.
	 */
	void setBubbleDiffusivity(int input_value);

	/**
	 * @brief Retrieves the diffusivity of bubbles within the matrix.
	 * @return The current bubble diffusivity.
	 */
	double getBubbleDiffusivity()
	{
		/// Member function to get the bubble diffusivity of the isotope in the fuel matrix
		return bubble_diffusivity;
	}

	/**
	 * @brief Sets the helium diffusivity within the matrix based on the selected model.
	 * The intra-granular helium diffusivity within the fuel grain is set according to the input_variable iHeDiffusivity
	 * @param input_value The model selection index for helium diffusivity.
	 */
	void setHeliumDiffusivity(int input_value);

	/**
	 * @brief Retrieves the helium diffusivity within the matrix.
	 * @return The current helium diffusivity.
	 */
	double getHeliumDiffusivity()
	{
		/// Member function to get the bubble diffusivity of the isotope in the fuel matrix
		return diffusivity;
	}

	/**
	 * @brief Sets the diffusivity of fission gases within the matrix based on the selected model.
	 * The intra-granular fission gas (xenon and krypton) diffusivity within the fuel grain is set according to the input_variable iFGDiffusionCoefficient
	 * @param input_value The model selection index for fission gas diffusivity.
	 */
	void setFissionGasDiffusivity(int input_value);

	/**
	 * @brief Retrieves the diffusivity of fission gases within the matrix.
	 * @return The current fission gas diffusivity.
	 */
	double getFissionGasDiffusivity()
	{
		/// Member function to get the diffusivity of the isotope in the fuel matrix
		return diffusivity;
	}

	/**
	 * @brief Sets the Henry's constant value.
	 * @param h The new value for Henry's constant.
	 */
	void setHenryConstant(double h)
	{
		/// Member function to set the value of the Henry constant
		henry_constant = h;
	}

	/**
	 * @brief Retrieves the Henry's constant value.
	 * @return The current value of Henry's constant.
	 */
	double getHenryConstant()
	{
		/// Member function to get the value of the Henry constant
		return henry_constant;
	}

	/**
	 * @brief Sets the resolution rate for isotopes from nanobubbles in the matrix.
	 * The helium intra-granular resolution rate is set according to the input_variable iResolutionRate.
	 * @param input_value The model selection index for setting the resolution rate.
	 */
	void setResolutionRate(int input_value);

	/**
	 * @brief Retrieves the resolution rate for isotopes from nanobubbles in the matrix.
	 * @return The current resolution rate.
	 */
	double getResolutionRate()
	{
		/// Member function to get the value of the resolution rate of the isotope from fuel matrix nanobubbles
		return resolution_rate;
	}

	/**
	 * @brief Sets the trapping rate for isotopes in nanobubbles within the matrix.
	 * The krypton intra-granular trapping rate is set according to the input_variable iTrappingRate.
	 * @param input_value The model selection index for setting the trapping rate.
	 */
	void setTrappingRate(int input_value);

	/**
	 * @brief Retrieves the trapping rate for isotopes in nanobubbles within the matrix.
	 * @return The current trapping rate.
	 */
	double getTrappingRate()
	{
		/// Member function to get the value of the trapping rate of the isotope in the fuel matrix nanobubbles
		return trapping_rate;
	}

	/**
	 * @brief Sets the nucleation rate based on the selected model.
	 * Evaluation of the nucleation rate of intragranular gas bubble inside the UO<sub>2</sub> matrix
	 * @param input_value The model selection index for nucleation rate.
	 */
	void setNucleationRate(int input_value);

	/**
	 * @brief Retrieves the nucleation rate.
	 * @return The current nucleation rate.
	 */
	double getNucleationRate()
	{
		return nucleation_rate;
	}

	/**
	 * @brief Sets the pore nucleation rate.
	 * @param t The new pore nucleation rate.
	 */
	void setPoreNucleationRate(double t)
	{
		pore_nucleation_rate = t;
	}

	/**
	 * @brief Retrieves the pore nucleation rate.
	 * @return The current pore nucleation rate.
	 */
	double getPoreNucleationRate()
	{
		return pore_nucleation_rate;
	}

	/**
	 * @brief Sets the production rate based on the selected model.
	 * @param input_value The model selection index for setting the production rate.
	 */
	void setProductionRate(int input_value);

	/**
	 * @brief  Member function to get the production rate of the sciantix_system.
	 * @return The current production rate.
	 */
	double getProductionRate()
	{
		return production_rate;
	}

	/**
	 * \brief Sets the bounds for the variable
	 * \param minBound The minimum bound
	 * \param maxBound The maximum bound
	 * \param var The variable to set
	 */
	void setBounds(const std::string &var, double minBound, double maxBound)
	{
		bounds[var] = {minBound, maxBound};
	}

	/**
	 * \brief Sets whether to check the bounds
	 * \param check A boolean value indicating whether to check the bounds
	 * \param var The variable to set
	 */
	void setBounded(const std::string &var, bool check)
	{
		bounded[var] = check;
	}

	void isWithinBounds(const std::string &var)
	{
		auto itBounds = bounds.find(var);
		auto itBounded = bounded.find(var);
		double excess;

		if (itBounds != bounds.end() && itBounded != bounded.end() && itBounded->second)
		{
			double minBound = itBounds->second.first;
			double maxBound = itBounds->second.second;
			double value;

			if (var == "radius_in_lattice")
				value = radius_in_lattice;
			else if (var == "volume_in_lattice")
				value = volume_in_lattice;
			else if (var == "diffusivity")
				value = diffusivity;
			else if (var == "bubble_diffusivity")
				value = bubble_diffusivity;
			else if (var == "resolution_rate")
				value = resolution_rate;
			else if (var == "trapping_rate")
				value = trapping_rate;
			else if (var == "nucleation_rate")
				value = nucleation_rate;
			else if (var == "pore_nucleation_rate")
				value = pore_nucleation_rate;
			else if (var == "production_rate")
				value = production_rate;
			else return;

			if (value < minBound)
			{
				excess = minBound - value;
				ErrorMessages::errorBounds(var, value, excess);
			}
			else if (value > maxBound)
			{
				excess = value - maxBound;
				ErrorMessages::errorBounds(var, value, excess);
			}
		}
	}

	/**
	 * @brief Default constructor for the System class.
	 */
	System() {}
	/**
	 * @brief Destructor for the System class
	 */
	~System() {}
};

#endif // SYSTEM_H
