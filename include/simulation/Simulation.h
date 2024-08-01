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

#ifndef SIMULATION_H
#define SIMULATION_H

#include <cmath>
#include <vector>
#include "Solver.h"
#include "Model.h"
#include "Matrix.h"
#include "System.h"
#include "SciantixArray.h"
#include "SciantixVariable.h"

/**
 * \class Simulation
 * \brief Derived class that orchestrates the overall simulation process combining various models and solvers.
 *
 * The Simulation class inherits from Solver and Model classes to utilize their functionalities
 * for simulating complex interactions and processes based on the provided models.
 */
class Simulation
{
private:
	SciantixArray<SciantixVariable> sciantix_variable;
	SciantixArray<SciantixVariable> history_variable;
	SciantixArray<SciantixVariable> physics_variable;
	
	SciantixArray<Model> model;
	SciantixArray<System> sciantix_system;
	SciantixArray<Matrix> matrices;
	SciantixArray<Gas> gas;

	SciantixArray<InputVariable> input_variable;
	std::vector<double> modes_initial_conditions;
	SciantixArray<InputVariable> scaling_factors;

	Solver solver;

	Simulation* instance;



	/**
	 * \brief Default constructor for the Simulation class
	 */
	Simulation() {}


public:

	Simulation& getInstance();


	void setVariables(
		int Sciantix_options[], 
		double Sciantix_history[], 
		double Sciantix_variables[], 
		double Sciantix_scaling_factors[], 
		double Sciantix_diffusion_modes[]
	);

	void setGas();
	void setMatrix();
	void setSystem();


	void initialize(
		int Sciantix_options[], 
		double Sciantix_history[], 
		double Sciantix_variables[], 
		double Sciantix_scaling_factors[], 
		double Sciantix_diffusion_modes[]
	);

	void execute();

	/**
	 * @brief Burnup uses the solver Integrator to computes the fuel burnup from the local power density.
	 * This method is called in Sciantix.cpp, after the definition of the Burnup model.
	 */
	void Burnup();

	/**
	 * @brief EffectiveBurnup uses the solver Integrator to computes the effective burnup of the fuel, if the
	 * criteria on the temperature are required.
	 * This method is called in Sciantix.cpp after the definition of the effective burnup model.
	 */
	void EffectiveBurnup();

	/**
	 * @brief GasProduction computes the gas produced from the production rate.
	 *
	 */
	void GasProduction();

	/**
	 * @brief Simulates the decay of gas based on decay rates from the gas model.
	 */
	void GasDecay();

	/**
	 * @brief Handles the diffusion of gas within the system, considering different states like in-grain or within bubbles.
	 */
	void GasDiffusion();

	/**
	 * @brief Models grain growth based on specific model parameters affecting the system's materials.
	 */
	void GrainGrowth();

	/**
	 * @brief IntraGranularBubbleBehaviour is a method of the object Simulation.
	 * Manages the behavior of intragranular bubbles, including their formation and changes over time.
	 * This method computes concentration and average size of intragranular gas bubbles.
	 */
	void IntraGranularBubbleBehaviour();

	/**
	 * @brief Simulates intergranular bubble dynamics, including growth and interaction with vacancies.
	 */
	void InterGranularBubbleBehaviour();

	/**
	 * @brief Calculates the sweeping of grain boundaries, particularly how it affects gas concentrations.
	 * Sweeping of the intra-granular gas concentrations.
	 */
	void GrainBoundarySweeping();

	/**
	 * @brief GrainBoundaryMicroCracking is method of simulation which executes
	 * the SCIANTIX simulation for the grain-boundary micro-cracking induced by a temperature difference.
	 * This method calls the related model "Grain-boundary micro-cracking", takes the model parameters and solve the model ODEs.
	 */
	void GrainBoundaryMicroCracking();

	/**
	 * @brief Handles the venting processes at grain boundaries, potentially releasing gases.
	 */
	void GrainBoundaryVenting();

	/**
	 * @brief Simulates the formation of high burnup structures within the nuclear fuel.
	 */
	void HighBurnupStructureFormation();

	/**
	 * @brief Models the porosity changes within high burnup structures, influencing material properties.
	 */
	void HighBurnupStructurePorosity();

	/**
	 * @brief Evaluates the deviation in stoichiometry within the nuclear material and its effects.
	 */
	void StoichiometryDeviation();

	/**
	 * @brief Conducts detailed thermochemical calculations for UO2 under specific conditions.
	 */
	void UO2Thermochemistry();

	/**
	 * @brief This method returns a pointer to the array of diffusion modes corresponding to the specified gas.
	 * @param gas_name The name of the gas for which diffusion modes are required.
	 * @return A pointer to the array of diffusion modes for the specified gas, or nullptr for invalid gas names.
	 */
	double *getDiffusionModes(std::string gas_name)
	{
		if (gas_name == "Xe")
			return &modes_initial_conditions[0];
		else if (gas_name == "Kr")
			return &modes_initial_conditions[3 * 40];
		else if (gas_name == "He")
			return &modes_initial_conditions[6 * 40];
		else if (gas_name == "Xe133")
			return &modes_initial_conditions[9 * 40];

		else if (gas_name == "Kr85m")
			return &modes_initial_conditions[12 * 40];

		else if (gas_name == "Xe in HBS")
			return &modes_initial_conditions[15 * 40];

		else
		{
			std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModes." << std::endl;
			return nullptr;
		}
	}

	/**
	 * @brief Retrieves diffusion modes related to solutions for a specified gas.
	 * @param gas_name Name of the gas.
	 * @return Pointer to the array of diffusion modes for solutions, or nullptr for invalid gas names.
	 */
	double *getDiffusionModesSolution(std::string gas_name)
	{
		if (gas_name == "Xe")
			return &modes_initial_conditions[1 * 40];

		else if (gas_name == "Kr")
			return &modes_initial_conditions[4 * 40];

		else if (gas_name == "He")
			return &modes_initial_conditions[7 * 40];

		else if (gas_name == "Xe133")
			return &modes_initial_conditions[10 * 40];

		else if (gas_name == "Kr85m")
			return &modes_initial_conditions[13 * 40];

		else if (gas_name == "Xe in HBS")
			return &modes_initial_conditions[16 * 40];
		else
		{
			std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModesSolution." << std::endl;
			return nullptr;
		}
	}

	/**
	 * @brief Retrieves diffusion modes related to bubbles for a specified gas.
	 * @param gas_name Name of the gas.
	 * @return Pointer to the array of diffusion modes for bubbles, or nullptr for invalid gas names.
	 */
	double *getDiffusionModesBubbles(std::string gas_name)
	{
		if (gas_name == "Xe")
			return &modes_initial_conditions[2 * 40];

		else if (gas_name == "Kr")
			return &modes_initial_conditions[5 * 40];

		else if (gas_name == "He")
			return &modes_initial_conditions[8 * 40];

		else if (gas_name == "Xe133")
			return &modes_initial_conditions[11 * 40];

		else if (gas_name == "Kr85m")
			return &modes_initial_conditions[14 * 40];

		else
		{
			std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModesBubbles." << std::endl;
			return nullptr;
		}
	}

	/**
	 * \brief Destructor for the Simulation class
	 */
	~Simulation() {}
};

#endif
