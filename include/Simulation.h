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
#include "MapModel.h"
#include "MapSciantixVariable.h"
#include "MapPhysicsVariable.h"
#include "HistoryVariableDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "ModelDeclaration.h"
#include "SolverDeclaration.h"
#include "PhysicsVariableDeclaration.h"
#include "GasDeclaration.h"
#include "MapGas.h"
#include "HighBurnupStructureFormation.h"
#include "HighBurnupStructurePorosity.h"
#include "SciantixDiffusionModeDeclaration.h"
#include "SystemDeclaration.h"
#include "MapSystem.h"
#include "MatrixDeclaration.h"
#include "MapMatrix.h"
#include "ConstantNumbers.h"
#include "UO2Thermochemistry.h"

/// @brief
/// Derived class representing the operations of SCIANTIX. The conjunction of the models with the implemented solvers results in the simulation.

class Simulation : public Solver, public Model
{
	public:

	void Burnup()
	{
		/// @brief Burnup uses the solver Integrator to computes the fuel burnup from the local power density.
		/// This method is called in Sciantix.cpp, after the definition of the Burnup model.
		
		sciantix_variable[sv["Burnup"]].setFinalValue(
			solver.Integrator(
				sciantix_variable[sv["Burnup"]].getInitialValue(),
				model[sm["Burnup"]].getParameter().at(0),
				physics_variable[pv["Time step"]].getFinalValue()
			)
		);

		if(history_variable[hv["Fission rate"]].getFinalValue() > 0.0)
			sciantix_variable[sv["Irradiation time"]].setFinalValue(
				solver.Integrator(
					sciantix_variable[sv["Irradiation time"]].getInitialValue(),
					1.0 / sciantix_variable[sv["Specific power"]].getFinalValue(),
					24.0 * sciantix_variable[sv["Burnup"]].getIncrement()
				)
			);
		else
			sciantix_variable[sv["Irradiation time"]].setConstant();
			
		sciantix_variable[sv["FIMA"]].setFinalValue(
			history_variable[hv["Fission rate"]].getFinalValue() * history_variable[hv["Irradiation time"]].getFinalValue() * 3.6e5 / 
			sciantix_variable[sv["U"]].getFinalValue()
		);
	}

	void EffectiveBurnup()
	{
		/// @brief EffectiveBurnup uses the solver Integrator to computes the effective burnup of the fuel, if the
		/// criteria on the temperature are required.
		/// This method is called in Sciantix.cpp after the definition of the effective burnup model.
		sciantix_variable[sv["Effective burnup"]].setFinalValue(
			solver.Integrator(
				sciantix_variable[sv["Effective burnup"]].getInitialValue(),
				model[sm["Effective burnup"]].getParameter().at(0),
				physics_variable[pv["Time step"]].getFinalValue()
			)
		);
	}

	void GasProduction()
	{
		/**
		 * @brief GasProduction computes the gas produced from the production rate.
		 *
		 */

		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			sciantix_variable[sv[sciantix_system[i].getGasName() + " produced"]].setFinalValue(
				solver.Integrator(
					sciantix_variable[sv[sciantix_system[i].getGasName() + " produced"]].getInitialValue(),
					model[sm["Gas production - " + sciantix_system[i].getGasName() + " in " + matrix[0].getName()]].getParameter().at(0),
					model[sm["Gas production - " + sciantix_system[i].getGasName() + " in " + matrix[0].getName()]].getParameter().at(1)
				)
			);
		}
	}

	void GasDecay()
	{
		/// @brief
		/// Gas Decay
		// This for loop slides over all the considered gases (both stable and radioactive ones)
		// but computes the decayed concentration only of radioactive isotopes
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			if (gas[ga[sciantix_system[i].getGasName()]].getDecayRate() > 0.0)
			{
				sciantix_variable[sv[sciantix_system[i].getGasName() + " decayed"]].setFinalValue(
					solver.Decay(
						sciantix_variable[sv[sciantix_system[i].getGasName() + " decayed"]].getInitialValue(),
						gas[ga[sciantix_system[i].getGasName()]].getDecayRate(),
						gas[ga[sciantix_system[i].getGasName()]].getDecayRate() * sciantix_variable[sv[sciantix_system[i].getGasName() + " produced"]].getFinalValue(),
						physics_variable[pv["Time step"]].getFinalValue()
					)
				);
			}
		}
	}

	void GasDiffusion()
	{
		/// @brief
		/// GasDiffusion
		/// This simulation method solves the PDE for the intra-granular gas diffusion within the (ideal) spherical fuel grain.		
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			switch (int(input_variable[iv["iDiffusionSolver"]].getValue()))
			{
				case 1:
				{
					sciantix_variable[sv[sciantix_system[i].getGasName() + " in grain"]].setFinalValue(
						solver.SpectralDiffusion(
							getDiffusionModes(sciantix_system[i].getGasName()),
							model[sm["Gas diffusion - " + sciantix_system[i].getName()]].getParameter(),
							physics_variable[pv["Time step"]].getFinalValue()
						)
					);

					double equilibrium_fraction(1.0);
					if ((sciantix_system[i].getResolutionRate() + sciantix_system[i].getTrappingRate()) > 0.0)
						equilibrium_fraction = sciantix_system[i].getResolutionRate() / (sciantix_system[i].getResolutionRate() + sciantix_system[i].getTrappingRate());

					sciantix_variable[sv[sciantix_system[i].getGasName() + " in intragranular solution"]].setFinalValue(
						equilibrium_fraction * sciantix_variable[sv[sciantix_system[i].getGasName() + " in grain"]].getFinalValue());

					sciantix_variable[sv[sciantix_system[i].getGasName() + " in intragranular bubbles"]].setFinalValue(
						(1.0 - equilibrium_fraction) * sciantix_variable[sv[sciantix_system[i].getGasName() + " in grain"]].getFinalValue());

					break;
				}

				case 2:
				{
					double initial_value_solution = sciantix_variable[sv[sciantix_system[i].getGasName() + " in intragranular solution"]].getFinalValue();
					double initial_value_bubbles  = sciantix_variable[sv[sciantix_system[i].getGasName() + " in intragranular bubbles"]].getFinalValue();

					solver.SpectralDiffusionNonEquilibrium(
						initial_value_solution,
						initial_value_bubbles,
						getDiffusionModesSolution(sciantix_system[i].getGasName()),
						getDiffusionModesBubbles(sciantix_system[i].getGasName()),
						model[sm["Gas diffusion - " + sciantix_system[i].getName()]].getParameter(),
						physics_variable[pv["Time step"]].getFinalValue()
					);

					sciantix_variable[sv[sciantix_system[i].getGasName() + " in intragranular solution"]].setFinalValue(initial_value_solution);
					sciantix_variable[sv[sciantix_system[i].getGasName() + " in intragranular bubbles"]].setFinalValue(initial_value_bubbles);
					sciantix_variable[sv[sciantix_system[i].getGasName() + " in grain"]].setFinalValue(initial_value_solution + initial_value_bubbles);
					
					break;
				}

				default:
					ErrorMessages::Switch("Simulation.h", "iDiffusionSolver", int(input_variable[iv["iDiffusionSolver"]].getValue()));
					break;
			}
		}
	
		// Calculation of the gas concentration arrived at the grain boundary, by mass balance.
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].setFinalValue(
				sciantix_variable[sv[sciantix_system[i].getGasName() + " produced"]].getFinalValue() -
				sciantix_variable[sv[sciantix_system[i].getGasName() + " decayed"]].getFinalValue() -
				sciantix_variable[sv[sciantix_system[i].getGasName() + " in grain"]].getFinalValue() -
				sciantix_variable[sv[sciantix_system[i].getGasName() + " released"]].getInitialValue()
			);

			if (sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
				sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].setFinalValue(0.0);
		}

		/**
		 * @brief If **iGrainBoundaryBehaviour = 0** (e.g., grain-boundary calculations are neglected), 
		 * all the gas arriving at the grain boundary is released.
		 * 
		 */
		if (input_variable[iv["iGrainBoundaryBehaviour"]].getValue() == 0)
		{
			for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
			{
				sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].setInitialValue(0.0);
				sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].setFinalValue(0.0);

				sciantix_variable[sv[sciantix_system[i].getGasName() + " released"]].setFinalValue(
					sciantix_variable[sv[sciantix_system[i].getGasName() + " produced"]].getFinalValue() -
					sciantix_variable[sv[sciantix_system[i].getGasName() + " decayed"]].getFinalValue() -
					sciantix_variable[sv[sciantix_system[i].getGasName() + " in grain"]].getFinalValue()
				);
			}
		}
	}

	void GrainGrowth()
	{
		/**
		 * @brief ### GrainGrowth
		 * 
		 */
		sciantix_variable[sv["Grain radius"]].setFinalValue(
			solver.QuarticEquation(model[sm["Grain growth"]].getParameter())
		);
	}

	void IntraGranularBubbleBehaviour()
	{
		/**
		 * @brief IntraGranularBubbleBehaviour is a method of the object Simulation.
		 * This method computes concentration and average size of intragranular gas bubbles.
		 *  
		 */

		// dN / dt = - getParameter().at(0) * N + getParameter().at(1)
		sciantix_variable[sv["Intragranular bubble concentration"]].setFinalValue(
			solver.Decay(
				sciantix_variable[sv["Intragranular bubble concentration"]].getInitialValue(),
				model[sm["Intragranular bubble evolution"]].getParameter().at(0),
				model[sm["Intragranular bubble evolution"]].getParameter().at(1),
				physics_variable[pv["Time step"]].getFinalValue()
			)
		);

		// Atom per bubbles and bubble radius
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			if (gas[ga[sciantix_system[i].getGasName()]].getDecayRate() == 0.0)
			{
				if (sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue() > 0.0)
					sciantix_variable[sv["Intragranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].setFinalValue(
						sciantix_variable[sv[sciantix_system[i].getGasName() + " in intragranular bubbles"]].getFinalValue() /
						sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue()
					);

				else
					sciantix_variable[sv["Intragranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].setFinalValue(0.0);

				sciantix_variable[sv["Intragranular bubble volume"]].addValue(
					sciantix_system[i].getVolumeInLattice() * sciantix_variable[sv["Intragranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].getFinalValue()
				);
			}
		}

		// Intragranular bubble radius
		sciantix_variable[sv["Intragranular bubble radius"]].setFinalValue(0.620350491 * pow(sciantix_variable[sv["Intragranular bubble volume"]].getFinalValue(), (1.0 / 3.0)));

		// Swelling
		// 4/3 pi N R^3
		sciantix_variable[sv["Intragranular gas swelling"]].setFinalValue(4.188790205 *
			pow(sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue(), 3) *
			sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue()
		);

		if(sciantix_variable[sv["He in intragranular bubbles"]].getInitialValue() > 0.0)
			sciantix_variable[sv["Intragranular similarity ratio"]].setFinalValue(sqrt(sciantix_variable[sv["He in intragranular bubbles"]].getFinalValue() / sciantix_variable[sv["He in intragranular bubbles"]].getInitialValue()));
		else
			sciantix_variable[sv["Intragranular similarity ratio"]].setFinalValue(0.0);

	}

	void InterGranularBubbleBehaviour()
	{
		const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

		// Vacancy concentration
		sciantix_variable[sv["Intergranular vacancies per bubble"]].setFinalValue(
			solver.LimitedGrowth(sciantix_variable[sv["Intergranular vacancies per bubble"]].getInitialValue(),
				model[sm["Intergranular bubble evolution"]].getParameter(),
				physics_variable[pv["Time step"]].getFinalValue()
			)
		);

		// Grain-boundary bubble volume
		double vol(0);
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
			// This for loop slides over only the stable gases, which determine the grain-boundary bubble dynamics
		{
			if (gas[ga[sciantix_system[i].getGasName()]].getDecayRate() == 0.0)
			{
				vol += sciantix_variable[sv["Intergranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].getFinalValue() *
					gas[ga[sciantix_system[i].getGasName()]].getVanDerWaalsVolume();
			}
		}
		vol += sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[0].getSchottkyVolume();
		sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(vol);

		// Grain-boundary bubble radius
		sciantix_variable[sv["Intergranular bubble radius"]].setFinalValue(
			0.620350491 * pow(sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue() / (matrix[0].getLenticularShapeFactor()), 1. / 3.));

		// Grain-boundary bubble area
		sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(
			pi * pow(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() * sin(matrix[0].getSemidihedralAngle()), 2));

		// Grain-boundary bubble coalescence
		double dbubble_area = sciantix_variable[sv["Intergranular bubble area"]].getIncrement();
		sciantix_variable[sv["Intergranular bubble concentration"]].setFinalValue(
			solver.BinaryInteraction(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue(), 2.0, dbubble_area));

		// lower limit for bubble concentration
        if (sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue() < matrix[0].getBubbleConcentrationLimit())
            sciantix_variable[sv["Intergranular bubble concentration"]].setFinalValue(matrix[0].getBubbleConcentrationLimit());

		// Conservation
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			if (gas[ga[sciantix_system[i].getGasName()]].getDecayRate() == 0.0)
			{
				sciantix_variable[sv["Intergranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].rescaleFinalValue(
					sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() / sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue()
				);
			}
		}

		double n_at(0);
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			if (gas[ga[sciantix_system[i].getGasName()]].getDecayRate() == 0.0)
				n_at += sciantix_variable[sv["Intergranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].getFinalValue();
		}
		sciantix_variable[sv["Intergranular atoms per bubble"]].setFinalValue(n_at);

		sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleFinalValue(
			sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() / sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue()
		);

		vol = 0.0;
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			if (gas[ga[sciantix_system[i].getGasName()]].getDecayRate() == 0.0)
			{
				vol += sciantix_variable[sv["Intergranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].getFinalValue() *
					gas[ga[sciantix_system[i].getGasName()]].getVanDerWaalsVolume();
			}
		}
		vol += sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[0].getSchottkyVolume();
		sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(vol);

		sciantix_variable[sv["Intergranular bubble radius"]].setFinalValue(
			0.620350491 * pow(sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue() / (matrix[0].getLenticularShapeFactor()), 1. / 3.));

		sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(
			pi * pow(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() * sin(matrix[0].getSemidihedralAngle()), 2));

		// Fractional coverage
		sciantix_variable[sv["Intergranular fractional coverage"]].setFinalValue(
			sciantix_variable[sv["Intergranular bubble area"]].getFinalValue() *
			sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue());

		// Intergranular gas release
		//                          F0
		//   ___________A0____________
		//   |_________A1__________  |
		//   |                    |  |
		//   |          F1        N1 N0
		//   |                    |  |
		//   |____________________|__|
		double similarity_ratio;
		
		if (sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue() > 0.0)
			similarity_ratio = sqrt(
				sciantix_variable[sv["Intergranular saturation fractional coverage"]].getFinalValue() /
				sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue()
			);
		else
			similarity_ratio = 1.0;

		if (similarity_ratio < 1.0)
		{
            if (sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue() < matrix[0].getBubbleConcentrationLimit())
            {
                sciantix_variable[sv["Intergranular bubble concentration"]].setFinalValue(matrix[0].getBubbleConcentrationLimit());
                
                // Rescaling bubble area in order to have F_c = F_c,sat -> F_c = F_c,sat = N_bub,lim * A_bub,new
                sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(
                    sciantix_variable[sv["Intergranular saturation fractional coverage"]].getFinalValue()/matrix[0].getBubbleConcentrationLimit()
                );
                
                // A = pi (r sin) ^2
                sciantix_variable[sv["Intergranular bubble radius"]].setFinalValue(
                    1/sin(matrix[0].getSemidihedralAngle()) * sqrt(sciantix_variable[sv["Intergranular bubble area"]].getFinalValue()/pi)
                );
                sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(
                    4.0/3.0 * pi * pow(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue(),3) * matrix[0].getLenticularShapeFactor()
                );
                sciantix_variable[sv["Intergranular fractional coverage"]].rescaleFinalValue(pow(similarity_ratio, 2));
                sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleFinalValue(pow(similarity_ratio, 1.5));
            }
            else
            {
                sciantix_variable[sv["Intergranular bubble area"]].rescaleFinalValue(similarity_ratio);
                sciantix_variable[sv["Intergranular fractional coverage"]].rescaleFinalValue(pow(similarity_ratio, 2));
                sciantix_variable[sv["Intergranular bubble volume"]].rescaleFinalValue(pow(similarity_ratio, 1.5));
                sciantix_variable[sv["Intergranular bubble radius"]].rescaleFinalValue(pow(similarity_ratio, 0.5));
                sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleFinalValue(pow(similarity_ratio, 1.5));
            }

			// New intergranular gas concentration
			for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
			{
				if (gas[ga[sciantix_system[i].getGasName()]].getDecayRate() == 0.0)
					sciantix_variable[sv["Intergranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].rescaleFinalValue(pow(similarity_ratio, 1.5));
			}

			n_at = 0.0;
			for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
			{
				if (gas[ga[sciantix_system[i].getGasName()]].getDecayRate() == 0.0)
					n_at += sciantix_variable[sv["Intergranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].getFinalValue();
			}
			sciantix_variable[sv["Intergranular atoms per bubble"]].setFinalValue(n_at);

			for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
			{
				sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].rescaleFinalValue(pow(similarity_ratio, 2.5));
			}
		}

		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			sciantix_variable[sv[sciantix_system[i].getGasName() + " released"]].setFinalValue(
				sciantix_variable[sv[sciantix_system[i].getGasName() + " produced"]].getFinalValue() -
				sciantix_variable[sv[sciantix_system[i].getGasName() + " decayed"]].getFinalValue() -
				sciantix_variable[sv[sciantix_system[i].getGasName() + " in grain"]].getFinalValue() -
				sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].getFinalValue()
			);

			if(sciantix_variable[sv[sciantix_system[i].getGasName() + " released"]].getFinalValue() < 0.0)
				sciantix_variable[sv[sciantix_system[i].getGasName() + " released"]].setFinalValue(0.0);
		}

		// Intergranular gaseous swelling
		sciantix_variable[sv["Intergranular gas swelling"]].setFinalValue(
			3 / sciantix_variable[sv["Grain radius"]].getFinalValue() *
			sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue() *
			sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue()
		);
	}

	void GrainBoundarySweeping()
	{
		// Sweeping of the intra-granular gas concentrations
		// dC / df = - C

		if (!input_variable[iv["Grain-boundary sweeping"]].getValue()) return;

		// intra-granular gas diffusion modes
		switch (int(input_variable[iv["iDiffusionSolver"]].getValue()))
		{
			case 1:
			{
				for (int i = 0; i < n_modes; ++i)
				{
					modes_initial_conditions[6 * 40 + i] =
						solver.Decay(
							modes_initial_conditions[6 * 40 + i],
							1.0,
							0.0,
							model[sm["Grain-boundary sweeping"]].getParameter().at(0)
						);
				}
				
				break;
			}

			case 2:
			{
				for (int i = 0; i < n_modes; ++i)
				{
					modes_initial_conditions[7 * 40 + i] =
						solver.Decay(
							modes_initial_conditions[7 * 40 + i],
							1.0,
							0.0,
							model[sm["Grain-boundary sweeping"]].getParameter().at(0)
						);

					modes_initial_conditions[8 * 40 + i] =
						solver.Decay(
							modes_initial_conditions[8 * 40 + i],
							1.0,
							0.0,
							model[sm["Grain-boundary sweeping"]].getParameter().at(0)
						);
				}

				break;
			}

			default:
				ErrorMessages::Switch("Simulation.h", "iDiffusionSolver", int(input_variable[iv["iDiffusionSolver"]].getValue()));
				break;
		}
	}

	void GrainBoundaryMicroCracking()
	{
		/// GrainBoundaryMicroCracking is method of simulation which executes the SCIANTIX simulation for the grain-boundary micro-cracking induced by a temperature difference. 
		/// This method calls the related model "Grain-boundary micro-cracking", takes the model parameters and solve the model ODEs.

		if (!input_variable[iv["iGrainBoundaryMicroCracking"]].getValue()) return;

		// ODE for the intergranular fractional intactness: this equation accounts for the reduction of the intergranular fractional intactness following a temperature transient
		// df / dT = - dm/dT f
		sciantix_variable[sv["Intergranular fractional intactness"]].setFinalValue(
			solver.Decay(sciantix_variable[sv["Intergranular fractional intactness"]].getInitialValue(),
				model[sm["Grain-boundary micro-cracking"]].getParameter().at(0), // 1st parameter = microcracking parameter
				0.0,
				history_variable[hv["Temperature"]].getIncrement()
			)
		);

		// ODE for the intergranular fractional coverage: this equation accounts for the reduction of the intergranular fractional coverage following a temperature transient
		// dFc / dT = - ( dm/dT f) Fc
		sciantix_variable[sv["Intergranular fractional coverage"]].setFinalValue(
			solver.Decay(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(),
				model[sm["Grain-boundary micro-cracking"]].getParameter().at(0) * sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue(),
				0.0,
				history_variable[hv["Temperature"]].getIncrement()
			)
		);

		// ODE for the saturation fractional coverage: this equation accounts for the reduction of the intergranular saturation fractional coverage following a temperature transient
		// dFcsat / dT = - (dm/dT f) Fcsat
		sciantix_variable[sv["Intergranular saturation fractional coverage"]].setFinalValue(
			solver.Decay(
				sciantix_variable[sv["Intergranular saturation fractional coverage"]].getInitialValue(),
				model[sm["Grain-boundary micro-cracking"]].getParameter().at(0) * sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue(),
				0.0,
				history_variable[hv["Temperature"]].getIncrement()
			)
		);

		// ODE for the intergranular fractional intactness: this equation accounts for the healing of the intergranular fractional intactness with burnup
		// df / dBu = - h f + h
		sciantix_variable[sv["Intergranular fractional intactness"]].setFinalValue(
			solver.Decay(
				sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue(),
				model[sm["Grain-boundary micro-cracking"]].getParameter().at(1),  // 2nd parameter = healing parameter
				model[sm["Grain-boundary micro-cracking"]].getParameter().at(1),
				sciantix_variable[sv["Burnup"]].getIncrement()
			)
		);

		// ODE for the saturation fractional coverage: this equation accounts for the healing of the intergranular saturation fractional coverage with burnup
		// dFcsat / dBu = h (1-f) Fcsat
		sciantix_variable[sv["Intergranular saturation fractional coverage"]].setFinalValue(
			solver.Decay(
				sciantix_variable[sv["Intergranular saturation fractional coverage"]].getFinalValue(),
				model[sm["Grain-boundary micro-cracking"]].getParameter().at(1) * (1.0 - sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue()),
				0.0,
				sciantix_variable[sv["Burnup"]].getIncrement()
			)
		);

		// Re-scaling: to maintain the current fractional coverage unchanged
		double similarity_ratio;
		
		if (sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue() > 0.0)
			similarity_ratio = sqrt(
				sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue() / sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue()
			);
		else
			similarity_ratio = 1.0;

		if (similarity_ratio < 1.0)
		{
			sciantix_variable[sv["Intergranular bubble area"]].rescaleInitialValue(similarity_ratio);
			sciantix_variable[sv["Intergranular bubble concentration"]].rescaleInitialValue(similarity_ratio);
			sciantix_variable[sv["Intergranular fractional coverage"]].rescaleInitialValue(pow(similarity_ratio, 2));
			sciantix_variable[sv["Intergranular bubble volume"]].rescaleInitialValue(pow(similarity_ratio, 1.5));
			sciantix_variable[sv["Intergranular bubble radius"]].rescaleInitialValue(pow(similarity_ratio, 0.5));
			sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleInitialValue(pow(similarity_ratio, 1.5));

			for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
			{
				if (gas[ga[sciantix_system[i].getGasName()]].getDecayRate() == 0.0)
					sciantix_variable[sv["Intergranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].rescaleInitialValue(pow(similarity_ratio, 1.5));
			}

			double n_at(0);
			for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
			{
				if (gas[ga[sciantix_system[i].getGasName()]].getDecayRate() == 0.0)
					n_at += sciantix_variable[sv["Intergranular " + sciantix_system[i].getGasName() + " atoms per bubble"]].getInitialValue();
			}
			sciantix_variable[sv["Intergranular atoms per bubble"]].setInitialValue(n_at);

			for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
			{
				sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].rescaleFinalValue(pow(similarity_ratio, 2.5));
			}
		}

		// Fission gas release due to grain-boundary micro-cracking
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			sciantix_variable[sv[sciantix_system[i].getGasName() + " released"]].setFinalValue(
				sciantix_variable[sv[sciantix_system[i].getGasName() + " produced"]].getFinalValue() -
				sciantix_variable[sv[sciantix_system[i].getGasName() + " decayed"]].getFinalValue() -
				sciantix_variable[sv[sciantix_system[i].getGasName() + " in grain"]].getFinalValue() -
				sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].getFinalValue());
		}
	}

	void GrainBoundaryVenting()
	{
		if (!int(input_variable[iv["iGrainBoundaryVenting"]].getValue())) return;

		double sigmoid_variable;
		sigmoid_variable = sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue() *
			exp(-sciantix_variable[sv["Intergranular fractional intactness"]].getIncrement());

		// Vented fraction
		sciantix_variable[sv["Intergranular vented fraction"]].setFinalValue(
			1.0 /
			pow((1.0 + model[sm["Grain-boundary venting"]].getParameter().at(0) *
				exp(-model[sm["Grain-boundary venting"]].getParameter().at(1) *
					(sigmoid_variable - model[sm["Grain-boundary venting"]].getParameter().at(2)))),
				(1.0 / model[sm["Grain-boundary venting"]].getParameter().at(0)))
		);

		// Venting probability
		sciantix_variable[sv["Intergranular venting probability"]].setFinalValue(
			(1.0 - sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue())
			+ sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue() * sciantix_variable[sv["Intergranular vented fraction"]].getFinalValue()
		);

		// Gas is vented by subtracting a fraction of the gas concentration at grain boundaries arrived from diffusion
		// Bf = Bf - p_v * dB
		for (std::vector<System>::size_type i = 0; i != sciantix_system.size(); ++i)
		{
			sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].setFinalValue(
				solver.Integrator(
					sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].getFinalValue(),
					- sciantix_variable[sv["Intergranular venting probability"]].getFinalValue(),
					sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].getIncrement()
				)
			);
		sciantix_variable[sv[sciantix_system[i].getGasName() + " at grain boundary"]].resetValue();
		}
	}


	void HighBurnupStructureFormation()
	{
		/// @brief
		/// HighBurnupStructureFormation
		/// HighBurnupStructureFormation is used to compute the local restructured volume fraction of the fuel, in the HBS region.

		if (!int(input_variable[iv["iHighBurnupStructureFormation"]].getValue())) return;

		// Restructuring rate:
		// df / bu = 3.54 * 2.77e-7 * b^2.54 - 3.54 * 2.77e-7 * b^2.54 * f
		double coefficient =
			model[sm["High-burnup structure formation"]].getParameter().at(0) *
			model[sm["High-burnup structure formation"]].getParameter().at(1) *
			pow(sciantix_variable[sv["Effective burnup"]].getFinalValue(), 2.54);
		
		sciantix_variable[sv["Restructured volume fraction"]].setFinalValue(
			solver.Decay(
				sciantix_variable[sv["Restructured volume fraction"]].getInitialValue(),
				coefficient,
				coefficient,
				sciantix_variable[sv["Effective burnup"]].getIncrement()
				)
			);
	}

	void HighBurnupStructurePorosity()
	{
		/// @brief
		/// HighBurnupStructurePorosity is method of simulation which executes the SCIANTIX simulation for the evolution of the porosity of a HBS matrix. 
		/// This method takes the model parameters, solves the model ODEs and updates the matrix density coherently with the actual porosity.

		if (!int(input_variable[iv["iHighBurnupStructurePorosity"]].getValue())) return;

		// porosity evolution 
		sciantix_variable[sv["HBS porosity"]].setFinalValue(
			solver.Integrator(
				sciantix_variable[sv["HBS porosity"]].getInitialValue(),
				model[sm["High-burnup structure porosity"]].getParameter().at(0),
				sciantix_variable[sv["Burnup"]].getIncrement()
			)
		);

		if(sciantix_variable[sv["HBS porosity"]].getFinalValue() > 0.15)
			sciantix_variable[sv["HBS porosity"]].setFinalValue(0.15);

		// evolution of pore number density via pore nucleation and gas re-solution
		if(sciantix_variable[sv["HBS porosity"]].getFinalValue())
			sciantix_variable[sv["HBS pore density"]].setFinalValue(
				solver.Decay(
						sciantix_variable[sv["HBS pore density"]].getInitialValue(),
						matrix[0].getPoreResolutionRate(),
						matrix[0].getPoreNucleationRate(),
						physics_variable[pv["Time step"]].getFinalValue()
					)
				);
		else
			sciantix_variable[sv["HBS pore density"]].setFinalValue(0.0);

		// calculation of pore radius based on porosity and pore number density	
		if(sciantix_variable[sv["HBS pore density"]].getFinalValue())
			sciantix_variable[sv["HBS pore volume"]].setFinalValue(
				sciantix_variable[sv["HBS porosity"]].getFinalValue() / sciantix_variable[sv["HBS pore density"]].getFinalValue());

		sciantix_variable[sv["HBS pore radius"]].setFinalValue(0.620350491 * pow(sciantix_variable[sv["HBS pore volume"]].getFinalValue(), (1.0 / 3.0)));

		// update of number density of HBS pores: interconnection by impingement
		double limiting_factor =
			(2.0 - sciantix_variable[sv["HBS porosity"]].getFinalValue()) /
			(2.0 * pow(1.0 - sciantix_variable[sv["HBS porosity"]].getFinalValue(), 3.0));

		double pore_interconnection_rate = 4.0 * limiting_factor;
		sciantix_variable[sv["HBS pore density"]].setFinalValue(
			solver.BinaryInteraction(
				sciantix_variable[sv["HBS pore density"]].getFinalValue(),
				pore_interconnection_rate,
				sciantix_variable[sv["HBS pore volume"]].getIncrement()
			)
		);
		
		// update of pore volume and pore radius after interconnection by impingement
		if(sciantix_variable[sv["HBS pore density"]].getFinalValue())
			sciantix_variable[sv["HBS pore volume"]].setFinalValue(
				sciantix_variable[sv["HBS porosity"]].getFinalValue() / sciantix_variable[sv["HBS pore density"]].getFinalValue());

		sciantix_variable[sv["HBS pore radius"]].setFinalValue(0.620350491 * pow(sciantix_variable[sv["HBS pore volume"]].getFinalValue(), (1.0 / 3.0)));

		// average (at/m^3) of gas atoms in HBS pores
		sciantix_variable[sv["Xe in HBS pores"]].setFinalValue(
			solver.Integrator(
				sciantix_variable[sv["Xe in HBS pores"]].getInitialValue(),

				2.0 * matrix[0].getPoreNucleationRate() +
				sciantix_variable[sv["HBS pore density"]].getFinalValue() *
				(matrix[0].getPoreTrappingRate() - matrix[0].getPoreResolutionRate()),

				physics_variable[pv["Time step"]].getFinalValue()
			)
		);		

		if(sciantix_variable[sv["HBS pore density"]].getFinalValue())
			sciantix_variable[sv["Xe atoms per HBS pore"]].setFinalValue(
			sciantix_variable[sv["Xe in HBS pores"]].getFinalValue() / sciantix_variable[sv["HBS pore density"]].getFinalValue()
		);

		sciantix_variable[sv["Xe in HBS pores - variance"]].setFinalValue(
			solver.Integrator(
				sciantix_variable[sv["Xe in HBS pores - variance"]].getInitialValue(),

				matrix[0].getPoreTrappingRate() * sciantix_variable[sv["HBS pore density"]].getFinalValue() -
				matrix[0].getPoreResolutionRate() * sciantix_variable[sv["HBS pore density"]].getFinalValue() +
				matrix[0].getPoreNucleationRate() * pow((sciantix_variable[sv["Xe atoms per HBS pore"]].getFinalValue()-2.0), 2.0),

				physics_variable[pv["Time step"]].getFinalValue()
			)
		);

		if(sciantix_variable[sv["HBS pore density"]].getFinalValue())
			sciantix_variable[sv["Xe atoms per HBS pore - variance"]].setFinalValue(
				sciantix_variable[sv["Xe in HBS pores - variance"]].getFinalValue() / sciantix_variable[sv["HBS pore density"]].getFinalValue()
			);		
		}

void StoichiometryDeviation()
{
	if (!input_variable[iv["iStoichiometryDeviation"]].getValue()) return;

if(history_variable[hv["Temperature"]].getFinalValue() < 1000.0)
{
	sciantix_variable[sv["Stoichiometry deviation"]].setConstant();
	sciantix_variable[sv["Fuel oxygen partial pressure"]].setFinalValue(0.0);
}

else if(input_variable[iv["iStoichiometryDeviation"]].getValue() < 5)
{	
	sciantix_variable[sv["Stoichiometry deviation"]].setFinalValue(
	solver.Decay(
	sciantix_variable[sv["Stoichiometry deviation"]].getInitialValue(),
		model[sm["Stoichiometry deviation"]].getParameter().at(0),
		model[sm["Stoichiometry deviation"]].getParameter().at(1),
		physics_variable[pv["Time step"]].getFinalValue()
	)
	);
	}

	else if(input_variable[iv["iStoichiometryDeviation"]].getValue() > 4)
	{
		sciantix_variable[sv["Stoichiometry deviation"]].setFinalValue(
			solver.NewtonLangmuirBasedModel(
				sciantix_variable[sv["Stoichiometry deviation"]].getInitialValue(),
					model[sm["Stoichiometry deviation"]].getParameter(),
					physics_variable[pv["Time step"]].getFinalValue()
			)
		);
	}

	sciantix_variable[sv["Fuel oxygen partial pressure"]].setFinalValue(
		BlackburnThermochemicalModel(
			sciantix_variable[sv["Stoichiometry deviation"]].getFinalValue(),
			history_variable[hv["Temperature"]].getFinalValue()
			)
		);
}

void UO2Thermochemistry()
{
		if (!input_variable[iv["iStoichiometryDeviation"]].getValue()) return;

	if(history_variable[hv["Temperature"]].getFinalValue() < 1000.0 || sciantix_variable[sv["Gap oxygen partial pressure"]].getFinalValue() == 0)
			sciantix_variable[sv["Equilibrium stoichiometry deviation"]].setFinalValue(0.0);

	else
	sciantix_variable[sv["Equilibrium stoichiometry deviation"]].setFinalValue(
		solver.NewtonBlackburn(
			model[sm["UO2 thermochemistry"]].getParameter()
				)
	);
	}

	double* getDiffusionModes(std::string gas_name)
	{	
		if(gas_name == "Xe")
			return &modes_initial_conditions[0];

		else if(gas_name == "Kr")
			return &modes_initial_conditions[3 * 40];

		else if(gas_name == "He")
			return &modes_initial_conditions[6 * 40];

		else if(gas_name == "Xe133")
			return &modes_initial_conditions[9 * 40];

		else // (gas_name == "Kr85m")
			return &modes_initial_conditions[12 * 40];
	}

	double* getDiffusionModesSolution(std::string gas_name)
	{	
		if(gas_name == "Xe")
			return &modes_initial_conditions[1 * 40];

		else if(gas_name == "Kr")
			return &modes_initial_conditions[4 * 40];

		else if(gas_name == "He")
			return &modes_initial_conditions[7 * 40];

		else if(gas_name == "Xe133")
			return &modes_initial_conditions[10 * 40];

		else // (gas_name == "Kr85m")
			return &modes_initial_conditions[13 * 40];
	}

	double* getDiffusionModesBubbles(std::string gas_name)
	{	
		if(gas_name == "Xe")
			return &modes_initial_conditions[2 * 40];

		else if(gas_name == "Kr")
			return &modes_initial_conditions[5 * 40];

		else if(gas_name == "He")
			return &modes_initial_conditions[8 * 40];

		else if(gas_name == "Xe133")
			return &modes_initial_conditions[11 * 40];

		else // (gas_name == "Kr85m")
			return &modes_initial_conditions[14 * 40];
	}

	Simulation() {}
	~Simulation() {}
};

#endif
