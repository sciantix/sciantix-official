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
#include "SetGPVariables.h"

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

    	for (auto& system : sciantix_system)
		{	
			if(system.getRestructuredMatrix() == 0)
				sciantix_variable[sv[system.getGasName() + " produced"]].setFinalValue(
					solver.Integrator(
						sciantix_variable[sv[system.getGasName() + " produced"]].getInitialValue(),
						model[sm["Gas production - " + system.getName()]].getParameter().at(0),
						model[sm["Gas production - " + system.getName()]].getParameter().at(1)
					)
				);
			else if(system.getRestructuredMatrix() == 1)
				sciantix_variable[sv[system.getGasName() + " produced in HBS"]].setFinalValue(
					solver.Integrator(
						sciantix_variable[sv[system.getGasName() + " produced in HBS"]].getInitialValue(),
						model[sm["Gas production - " + system.getName()]].getParameter().at(0),
						model[sm["Gas production - " + system.getName()]].getParameter().at(1)
					)
				);
		}
	}

	void GasDecay()
	{
    	for (auto& system : sciantix_system)
		{
			if (gas[ga[system.getGasName()]].getDecayRate() > 0.0 && system.getRestructuredMatrix() == 0)
			{
				sciantix_variable[sv[system.getGasName() + " decayed"]].setFinalValue(
					solver.Decay(
						sciantix_variable[sv[system.getGasName() + " decayed"]].getInitialValue(),
						gas[ga[system.getGasName()]].getDecayRate(),
						gas[ga[system.getGasName()]].getDecayRate() * sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue(),
						physics_variable[pv["Time step"]].getFinalValue()
					)
				);
			}
		}
	}

	void GasDiffusion()
	{
    	for (auto& system : sciantix_system)
		{
			switch (int(input_variable[iv["iDiffusionSolver"]].getValue()))
			{
				case 1:
				{
					if (system.getRestructuredMatrix() == 0)
					{
						sciantix_variable[sv[system.getGasName() + " in grain"]].setFinalValue(
							solver.SpectralDiffusion(
								getDiffusionModes(system.getGasName()),
								model[sm["Gas diffusion - " + system.getName()]].getParameter(),
								physics_variable[pv["Time step"]].getFinalValue()
							)
						);

						double equilibrium_fraction(1.0);
						if ((system.getResolutionRate() + system.getTrappingRate()) > 0.0)
							equilibrium_fraction = system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate());

						sciantix_variable[sv[system.getGasName() + " in intragranular solution"]].setFinalValue(
							equilibrium_fraction * sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
						);

						sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].setFinalValue(
							(1.0 - equilibrium_fraction) * sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
						);
					}
					else if (system.getRestructuredMatrix() == 1)
					{
						sciantix_variable[sv[system.getGasName() + " in grain HBS"]].setFinalValue(
							solver.SpectralDiffusion(
								getDiffusionModes(system.getGasName() + " in HBS"),
								model[sm["Gas diffusion - " + system.getName()]].getParameter(),
								physics_variable[pv["Time step"]].getFinalValue()
							)
						);
					}
					break;
				}

				case 2:
				{
					double initial_value_solution(0.0), initial_value_bubbles(0.0);

					if (system.getRestructuredMatrix() == 0)
					{
						initial_value_solution = sciantix_variable[sv[system.getGasName() + " in intragranular solution"]].getFinalValue();
						initial_value_bubbles  = sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].getFinalValue();

						solver.SpectralDiffusionNonEquilibrium(
							initial_value_solution,
							initial_value_bubbles,
							getDiffusionModesSolution(system.getGasName()),
							getDiffusionModesBubbles(system.getGasName()),
							model[sm["Gas diffusion - " + system.getName()]].getParameter(),
							physics_variable[pv["Time step"]].getFinalValue()
						);
						sciantix_variable[sv[system.getGasName() + " in intragranular solution"]].setFinalValue(initial_value_solution);
						sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].setFinalValue(initial_value_bubbles);
						sciantix_variable[sv[system.getGasName() + " in grain"]].setFinalValue(initial_value_solution + initial_value_bubbles);
					}
					else if (system.getRestructuredMatrix() == 1)
					{
						initial_value_solution = sciantix_variable[sv[system.getGasName() + " in grain HBS"]].getFinalValue();

						solver.SpectralDiffusionNonEquilibrium(
							initial_value_solution,
							initial_value_bubbles,
							getDiffusionModesSolution(system.getGasName()),
							getDiffusionModesBubbles(system.getGasName()),
							model[sm["Gas diffusion - " + system.getName()]].getParameter(),
							physics_variable[pv["Time step"]].getFinalValue()
						);
						sciantix_variable[sv[system.getGasName() + " in grain"]].setFinalValue(initial_value_solution + initial_value_bubbles);
					}					
					break;
				}

				default:
					ErrorMessages::Switch("Simulation.h", "iDiffusionSolver", int(input_variable[iv["iDiffusionSolver"]].getValue()));
					break;
			}
		}

		// Calculation of the gas concentration arrived at the grain boundary, by mass balance.
		for (auto& system : sciantix_system)
		{
			if(system.getRestructuredMatrix() == 0)
			{
				sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(
					sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
					sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
					sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
					sciantix_variable[sv[system.getGasName() + " released"]].getFinalValue()
				);

				if (sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
					sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);
			}
		}

		/**
		 * @brief If **iGrainBoundaryBehaviour = 0** (e.g., grain-boundary calculations are neglected), 
		 * all the gas arriving at the grain boundary is released.
		 * 
		 */
		if (input_variable[iv["iGrainBoundaryBehaviour"]].getValue() == 0)
		{
			for (auto& system : sciantix_system)
			{
				if(system.getRestructuredMatrix() == 0)
				{
					{
						sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setInitialValue(0.0);
						sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);

						sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
							sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
							sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
							sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
						);
					}
				}
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
		for (auto& system : sciantix_system)
		{
			if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
			{
				if (sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue() > 0.0)
					sciantix_variable[sv["Intragranular " + system.getGasName() + " atoms per bubble"]].setFinalValue(
						sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].getFinalValue() /
						sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue()
					);

				else
					sciantix_variable[sv["Intragranular " + system.getGasName() + " atoms per bubble"]].setFinalValue(0.0);

				sciantix_variable[sv["Intragranular bubble volume"]].addValue(
					system.getVolumeInLattice() * sciantix_variable[sv["Intragranular " + system.getGasName() + " atoms per bubble"]].getFinalValue()
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
		// const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

		// Vacancy concentration
		sciantix_variable[sv["Intergranular vacancies per bubble"]].setFinalValue(
			solver.LimitedGrowth(sciantix_variable[sv["Intergranular vacancies per bubble"]].getInitialValue(),
				model[sm["Intergranular bubble evolution"]].getParameter(),
				physics_variable[pv["Time step"]].getFinalValue()
			)
		);

		// int ngas = gas.size();
		// int rows = 5+ngas;
		int rows = 4;
		int ngas = 0;
		std::cout << "Matrix size: " << rows << std::endl;
		std::vector<std::vector<double>> matrixGB(rows, std::vector<double>(rows, 0.0));
		std::vector<double> initialGB(rows, 0.0);

		// std::vector<double> initialx;
		// auto parameters = model[sm["Intergranular bubble evolution"]].getParameter();

		// std::cout << "The vector xo is: \n"<<std::endl;
		// for (int i = 2; i < initialx.size(); ++i) {
		// 	initialx.push_back(parameters[i]);
		// 	std::cout << initialx[i] << std::endl;
		// }

		// //This i dont know
		// int k = 0;
		// for (auto& system : sciantix_system) {
		// 	sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setInitialValue(initialx[k]);
		// 	k++;
		// }
		// sciantix_variable[sv["Intergranular bubble volume"]].setInitialValue(initialx[ngas]);
		// sciantix_variable[sv["Intergranular bubble area"]].setInitialValue(initialx[ngas+1]);
		// sciantix_variable[sv["Intergranular bubble concentration"]].setInitialValue(initialx[ngas+2]);
		// sciantix_variable[sv["Intergranular fractional coverage"]].setInitialValue(initialx[ngas+3]);
		// sciantix_variable[sv["Intergranular vented fraction"]].setInitialValue(initialx[ngas+4]);
		// ///
		
		for (int row=0;row<rows;++row)
		{	
		 	matrixGB[row][row]=1.0;
		}

		std::vector<double> dS;
		std::vector<double> Bi;
		std::vector<double> Sf;
		std::vector<double> Bf;
		std::vector<double> Si;
		std::vector<double> w;
		double wBi(0.0);
		double wBf(0.0);

		for (auto& system : sciantix_system)
		{
			dS.push_back(sciantix_variable[sv[system.getGasName() + " produced"]].getIncrement() - sciantix_variable[sv[system.getGasName() + " in grain"]].getIncrement() - sciantix_variable[sv[system.getGasName() + " decayed"]].getIncrement());
			Bi.push_back(sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getInitialValue());
			Bf.push_back(sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue());
			Sf.push_back(sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() - sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() - sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue());
			Si.push_back(sciantix_variable[sv[system.getGasName() + " produced"]].getInitialValue() - sciantix_variable[sv[system.getGasName() + " in grain"]].getInitialValue() - sciantix_variable[sv[system.getGasName() + " decayed"]].getInitialValue());
			w.push_back(gas[ga[system.getGasName()]].getVanDerWaalsVolume());
			wBi += (gas[ga[system.getGasName()]].getVanDerWaalsVolume()*sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getInitialValue());
			wBf += (gas[ga[system.getGasName()]].getVanDerWaalsVolume()*sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue());
		}

		// for (int row=0;row<ngas;++row)
		// {
		// 	// not linearized
		// 	matrixGB[row][rows-1]=dS[row]-Sf[row];

		// 	// linearized
		// 	// matrixGB[row][rows-1]=-Si[row];
			
		// 	matrixGB[ngas][row]=(-w[row]/(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue()*3/sciantix_variable[sv["Grain radius"]].getFinalValue()));
		// 	// off nucleation term 
		// 	matrixGB[ngas][ngas+2]=(+wBi/(pow(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue(),2)*3/sciantix_variable[sv["Grain radius"]].getFinalValue()));
		// }

		///NEW
		matrixGB[0][2]=(+wBf/(pow(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue(),2)*3/sciantix_variable[sv["Grain radius"]].getFinalValue()));
		matrixGB[1][0]=-(sciantix_variable[sv["Intergranular bubble area"]].getInitialValue()*2)/
			(sciantix_variable[sv["Intergranular bubble volume"]].getInitialValue()*3);
		matrixGB[2][1]= +2*pow(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue(),2);
		matrixGB[3][1]=-(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue());
		matrixGB[3][2]=-(sciantix_variable[sv["Intergranular bubble area"]].getInitialValue());
		///

		///OLD
		// matrixGB[ngas+1][ngas]=-(sciantix_variable[sv["Intergranular bubble area"]].getInitialValue()*2)/
		// 	(sciantix_variable[sv["Intergranular bubble volume"]].getInitialValue()*3);
		// matrixGB[ngas+2][ngas+1]= +2*pow(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue(),2);
		// matrixGB[ngas+3][ngas+1]=-(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue());
		// matrixGB[ngas+3][ngas+2]=-(sciantix_variable[sv["Intergranular bubble area"]].getInitialValue());
		// matrixGB[ngas+4][ngas+3]=-sciantix_variable[sv["Intergranular vented fraction derivative"]].getInitialValue();
		// ///

		std::cout << "Matrix:" << std::endl;
		for (int i = 0; i < rows; ++i) {
			for (int j = 0; j < rows; ++j) {
				if (std::isnan(matrixGB[i][j])) {
					matrixGB[i][j] = 0.0;
				}
				std::cout << matrixGB[i][j] << "\t";
			}
			std::cout << std::endl;
		}

		///OLD
		// for (int row=0;row<ngas;++row)
		// {
		// 	// not linearized
		// 	initialGB[row]=(Bi[row]-Sf[row]*sciantix_variable[sv["Intergranular vented fraction"]].getInitialValue()+dS[row]);
		// 	// linearized
		// 	//initialGB[row]=(1-sciantix_variable[sv["Intergranular vented fraction"]].getInitialValue())*dS[row]/physics_variable[pv["Time step"]].getFinalValue();
		// }
		// // linearized
		// //initialGB[ngas]=(sciantix_variable[sv["Intergranular vacancies per bubble"]].getIncrement() * matrix[sma["UO2"]].getSchottkyVolume())/physics_variable[pv["Time step"]].getFinalValue();
		// //not linearized
		// initialGB[ngas]=(sciantix_variable[sv["Intergranular bubble volume"]].getInitialValue()
		// 	+sciantix_variable[sv["Intergranular vacancies per bubble"]].getIncrement() * matrix[sma["UO2"]].getSchottkyVolume());
		// // initialGB[ngas]=(sciantix_variable[sv["Intergranular bubble volume"]].getInitialValue()-
		// // 	wBi/(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue()*3 / sciantix_variable[sv["Grain radius"]].getFinalValue())+
		// // 	sciantix_variable[sv["Intergranular vacancies per bubble"]].getIncrement() * matrix[sma["UO2"]].getSchottkyVolume());
		// initialGB[ngas+1]=sciantix_variable[sv["Intergranular bubble area"]].getInitialValue()/3;
		// initialGB[ngas+2]=sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue()+
		// 	2*pow(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue(),2)*sciantix_variable[sv["Intergranular bubble area"]].getInitialValue();
		// initialGB[ngas+3]=(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue()-
		// 	2*sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue()*sciantix_variable[sv["Intergranular bubble area"]].getInitialValue());
		// initialGB[ngas+4]=sciantix_variable[sv["Intergranular vented fraction"]].getInitialValue()-
		// 	sciantix_variable[sv["Intergranular vented fraction derivative"]].getInitialValue()*sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue();
		
		///NEW
		initialGB[ngas]=(sciantix_variable[sv["Intergranular bubble volume"]].getInitialValue()
			+sciantix_variable[sv["Intergranular vacancies per bubble"]].getIncrement() * matrix[sma["UO2"]].getSchottkyVolume())+
			(2*wBf-wBi)/(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue()*3 / sciantix_variable[sv["Grain radius"]].getFinalValue());
		initialGB[ngas+1]=sciantix_variable[sv["Intergranular bubble area"]].getInitialValue()/3;
		initialGB[ngas+2]=sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue()+
			2*pow(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue(),2)*sciantix_variable[sv["Intergranular bubble area"]].getInitialValue();
		initialGB[ngas+3]=(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue()-
			2*sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue()*sciantix_variable[sv["Intergranular bubble area"]].getInitialValue());
		///

		
		std::cout << "The vector b is: \n"<<std::endl;
		for (int i = 0; i < rows; ++i) {
			if (std::isnan(initialGB[i])) {
				initialGB[i] = 0.0;
			}
			std::cout << initialGB[i] << std::endl;
		}
			

		//Partial pivoting begins here.
		double compA, tempA, tempb, factorA;
		//This grand for loop turns matrix A into upper triangular form, using partial pivoting.
		for(int i = 0; i < rows; i++){
			//Initializes maximum value of pivot, compA and its index, q.
			compA = matrixGB[i][i];
			int q = i;
				//This for loop compares each pivot candidate and finds the maximum of them, which becomes compA.
					for(int j = i + 1; j < rows; j++)
					if(fabs(compA) < fabs(matrixGB[j][i])){
						compA = matrixGB[j][i] ;
							q = j;
						}
			//This if statement checks if A is singular and if so, quits.
			//Matrix A is singular if a principal diagonalelement is equal to zero.
			//For machine precision, 10^-(-5) is picked. The values smaller than this number are considered to be zero.
			// if(fabs(compA) < 0.00001){ 
			// 	std::cout << "A is singular."<<std::endl;
			// }
			//Swaps the maximum row with the current row for A.
			for(int k = 0; k < rows; k++)
			{
					tempA = matrixGB[q][k];
					matrixGB[q][k] = matrixGB[i][k];
					matrixGB[i][k] = tempA;       
			}
			//Swaps the same rows of b.
			tempb = initialGB[q];
			initialGB[q] = initialGB[i];
			initialGB[i] = tempb;
			//Turns A into an upper triangular matrix.
			for(int l = i+1; l < rows; l++){
					//factorA sets the l'th element of A equal to zero.
				factorA = matrixGB[l][i]/matrixGB[i][i];
					initialGB[l] = initialGB[l] - factorA * initialGB[i];
					//This part sets the elements under the diagonal equal to zero.
					for(int m = 0; m < rows; m++){
						matrixGB[l][m] = matrixGB[l][m] - factorA * matrixGB[i][m];
					// if(fabs(matrixGB[l][m]) < 0.00001)
					// 	matrixGB[l][m] = 0;
				}
			}
		}
		//Creates solution vector x with dimensions nx1.
		double x[rows];
		//Starts from bottom for backward substitution.
		for(int i = rows-1; i >= 0; i--){
			//Sets i'th element of x is equal to the i'th element of b divided by the i'th element of A. 
			x[i] = initialGB[i] / matrixGB[i][i];
			//Substracts the product of x[i] and storedA[j][i] from storedb[j] and finds the solution vector x.
			for(int j = 0; j < i; j++){
				initialGB[j] = initialGB[j] - matrixGB[j][i]*x[i];
			}
		}
		//Prints out the solution vector x.
		std::cout << "The solution vector x is: \n"<<std::endl;
		for (int i = 0; i < rows; i++){
			std::cout << x[i] << "\n"<<std::endl;
		}

		// // not linearized
		// int i = 0;
		// for (auto& system : sciantix_system) {
		// 	sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(x[i]);
		// 	i++;
		// }
		// sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(x[ngas]);
		// sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(x[ngas+1]);
		// sciantix_variable[sv["Intergranular bubble concentration"]].setFinalValue(x[ngas+2]);
		// sciantix_variable[sv["Intergranular fractional coverage"]].setFinalValue(x[ngas+3]);
		// sciantix_variable[sv["Intergranular vented fraction"]].setFinalValue(x[ngas+4]);

		// // linearized
		// int i = 0;
		// for (auto& system : sciantix_system) {
		// 	sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(
		// 		sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getInitialValue()+
		// 		x[i]*physics_variable[pv["Time step"]].getFinalValue()
		// 	);
		// 	i++;
		// }
		// sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(
		// 	sciantix_variable[sv["Intergranular bubble volume"]].getInitialValue()+
		// 	x[ngas]*physics_variable[pv["Time step"]].getFinalValue()
		// );
		// sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(
		// 	sciantix_variable[sv["Intergranular bubble area"]].getInitialValue()+
		// 	x[ngas+1]*physics_variable[pv["Time step"]].getFinalValue()
		// );
		// sciantix_variable[sv["Intergranular bubble concentration"]].setFinalValue(
		// 	sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue()+
		// 	x[ngas+2]*physics_variable[pv["Time step"]].getFinalValue()
		// );
		// sciantix_variable[sv["Intergranular fractional coverage"]].setFinalValue(
		// 	sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue()+
		// 	x[ngas+3]*physics_variable[pv["Time step"]].getFinalValue()
		// );
		// sciantix_variable[sv["Intergranular vented fraction"]].setFinalValue(
		// 	sciantix_variable[sv["Intergranular vented fraction"]].getInitialValue()+
		// 	x[ngas+4]*physics_variable[pv["Time step"]].getFinalValue()
		// );

		//NEW
		sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(x[ngas]);
		sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(x[ngas+1]);
		sciantix_variable[sv["Intergranular bubble concentration"]].setFinalValue(x[ngas+2]);
		sciantix_variable[sv["Intergranular fractional coverage"]].setFinalValue(x[ngas+3]);

		sciantix_variable[sv["Intergranular bubble radius"]].setFinalValue(
				0.620350491 * pow(sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue() / (matrix[sma["UO2"]].getLenticularShapeFactor()), 1. / 3.));

		SetGPVariables();			

		// std::cout << "\nIntergranular bubble behaviour" << std::endl;

		// double tol(1e-3);
		// int iter(0);
		// int max_iter(10);		

		// while (iter < max_iter)
		// {	
		// 	std::cout << "Time = " << history_variable[hv["Time"]].getFinalValue() << std::endl;
		// 	std::cout << "\n-----------------------" << std::endl;
		// 	std::cout << "iter = " << iter << std::endl;

		// 	std::cout << "\nInitial values" << std::endl;
		// 	std::cout << "at/bub  = " << sciantix_variable[sv["Intergranular atoms per bubble"]].getInitialValue() << std::endl;
		// 	std::cout << "vac/bub = " << sciantix_variable[sv["Intergranular vacancies per bubble"]].getInitialValue() << std::endl;
		// 	std::cout << "A_gb =    " << sciantix_variable[sv["Intergranular bubble area"]].getInitialValue() << std::endl;
		// 	std::cout << "N_gb =    " << sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() << std::endl;
		// 	std::cout << "F_v  =    " << sciantix_variable[sv["Intergranular vented fraction"]].getInitialValue() << std::endl;

		// 	// Grain-boundary bubble volume
		// 	double vol(0);
		// 	for (auto& system : sciantix_system)
		// 	{
		// 		if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
		// 		{
		// 			vol += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue() *
		// 				gas[ga[system.getGasName()]].getVanDerWaalsVolume();
		// 		}
		// 	}
		// 	vol += sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[sma["UO2"]].getSchottkyVolume();
		// 	sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(vol);

		// 	// Grain-boundary bubble radius
		// 	sciantix_variable[sv["Intergranular bubble radius"]].setFinalValue(
		// 		0.620350491 * pow(sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue() / (matrix[sma["UO2"]].getLenticularShapeFactor()), 1. / 3.));

		// 	// Grain-boundary bubble area
		// 	sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(
		// 		pi * pow(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() * sin(matrix[sma["UO2"]].getSemidihedralAngle()), 2));

		// 	// Grain-boundary bubble coalescence
		// 	// double dbubble_area = sciantix_variable[sv["Intergranular bubble area"]].getIncrement();
		// 	sciantix_variable[sv["Intergranular bubble concentration"]].setFinalValue(
		// 		solver.BinaryInteraction(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue(), 2.0, sciantix_variable[sv["Intergranular bubble area"]].getIncrement()));

		// 	// Fractional coverage
		// 	sciantix_variable[sv["Intergranular fractional coverage"]].setFinalValue(
		// 		sciantix_variable[sv["Intergranular bubble area"]].getFinalValue() *
		// 		sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue());

		// 	// update vented fraction
		// 	SetGPVariables();

		// 	// Conservation
		// 	double n_at(0);
		// 	for (auto& system : sciantix_system)
		// 	{
		// 		if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
		// 		{
		// 			sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].rescaleFinalValue(
		// 				sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() / sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue()
		// 			);
		// 			n_at += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue();
		// 		}
		// 	}
		// 	sciantix_variable[sv["Intergranular atoms per bubble"]].setFinalValue(n_at);

		// 	sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleFinalValue(
		// 		sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() / sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue()
		// 	);

		// 	double err(0);
		// 	if(sciantix_variable[sv["Intergranular bubble area"]].getFinalValue())
		// 		err = sciantix_variable[sv["Intergranular bubble area"]].getIncrement() / sciantix_variable[sv["Intergranular bubble area"]].getFinalValue();
			
		// 	std::cout << "\nFinal values" << std::endl;
		// 	std::cout << "at/bub  = " << sciantix_variable[sv["Intergranular atoms per bubble"]].getFinalValue() << std::endl;
		// 	std::cout << "vac/bub = " << sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() << std::endl;
		// 	std::cout << "A_gb =    " << sciantix_variable[sv["Intergranular bubble area"]].getFinalValue() << std::endl;
		// 	std::cout << "N_gb =    " << sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue() << std::endl;
		// 	std::cout << "F_v  =    " << sciantix_variable[sv["Intergranular vented fraction"]].getFinalValue() << std::endl;
		// 	std::cout << "ERROR:    " << err << std::endl;

		// 	std::cout << "\nIncrements" << std::endl;
		// 	std::cout << "at/bub  = " << sciantix_variable[sv["Intergranular atoms per bubble"]].getIncrement() << std::endl;
		// 	std::cout << "vac/bub = " << sciantix_variable[sv["Intergranular vacancies per bubble"]].getIncrement() << std::endl;
		// 	std::cout << "A_gb =    " << sciantix_variable[sv["Intergranular bubble area"]].getIncrement() << std::endl;
		// 	std::cout << "N_gb =    " << sciantix_variable[sv["Intergranular bubble concentration"]].getIncrement() << std::endl;
		// 	std::cout << "F_v  =    " << sciantix_variable[sv["Intergranular vented fraction"]].getIncrement() << std::endl;

		// 	if(fabs(err)<tol)
		// 	{
		// 		std::cout << "ERR " << fabs(err) << " < " << tol << std::endl;
		// 		std::cout << "!! End of the intergranular loop !!" << std::endl;
		// 		sciantix_variable[sv["Intergranular vacancies per bubble"]].resetValue();

		// 		break;
		// 	}

		// 	iter++;

		// 	sciantix_variable[sv["Intergranular atoms per bubble"]].resetValue();
		// 	sciantix_variable[sv["Intergranular bubble area"]].resetValue();
		// 	sciantix_variable[sv["Intergranular bubble concentration"]].resetValue();
		// 	//sciantix_variable[sv["Intergranular vented fraction"]].resetValue(); // forse solo per coerenza, per vedere nel log l'incremento
		// }
		
		// Intergranular gaseous swelling
		sciantix_variable[sv["Intergranular gas swelling"]].setFinalValue(
			3 / sciantix_variable[sv["Grain radius"]].getFinalValue() *
			sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue() *
			sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue()
		);

		std::cout << "	\nGasRelease" << std::endl;

		// dB/dt
		double source_rate(0.0), decay_rate(0.0);
		
		for (auto& system : sciantix_system)
		{
			if(physics_variable[pv["Time step"]].getFinalValue())
			{
				std::cout << "Derivative = " << sciantix_variable[sv["Intergranular vented fraction derivative"]].getFinalValue()*sciantix_variable[sv["Intergranular fractional coverage"]].getIncrement()<< std::endl;
				std::cout << "Increment = " << sciantix_variable[sv["Intergranular vented fraction"]].getIncrement() << std::endl;

				source_rate = (1.0 - sciantix_variable[sv["Intergranular vented fraction"]].getFinalValue()) * (sciantix_variable[sv[system.getGasName() + " produced"]].getIncrement() - sciantix_variable[sv[system.getGasName() + " in grain"]].getIncrement() - sciantix_variable[sv[system.getGasName() + " decayed"]].getIncrement()) / physics_variable[pv["Time step"]].getFinalValue() - 
						(sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() - sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() - sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue()) * sciantix_variable[sv["Intergranular vented fraction"]].getIncrement() / physics_variable[pv["Time step"]].getFinalValue();
				
				std::cout << "Source rate = " << source_rate << std::endl;
			}
			sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(
				solver.Decay(
					sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getInitialValue(),
					decay_rate,
					source_rate,
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);
		
		std::cout << "B_i  =    " << sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getInitialValue() << std::endl;
		std::cout << "B_f  =    " << sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() << std::endl;
		std::cout << "d_B  =    " << sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getIncrement() << std::endl;
		//sciantix_variable[sv[system.getGasName() + " at grain boundary"]].resetValue();
		}

		for (auto& system : sciantix_system)
		{
			if(system.getRestructuredMatrix() == 0)
			{
				sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
					sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
					sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
					sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
					sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue()
				);

				if (sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
					sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);
			}
		}

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

		// // ODE for the intergranular fractional coverage: this equation accounts for the reduction of the intergranular fractional coverage following a temperature transient
		// // dFc / dT = - ( dm/dT f) Fc
		// sciantix_variable[sv["Intergranular fractional coverage"]].setFinalValue(
		// 	solver.Decay(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(),
		// 		model[sm["Grain-boundary micro-cracking"]].getParameter().at(0) * sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue(),
		// 		0.0,
		// 		history_variable[hv["Temperature"]].getIncrement()
		// 	)
		// );

		// // ODE for the saturation fractional coverage: this equation accounts for the reduction of the intergranular saturation fractional coverage following a temperature transient
		// // dFcsat / dT = - (dm/dT f) Fcsat
		// sciantix_variable[sv["Intergranular saturation fractional coverage"]].setFinalValue(
		// 	solver.Decay(
		// 		sciantix_variable[sv["Intergranular saturation fractional coverage"]].getInitialValue(),
		// 		model[sm["Grain-boundary micro-cracking"]].getParameter().at(0) * sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue(),
		// 		0.0,
		// 		history_variable[hv["Temperature"]].getIncrement()
		// 	)
		// );

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

		// // ODE for the saturation fractional coverage: this equation accounts for the healing of the intergranular saturation fractional coverage with burnup
		// // dFcsat / dBu = h (1-f) Fcsat
		// sciantix_variable[sv["Intergranular saturation fractional coverage"]].setFinalValue(
		// 	solver.Decay(
		// 		sciantix_variable[sv["Intergranular saturation fractional coverage"]].getFinalValue(),
		// 		- model[sm["Grain-boundary micro-cracking"]].getParameter().at(1) * (1.0 - sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue()),
		// 		0.0,
		// 		sciantix_variable[sv["Burnup"]].getIncrement()
		// 	)
		// );

		// // Re-scaling: to maintain the current fractional coverage unchanged
		// double similarity_ratio;
		
		// if (sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue() > 0.0)
		// 	similarity_ratio = sqrt(
		// 		sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue() / sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue()
		// 	);
		// else
		// 	similarity_ratio = 1.0;

		// if (similarity_ratio < 1.0)
		// {
		// 	sciantix_variable[sv["Intergranular bubble area"]].rescaleInitialValue(similarity_ratio);
		// 	sciantix_variable[sv["Intergranular bubble concentration"]].rescaleInitialValue(similarity_ratio);
		// 	sciantix_variable[sv["Intergranular fractional coverage"]].rescaleInitialValue(pow(similarity_ratio, 2));
		// 	sciantix_variable[sv["Intergranular bubble volume"]].rescaleInitialValue(pow(similarity_ratio, 1.5));
		// 	sciantix_variable[sv["Intergranular bubble radius"]].rescaleInitialValue(pow(similarity_ratio, 0.5));
		// 	sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleInitialValue(pow(similarity_ratio, 1.5));

		// 	for (auto& system : sciantix_system)
		// 	{
		// 		if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
		// 			sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].rescaleInitialValue(pow(similarity_ratio, 1.5));
		// 	}

		// 	double n_at(0);
		// 	for (auto& system : sciantix_system)
		// 	{
		// 		if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
		// 			n_at += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getInitialValue();
		// 	}
		// 	sciantix_variable[sv["Intergranular atoms per bubble"]].setInitialValue(n_at);

		// 	for (auto& system : sciantix_system)
		// 	{	
		// 		if(system.getRestructuredMatrix() == 0)
		// 			sciantix_variable[sv[system.getGasName() + " at grain boundary"]].rescaleFinalValue(pow(similarity_ratio, 2.5));
		// 	}
		// }

		// Calculation of the gas concentration arrived at the grain boundary, by mass balance.
		// for (auto& system : sciantix_system)
		// {
		// 	if(system.getRestructuredMatrix() == 0)
		// 	{
		// 		sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
		// 			sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
		// 			sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
		// 			sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
		// 			sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue()
		// 		);

		// 		if (sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
		// 			sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);
		// 	}
		// }
	}

	void GrainBoundaryVenting()
	{
	
		if (!int(input_variable[iv["iGrainBoundaryVenting"]].getValue())) return;
		
		std::cout << "	\nGasRelease" << std::endl;

		// dB/dt
		double source_rate(0.0), decay_rate(0.0);


		// Venting probability
		sciantix_variable[sv["Intergranular venting probability"]].setFinalValue(
			(1.0 - sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue()) + sciantix_variable[sv["Intergranular vented fraction"]].getFinalValue() * sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue()
		);
		std::cout << "p_v  =    " << sciantix_variable[sv["Intergranular venting probability"]].getInitialValue() << std::endl;
		
		
		for (auto& system : sciantix_system)
		{
			if(physics_variable[pv["Time step"]].getFinalValue())
			{
				source_rate = (1.0 - sciantix_variable[sv["Intergranular venting probability"]].getFinalValue()) * (sciantix_variable[sv[system.getGasName() + " produced"]].getIncrement() - sciantix_variable[sv[system.getGasName() + " in grain"]].getIncrement() - sciantix_variable[sv[system.getGasName() + " decayed"]].getIncrement()) / physics_variable[pv["Time step"]].getFinalValue() - 
						(sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() - sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() - sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue()) * sciantix_variable[sv["Intergranular venting probability"]].getIncrement() / physics_variable[pv["Time step"]].getFinalValue();
				
				std::cout << "Source rate = " << source_rate << std::endl;
			}
			sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(
				solver.Decay(
					sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getInitialValue(),
					decay_rate,
					source_rate,
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);
		
		std::cout << "B_i  =    " << sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getInitialValue() << std::endl;
		std::cout << "B_f  =    " << sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() << std::endl;
		std::cout << "d_B  =    " << sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getIncrement() << std::endl;
		//sciantix_variable[sv[system.getGasName() + " at grain boundary"]].resetValue();
		}

		for (auto& system : sciantix_system)
		{
			if(system.getRestructuredMatrix() == 0)
			{
				sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
					sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
					sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
					sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
					sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue()
				);

				if (sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
					sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);
			}
		}
	}

	void HighBurnupStructureFormation()
	{
		if (!int(input_variable[iv["iHighBurnupStructureFormation"]].getValue())) return;

		// Restructuring rate:
		// dalpha_r / bu = 3.54 * 2.77e-7 (1-alpha_r) b^2.54
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

		// evolution of pore number density via pore nucleation and re-solution
		if(sciantix_variable[sv["HBS porosity"]].getFinalValue())
			sciantix_variable[sv["HBS pore density"]].setFinalValue(
				solver.Decay(
						sciantix_variable[sv["HBS pore density"]].getInitialValue(),
						matrix[sma["UO2HBS"]].getPoreResolutionRate(),
						matrix[sma["UO2HBS"]].getPoreNucleationRate(),
						physics_variable[pv["Time step"]].getFinalValue()
					)
				);
		else
			sciantix_variable[sv["HBS pore density"]].setFinalValue(0.0);

		// calculation of pore volume based on porosity and pore number density	
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

				2.0 * matrix[sma["UO2HBS"]].getPoreNucleationRate() +
				sciantix_variable[sv["HBS pore density"]].getFinalValue() *
				(matrix[sma["UO2HBS"]].getPoreTrappingRate() - matrix[sma["UO2HBS"]].getPoreResolutionRate()),

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

				matrix[sma["UO2"]].getPoreTrappingRate() * sciantix_variable[sv["HBS pore density"]].getFinalValue() -
				matrix[sma["UO2"]].getPoreResolutionRate() * sciantix_variable[sv["HBS pore density"]].getFinalValue() +
				matrix[sma["UO2"]].getPoreNucleationRate() * pow((sciantix_variable[sv["Xe atoms per HBS pore"]].getFinalValue()-2.0), 2.0),

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

	/**
	 * @brief This method returns a pointer to the array of diffusion modes corresponding to the specified gas.
	 * 
	 * @param gas_name The name of the gas for which diffusion modes are required.
	 * @return A pointer to the array of diffusion modes for the specified gas.
	 *         Returns nullptr if the gas name is invalid.
	 */
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

		else if (gas_name == "Kr85m")
			return &modes_initial_conditions[14 * 40];

		else if (gas_name == "Xe in HBS")
			return &modes_initial_conditions[17 * 40];
		else
		{
			std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModesBubbles." << std::endl;
			return nullptr;
		}
	}

	Simulation() {}
	~Simulation() {}
};

#endif
