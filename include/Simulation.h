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
#include "FuelElementDeclaration.h"
#include "MapFuelElement.h"
#include "SciantixDiffusionModeDeclaration.h"
#include "SystemDeclaration.h"
#include "MapSystem.h"
#include "ConstantNumbers.h"

/// @brief
/// Derived class representing the operations of SCIANTIX. The conjunction of the models with the implemented solvers results in the simulation.

class Simulation : public Solver, public Model
{
	public:

	void UpdateGasGap()
	{
		double x(0);
		x = sciantix_variable[sv["He gap"]].getFinalValue();
		x = x + sciantix_variable[sv["H gap"]].getFinalValue();
		x = x + sciantix_variable[sv["Xe gap"]].getFinalValue();
		x = x + sciantix_variable[sv["Xe133 gap"]].getFinalValue();
		x = x + sciantix_variable[sv["Cs133 gap"]].getFinalValue();
		x = x + sciantix_variable[sv["Kr85m gap"]].getFinalValue();


		sciantix_variable[sv["Gas in gap"]].setFinalValue(x);
		// std::cout << "gas in gap" << std::endl;
		// std::cout << sciantix_variable[sv["Gas in gap"]].getFinalValue() << std::endl;
		// std::cout << "Xe in gap" << std::endl;
		// std::cout << sciantix_variable[sv["Xe gap"]].getFinalValue() << std::endl;
	}

	void CoolantRadiolysis()  //term of hydrogen production due to radiolysis evaluated by Lewis (to start) in at/m^3
	{

		sciantix_variable[sv["H gap"]].setFinalValue(
			solver.Decay(
					sciantix_variable[sv["H gap"]].getFinalValue(),
					gas[ga["H"]].getReleaseRateCoefficient(),
					sciantix_variable[sv["H production rate"]].getFinalValue(), 
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);
		//sciantix_variable[sv["H gap"]].setFinalValue(sciantix_variable[sv["H gap"]].getFinalValue() + sciantix_variable[sv["H production rate"]].getFinalValue()*physics_variable[pv["Time step"]].getFinalValue()); 
		
	}

	void ZircaloyOxidation() //term of hydrogen production due to zyrcaloy oxidation evaluated by Lewis (to start) in at/m^3
	{
		sciantix_variable[sv["H gap"]].setFinalValue(sciantix_variable[sv["H gap"]].getFinalValue() + 3.08*pow(10,20)*physics_variable[pv["Time step"]].getFinalValue());
	}

	void PressureEvolution() 
	{

		sciantix_variable[sv["Gap pressure"]].setFinalValue(
				sciantix_variable[sv["Gas in gap"]].getFinalValue()*PhysicsConstants::boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()*1e-06
			);
		std::cout << "pression" << std::endl;
		std::cout << sciantix_variable[sv["Gap pressure"]].getFinalValue() << std::endl;
		
		//sciantix_variable[sv["Gap pressure"]].setFinalValue(sciantix_variable[sv["H gap"]].getFinalValue()*PhysicsConstants::boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue()*1e-06); 
		
	}

	void GasDecay()
	{
		for (auto& system : sciantix_system)
		{
			if (gas[ga[system.getGasName()]].getDecayRate() > 0.0)
			{
				sciantix_variable[sv[system.getGasName() + " decayed"]].setFinalValue(
					solver.Decay(
						sciantix_variable[sv[system.getGasName() + " gap"]].getInitialValue(),
						gas[ga[system.getGasName()]].getDecayRate(),
						0,
						physics_variable[pv["Time step"]].getFinalValue()
					)
				);
			}
		}
	}

void GasInGapIntact() //this function computes the evolution of fission gas concentration in the gap (no hydrogen)
	{
		// dN / dt = q - (L+E)N
		sciantix_variable[sv["Xe gap"]].setFinalValue(
			solver.Decay(
					sciantix_variable[sv["Xe gap"]].getInitialValue(),
					gas[ga["Xe"]].getDecayRate(),
					history_variable[hv["Xe release rate from fuel"]].getFinalValue(), // release rate from the fuel
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);

		// dN / dt = q - (L+E)N  (for now E=0)
		sciantix_variable[sv["Xe133 gap"]].setFinalValue(
			solver.Decay(
					sciantix_variable[sv["Xe133 gap"]].getFinalValue(),
					gas[ga["Xe133"]].getDecayRate(),
					history_variable[hv["Xe133 release rate from fuel"]].getFinalValue(), // release rate from the fuel
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);

		// dN / dt = L133Xe*N133Xe - (L+E)N  (for now E=0)
		sciantix_variable[sv["Cs133 gap"]].setFinalValue(
			solver.Decay(
					sciantix_variable[sv["Cs133 gap"]].getFinalValue(),
					gas[ga["Cs133"]].getDecayRate(),
					gas[ga["Xe133"]].getDecayRate()*sciantix_variable[sv["Xe133 gap"]].getFinalValue(), // Xe133 decay
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);

		
		// dN / dt = q - (L+E)N  (for now E=0)
		sciantix_variable[sv["Kr85m gap"]].setFinalValue(
			solver.Decay(
					sciantix_variable[sv["Kr85m gap"]].getFinalValue(),
					gas[ga["Kr85m"]].getDecayRate(),
					history_variable[hv["Kr85m release rate from fuel"]].getFinalValue(), // release rate from the fuel
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);

		sciantix_variable[sv["He gap"]].setFinalValue(
			(sciantix_variable[sv["Gap pressure"]].getInitialValue()*1e06)/(PhysicsConstants::boltzmann_constant *history_variable[hv["Temperature"]].getFinalValue())
			);
	}


	void GasInGapFailed() //this function computes the evolution of fission gas concentration in the gap (no hydrogen)
	{
		// dN / dt = q - (L+E)N
		sciantix_variable[sv["Xe gap"]].setFinalValue(
			solver.Decay(
					sciantix_variable[sv["Xe gap"]].getInitialValue(),
					gas[ga["Xe"]].getReleaseRateCoefficient() + gas[ga["Xe"]].getDecayRate(),
					history_variable[hv["Xe release rate from fuel"]].getFinalValue(), // release rate from the fuel
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);

		// dN / dt = q - (L+E)N  (for now E=0)
		sciantix_variable[sv["Xe133 gap"]].setFinalValue(
			solver.Decay(
					sciantix_variable[sv["Xe133 gap"]].getFinalValue(),
					gas[ga["Xe133"]].getReleaseRateCoefficient() + gas[ga["Xe133"]].getDecayRate(),
					history_variable[hv["Xe133 release rate from fuel"]].getFinalValue(), // release rate from the fuel
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);

		// dN / dt = L133Xe*N133Xe - (L+E)N  (for now E=0)
		sciantix_variable[sv["Cs133 gap"]].setFinalValue(
			solver.Decay(
					sciantix_variable[sv["Cs133 gap"]].getFinalValue(),
					gas[ga["Cs133"]].getReleaseRateCoefficient() + gas[ga["Cs133"]].getDecayRate(),
					gas[ga["Xe133"]].getDecayRate()*sciantix_variable[sv["Xe133 gap"]].getFinalValue(), // Xe133 decay
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);

		
		// dN / dt = q - (L+E)N  (for now E=0)
		sciantix_variable[sv["Kr85m gap"]].setFinalValue(
			solver.Decay(
					sciantix_variable[sv["Kr85m gap"]].getFinalValue(),
					gas[ga["Kr85m"]].getReleaseRateCoefficient() + gas[ga["Kr85m"]].getDecayRate(),
					history_variable[hv["Kr85m release rate from fuel"]].getFinalValue(), // release rate from the fuel
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);

		sciantix_variable[sv["He gap"]].setFinalValue(
			solver.Decay(
					sciantix_variable[sv["He gap"]].getFinalValue(),
					gas[ga["He"]].getReleaseRateCoefficient(),
					0, 
					physics_variable[pv["Time step"]].getFinalValue()
				)
			);
	}

	void GasInCoolant() //release rate = nu*N_gap*Gap_volume
	{
		
		if (sciantix_variable[sv["Gap pressure"]].getFinalValue() > 4.0) //estimation of the limit gap presssure from Veschunov
		{
			//Xe
			sciantix_variable[sv["Xe released"]].setFinalValue(
					sciantix_variable[sv["Xe gap"]].getFinalValue()*gas[ga["Xe"]].getReleaseRateCoefficient()*sciantix_variable[sv["Gap volume"]].getFinalValue()	
			);

			//Xe133
			sciantix_variable[sv["Xe133 released"]].setFinalValue(
					sciantix_variable[sv["Xe133 gap"]].getFinalValue()*gas[ga["Xe133"]].getReleaseRateCoefficient()*sciantix_variable[sv["Gap volume"]].getFinalValue()	
			);

			//Cs133
			sciantix_variable[sv["Cs133 released"]].setFinalValue(
					sciantix_variable[sv["Cs133 gap"]].getFinalValue()*gas[ga["Cs133"]].getReleaseRateCoefficient()*sciantix_variable[sv["Gap volume"]].getFinalValue()	
			);

			//Kr85m
			sciantix_variable[sv["Kr85m released"]].setFinalValue(
					sciantix_variable[sv["Kr85m gap"]].getFinalValue()*gas[ga["Kr85m"]].getReleaseRateCoefficient()*sciantix_variable[sv["Gap volume"]].getFinalValue()	
			);

			//He
			sciantix_variable[sv["He released"]].setFinalValue(
					sciantix_variable[sv["He gap"]].getFinalValue()*gas[ga["He"]].getReleaseRateCoefficient()*sciantix_variable[sv["Gap volume"]].getFinalValue()	
			);

			//H
			sciantix_variable[sv["H released"]].setFinalValue(
					sciantix_variable[sv["H gap"]].getFinalValue()*gas[ga["H"]].getReleaseRateCoefficient()*sciantix_variable[sv["Gap volume"]].getFinalValue()	
			);

			
		}

	}

	Simulation() {}
	~Simulation() {}
};

#endif
