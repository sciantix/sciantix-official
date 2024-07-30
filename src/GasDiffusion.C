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
//////////////////////////////////////////////////////////////////////////////////////

#include "GasDiffusion.h"

void GasDiffusion()
{
	switch (static_cast<int>(input_variable[iv["iDiffusionSolver"]].getValue()))
	{
		case 1:  
			defineSpectralDiffusion1Equation();
			break;

		case 2: 
			defineSpectralDiffusion2Equations();
			break;

		case 3:
			defineSpectralDiffusion3Equations();
			break;
		
		case 9:
			defineRomCylinder();
			break;

		default:
			errorHandling();
			break;
	}
}

void defineSpectralDiffusion1Equation()
{
	std::string reference;

    for (auto& system : sciantix_system)
	{
		model.emplace_back();
		int modelIndex = static_cast<int>(model.size()) - 1;
		model[modelIndex].setName("Gas diffusion - " + system.getName());
		model[modelIndex].setRef(reference);

		std::vector<double> parameters;
		parameters.push_back(n_modes);
		double gasDiffusivity;
		if (system.getResolutionRate() + system.getTrappingRate() == 0)
		{
			gasDiffusivity = system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor();
		}
		else
		{
			gasDiffusivity = (system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate())) * system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor() +
							 (system.getTrappingRate() / (system.getResolutionRate() + system.getTrappingRate())) * system.getBubbleDiffusivity();
		}
		parameters.push_back(gasDiffusivity);
		parameters.push_back(matrix[sma[system.getMatrixName()]].getGrainRadius());
		parameters.push_back(system.getProductionRate());
		parameters.push_back(gas[ga[system.getGasName()]].getDecayRate());

		model[modelIndex].setParameter(parameters);
	}
}

void defineSpectralDiffusion2Equations()
{
	std::string reference;

    for (auto& system : sciantix_system)
	{
		model.emplace_back();
		int modelIndex = static_cast<int>(model.size()) - 1;
		model[modelIndex].setName("Gas diffusion - " + system.getName());
		model[modelIndex].setRef(reference);

		std::vector<double> parameters;

		parameters.push_back(n_modes);

		parameters.push_back(system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor());
		parameters.push_back(system.getBubbleDiffusivity());

		parameters.push_back(matrix[sma[system.getMatrixName()]].getGrainRadius());

		parameters.push_back(system.getProductionRate());
		parameters.push_back(0.0);
		
		parameters.push_back(system.getResolutionRate());
		parameters.push_back(system.getTrappingRate());
		parameters.push_back(gas[ga[system.getGasName()]].getDecayRate());

		model[modelIndex].setParameter(parameters);
	}
}

void defineSpectralDiffusion3Equations()
{
	std::string reference;

	model.emplace_back();
	int modelIndex = static_cast<int>(model.size()) - 1;
	model[modelIndex].setName("Gas diffusion - Xe in UO2 with HBS");
	model[modelIndex].setRef(reference);

	std::vector<double> parameters;

	parameters.push_back(n_modes);

	parameters.push_back(gas[ga["Xe"]].getPrecursorFactor() * sciantix_system[sy["Xe in UO2"]].getFissionGasDiffusivity() / (pow(matrix[sma["UO2"]].getGrainRadius(),2)));
	parameters.push_back(0.0);
	parameters.push_back(sciantix_system[sy["Xe in UO2HBS"]].getFissionGasDiffusivity() / (pow(matrix[sma["UO2HBS"]].getGrainRadius(),2)));
	
	parameters.push_back(1.0);
	
	parameters.push_back(sciantix_system[sy["Xe in UO2"]].getProductionRate());
	parameters.push_back(0.0);
	parameters.push_back(sciantix_system[sy["Xe in UO2HBS"]].getProductionRate());

	parameters.push_back(sciantix_system[sy["Xe in UO2"]].getResolutionRate());
	parameters.push_back(sciantix_system[sy["Xe in UO2"]].getTrappingRate());
	parameters.push_back(gas[ga["Xe"]].getDecayRate());

	double sweeping_term(0.0);
	if(physics_variable[pv["Time step"]].getFinalValue())
		sweeping_term = 1./(1. - sciantix_variable[sv["Restructured volume fraction"]].getFinalValue()) * sciantix_variable[sv["Restructured volume fraction"]].getIncrement() / physics_variable[pv["Time step"]].getFinalValue();

	if (std::isinf(sweeping_term) || std::isnan(sweeping_term))
		sweeping_term = 0.0;

	// exchange 1 --> 3
	parameters.push_back(sweeping_term);

	model[modelIndex].setParameter(parameters);
}

void defineRomCylinder()
{
	std::string reference;

    for (auto& system : sciantix_system)
	{
		model.emplace_back();
		int modelIndex = static_cast<int>(model.size()) - 1;
		model[modelIndex].setName("Gas diffusion - " + system.getName());
		model[modelIndex].setRef(reference);

		std::vector<double> parameters;
		parameters.push_back(n_modes); //0
		double gasDiffusivity; 
		if (system.getResolutionRate() + system.getTrappingRate() == 0)
		{
			gasDiffusivity = system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor();
		}
		else
		{
			gasDiffusivity = (system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate())) * system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor() +
							 (system.getTrappingRate() / (system.getResolutionRate() + system.getTrappingRate())) * system.getBubbleDiffusivity();
		}
		parameters.push_back(gasDiffusivity); //1
		parameters.push_back(matrix[sma[system.getMatrixName()]].getGrainRadius()); //2
		parameters.push_back(sciantix_variable[sv["Grain length"]].getFinalValue()); //3 MDG - da aggiungere in Matrix.h 
		parameters.push_back(system.getYield() * history_variable[hv["Fission rate"]].getFinalValue());//4 MDG source term
		std::cout<<"yield = " <<system.getYield() <<std::endl;

		parameters.push_back(sciantix_variable[sv["Thermal diffusivity"]].getFinalValue()); //5 MDG thermal diffusivity
		parameters.push_back(sciantix_variable[sv["Fission heat"]].getFinalValue()*history_variable[hv["Fission rate"]].getFinalValue()); //6 MDG heat source term
		parameters.push_back(history_variable[hv["Temperature"]].getFinalValue()); //7 MDG temperature bc
		parameters.push_back(system.getalphaD()); //8 MDG  alphaD
        //parameters.push_back(history_variable[hv["Temperature"]].getFinalValue());//T0
        parameters.push_back(sciantix_variable[sv["T0"]].getFinalValue());//9 MDG T0

		parameters.push_back(system.getProductionRate());
		parameters.push_back(gas[ga[system.getGasName()]].getDecayRate());

		model[modelIndex].setParameter(parameters);
	}
}


void errorHandling()
{
	ErrorMessages::Switch(__FILE__, "iDiffusionSolver", static_cast<int>(input_variable[iv["iDiffusionSolver"]].getValue()));
}
