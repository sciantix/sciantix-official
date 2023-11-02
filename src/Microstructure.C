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

#include "Microstructure.h"

void Microstructure()
{	
	/**
	 * @brief This function defines the sciantix model *Microstructure*.
	 * 
	 * The model Microstructure is used to evaluate the lattice parameter and the theoretical density, accordingly to cromium content.
	 * 
	 * 
	 */

	model.emplace_back();

	int model_index = int(model.size()) - 1;

	model[model_index].setName("Microstructure");
	std::string reference;
	std::vector<double> parameter;
	
	reference += "T. Cardinaels et al., JNM, 424 (2012) 252-260.";

	const double lattice_parameter_UO2 = 5.47109; 
	double lattice_parameter = (lattice_parameter_UO2-1.2*1e-6*(sciantix_variable[sv["Cr_content"]].getFinalValue()))*1e-10; // empirical correlation for the lattice parameter

	// routine to calculate the molar mass of Uranium
	double conv_fact = sciantix_variable[sv["Fuel density"]].getFinalValue() * PhysicsConstants::avogadro_number *10 * 0.8815 * 100; // to move from atoms/m3 to percentage in atoms (see Initializatio.cpp)
	double molar_mass_Uranium = sciantix_variable[sv["U234"]].getFinalValue()/conv_fact *pow(234.04095,2)+ sciantix_variable[sv["U235"]].getFinalValue()/conv_fact *pow(235.04393,2)+  
								sciantix_variable[sv["U236"]].getFinalValue()/conv_fact *pow(236.04557,2)+ sciantix_variable[sv["U237"]].getFinalValue()/conv_fact *pow(237.04873,2)+  
								sciantix_variable[sv["U238"]].getFinalValue()/conv_fact *pow(238.05079,2);

	const double Cr_content = sciantix_variable[sv["Chromium content"]].getFinalValue()*1e-6*(molar_mass_Uranium + 2*PhysicsConstants::molar_mass_Oxigen)/
					 		 (sciantix_variable[sv["Chromium content"]].getFinalValue()*1e-6*(molar_mass_Uranium-PhysicsConstants::molar_mass_Chromium)+molar_mass_Uranium); //(mol/m3)

	double theoretical_density = (4*((1-Cr_content)*molar_mass_Uranium + Cr_content*PhysicsConstants::molar_mass_Chromium + 2*PhysicsConstants::molar_mass_Oxigen)/
								(PhysicsConstants::avogadro_number*pow(lattice_parameter*1e2,3)))*1e3;	// (kg/m3)

	parameter.push_back(lattice_parameter);
	parameter.push_back(theoretical_density);
	
	model[model_index].setParameter(parameter);
	model[model_index].setRef(reference);
	sciantix_variable[sv["Lattice parameter"]].setFinalValue(lattice_parameter);
	sciantix_variable[sv["Theoretical density"]].setFinalValue(theoretical_density);
	matrix[0].setLatticeParameter(lattice_parameter);
	matrix[0].setTheoreticalDensity(theoretical_density);

}

