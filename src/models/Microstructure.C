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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

void Simulation::Microstructure()
{
    // Model declaration
    Model model_;
    model_.setName("Microstructure");

	Matrix fuel_(matrices[0]);

    std::string reference;
    reference += "T. Cardinaels et al., JNM, 424 (2012) 252-260.";

    const double lattice_parameter_UO2 = 5.47109; 
	double lattice_parameter = (lattice_parameter_UO2-1.2*1e-6*(sciantix_variable["Chromium content"].getFinalValue()))*1e-10; // empirical correlation for the lattice parameter

	// routine to calculate the molar mass of Uranium
	double conv_fact = sciantix_variable["Fuel density"].getFinalValue() * avogadro_number *10 * 0.8815 * 100; // to move from atoms/m3 to percentage in atoms (see Initializatio.cpp)
	double molar_mass_Uranium = sciantix_variable["U234"].getFinalValue()/conv_fact *pow(234.04095,2)+ sciantix_variable["U235"].getFinalValue()/conv_fact *pow(235.04393,2)+  
								sciantix_variable["U236"].getFinalValue()/conv_fact *pow(236.04557,2)+ sciantix_variable["U237"].getFinalValue()/conv_fact *pow(237.04873,2)+  
								sciantix_variable["U238"].getFinalValue()/conv_fact *pow(238.05079,2);

	const double Cr_content = sciantix_variable["Chromium content"].getFinalValue()*1e-6*(molar_mass_Uranium + 2*molar_mass_Oxygen)/
					 		 (sciantix_variable["Chromium content"].getFinalValue()*1e-6*(molar_mass_Uranium-molar_mass_Chromium)+molar_mass_Uranium); //(mol/m3)

	double theoretical_density = (4*((1-Cr_content)*molar_mass_Uranium + Cr_content*molar_mass_Chromium + 2*molar_mass_Oxygen)/
								(avogadro_number*pow(lattice_parameter*1e2,3)))*1e3;	// (kg/m3)

	sciantix_variable["Lattice parameter"].setFinalValue(lattice_parameter);
	sciantix_variable["Theoretical density"].setFinalValue(theoretical_density);
	fuel_.setLatticeParameter(sciantix_variable, lattice_parameter);
	fuel_.setTheoreticalDensity(sciantix_variable, theoretical_density);
}