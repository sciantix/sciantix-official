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
//  Authors: D. Pizzocri, G. Zullo, G. Nicodemo                                     //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "ChromiumSolubility.h"

void ChromiumSolubility()
{	
	/**
	 * @brief This function defines the sciantix model *ChromiumSolubility*.
	 * 
	 * The model ChromiumSolubility is used to evaluate the cromium solubility accordingly to the temperature and the oxigen content.
	 * Then the number of oxide chromium atoms is evaluated, accordingly to the oxigen content of the system. 
	 * 
	 * Solubility of Chromium is evaluated as log10(y_Cr) = p*log10(P_O2) + V + U/T
	 * Coefficients U and V came from experimental fitting, in this way the Gibbs Potential is approximated as a function of 1/T
	 * Metallic chromium shows two different phases with threshold temperature of 1651 °C
	 * CrO is expected to be negligible, accordingly to Cr - O phase diagram
	 * The exchange between Cr-metal phase and Cr-oxide phase is described accordingly to the empirical expression:
	 * oxide_fraction  = 1 - exp(C1 * T - C1*C2)
	 * 
	 */

	model.emplace_back();

	int model_index = int(model.size()) - 1;

	model[model_index].setName("Chromium solubility");
	std::string reference;
	std::vector<double> parameter;
	
	reference += "Riglet-Martial et al., JNM, 447 (2014) 63-72.";

	double temperature = history_variable[hv["Temperature"]].getFinalValue();
	const double Oxigen_chemical_potential_0K = - 479070; //(J/mol)
	double Oxigen_chemical_potential_Cr2O3 = Oxigen_chemical_potential_0K + PhysicsConstants::calorie * temperature * (8.86*log10(temperature)-4.42 + 9.152*log10(2.4/100)); // (J/mol)
	double Oxigen_chemical_potential_Cr    = Oxigen_chemical_potential_0K + PhysicsConstants::calorie * temperature * (8.86*log10(temperature)-4.42 + 9.152*log10(1.017/100)); // (J/mol)
  	double log10_Oxigen_pressure_Cr203 = Oxigen_chemical_potential_Cr2O3/(PhysicsConstants::gas_constant*temperature*log(10));  // (bar)
	double log10_Oxigen_pressure_Cr	   = Oxigen_chemical_potential_Cr   /(PhysicsConstants::gas_constant*temperature*log(10));  // (bar)

	// Values for metal phase
	double V_coeff_Cr_ht = 0;
	double V_coeff_Cr_lt = 0;
	double U_coeff_Cr_ht = 0; 
	double U_coeff_Cr_lt = 0; 
	double threshold_temp = 0; 
	double end_temp = 0;

	double C1 = 0;
	double C2 = 0;

	switch (int (input_variable[iv["iChromiumSolubility"]].getValue()))
	{
		case 0:
		{
			/**
			 * Coefficient from solubility model by Riglet-Martial et al. (2014)
			 *
			 */

			V_coeff_Cr_ht = -8.391; 
			V_coeff_Cr_lt = -6.076; 
			U_coeff_Cr_ht =  2.816e+4; 
			U_coeff_Cr_lt =  2.370e+4; 

			threshold_temp = 1651+273.15; //(K)
			end_temp = 1500;

			C1 = 0.005;
			C2 = 1800 + 273.15;

			break;
		}

		case 1:
		{
			/**
			 * Optimized coeffients
			 *
			 */

			V_coeff_Cr_lt = -5.24961027667847;
			U_coeff_Cr_lt =  22256.321344636795;

			threshold_temp = 5000; //(K)
			end_temp = 1500 + 273.15;

			C1 = 0.004941175257332353;
			C2 = 1200.2686207610132;

			break;
		}
		

		default:
			ErrorMessages::Switch("ChromiumSolubility.cpp", "iChromiumSolubility", int (input_variable[iv["iChromiumSolubility"]].getValue()));
			break;
	}

	// Values for oxide phase
	const double V_coeff_Cr2O3 =  0.1385; 
	const double U_coeff_Cr2O3 = -0.4827e+4;  
	const double p_stoichiometry_Cr = 0.75;
	const double p_stoichiometry_Cr2O3 = 0;

	double Cr_solubility; 

	// routine to calculate the molar mass of Uranium
	double conv_fact = sciantix_variable[sv["Fuel density"]].getFinalValue() * PhysicsConstants::avogadro_number * 10 * 0.8815 * 100; // to move from atoms/m3 to percentage in atoms (see Initialization.cpp)
	double molar_mass_Uranium = sciantix_variable[sv["U234"]].getFinalValue()/conv_fact *pow(234.04095,2)+ sciantix_variable[sv["U235"]].getFinalValue()/conv_fact *pow(235.04393,2)+  
								sciantix_variable[sv["U236"]].getFinalValue()/conv_fact *pow(236.04557,2)+ sciantix_variable[sv["U237"]].getFinalValue()/conv_fact *pow(237.04873,2)+  
								sciantix_variable[sv["U238"]].getFinalValue()/conv_fact *pow(238.05079,2);

	if (temperature < threshold_temp) 
		{
    	Cr_solubility = pow(10, p_stoichiometry_Cr*log10_Oxigen_pressure_Cr + V_coeff_Cr_lt + U_coeff_Cr_lt/ temperature); //(molar Cr solubility)
		Cr_solubility = 100*Cr_solubility * PhysicsConstants::molar_mass_Chromium / ((1-Cr_solubility)*molar_mass_Uranium + Cr_solubility * PhysicsConstants::molar_mass_Chromium + 2* PhysicsConstants::molar_mass_Oxigen ); //(% weight Cr /UO2)
		}
	else
		{
    	Cr_solubility = pow(10, p_stoichiometry_Cr*log10_Oxigen_pressure_Cr + V_coeff_Cr_ht + U_coeff_Cr_ht/ temperature); //(molar Cr solubility)
		Cr_solubility = 100*Cr_solubility * PhysicsConstants::molar_mass_Chromium / ((1-Cr_solubility)*molar_mass_Uranium + Cr_solubility * PhysicsConstants::molar_mass_Chromium + 2* PhysicsConstants::molar_mass_Oxigen ); //(% weight Cr /UO2)
		}

	double Cr2O3_solubility = pow(10, p_stoichiometry_Cr2O3*log10_Oxigen_pressure_Cr203 + V_coeff_Cr2O3 + U_coeff_Cr2O3/ temperature); //(molar Cr solubility)
	Cr2O3_solubility = 100*Cr2O3_solubility * PhysicsConstants::molar_mass_Chromium / ((1-Cr2O3_solubility)*molar_mass_Uranium + Cr2O3_solubility * PhysicsConstants::molar_mass_Chromium + 2* PhysicsConstants::molar_mass_Oxigen ); //(% weight Cr /UO2)

	if (temperature < end_temp)
	{
		Cr_solubility = 0.1051692;
	}
	if (Cr2O3_solubility < 0)
		Cr2O3_solubility = 0;

	// Calculations to get the amount of Cr in weight %
	double U_content = sciantix_variable[sv["U234"]].getFinalValue() + sciantix_variable[sv["U235"]].getFinalValue() +sciantix_variable[sv["U236"]].getFinalValue() +
					   sciantix_variable[sv["U237"]].getFinalValue() + sciantix_variable[sv["U238"]].getFinalValue(); // (at U/m3)

	double U_weight = U_content*molar_mass_Uranium/PhysicsConstants::avogadro_number; //(g U/m3)
	double O2_weight = U_content*2*PhysicsConstants::molar_mass_Oxigen/PhysicsConstants::avogadro_number; //(g O2/m3)
	double UO2_weight = U_weight + O2_weight; //(g UO2/m3)

	double Cr_weight = UO2_weight*sciantix_variable[sv["Chromium content"]].getFinalValue()*1e-6; //(g Cr/m3)
	double Cr_atoms = Cr_weight*PhysicsConstants::avogadro_number/PhysicsConstants::molar_mass_Chromium;

	// moving from weight% to atoms/m3

	double Cr_solution = 0; //(Cr atoms/m3)
	double Cr_precipitate = 0; //(Cr atoms/m3)
	double Cr203_solution = 0; //(Cr atoms/m3)
	double Cr2O3_precipitate = 0; //(Cr atoms/m3)

	double Cr_solubility_atoms    = UO2_weight*Cr_solubility   /100/PhysicsConstants::molar_mass_Chromium * PhysicsConstants::avogadro_number; // atoms Cr/m3
	double Cr2O3_solubility_atoms = UO2_weight*Cr2O3_solubility/100/PhysicsConstants::molar_mass_Chromium * PhysicsConstants::avogadro_number; // atoms Cr/m3	

	// Ration between Cr203 and Cr-metal in the fuel 

	double Cr203_fraction = 1 - exp(C1*(temperature) - C1*C2);

	if (sciantix_variable[sv["Burnup"]].getFinalValue() <= 30)
		Cr203_fraction = 1 - exp(C1*((temperature) - C2 + 2000*(sciantix_variable[sv["Burnup"]].getFinalValue()-30)/30));

	if (Cr203_fraction < 0)
	{
		Cr203_fraction = 0;
	}

	double Cr_atoms_oxide = Cr_atoms*Cr203_fraction;
	double Cr_atoms_metal = Cr_atoms - Cr_atoms_oxide;

	// Routine to set the amount of chromium in solution/precipitate for each phase

	if (Cr_solubility_atoms >= Cr_atoms_metal)
	{
		Cr_solution = Cr_atoms_metal;
		Cr_precipitate = 0;
	}		
	else 
	{
		Cr_solution = Cr_solubility_atoms;
		Cr_precipitate = Cr_atoms_metal - Cr_solution;
	}
		
	if (Cr2O3_solubility_atoms >= Cr_atoms_oxide)
	{
		Cr203_solution = Cr_atoms_oxide;
		Cr2O3_precipitate = 0;
	}	
	else
	{
		Cr203_solution = Cr2O3_solubility_atoms;
		Cr2O3_precipitate = Cr_atoms_oxide - Cr203_solution;
	}
		
	parameter.push_back(Cr_precipitate);
	parameter.push_back(Cr_solution);
	parameter.push_back(Cr2O3_precipitate);
	parameter.push_back(Cr203_solution);

	model[model_index].setParameter(parameter);

	sciantix_variable[sv["Chromium solubility"]].setFinalValue(Cr_solubility);
	sciantix_variable[sv["Chromia solubility"]].setFinalValue(Cr2O3_solubility);
	sciantix_variable[sv["Chromium solution"]].setFinalValue(Cr_solution);
	sciantix_variable[sv["Chromium precipitate"]].setFinalValue(Cr_precipitate);
	sciantix_variable[sv["Chromia solution"]].setFinalValue(Cr203_solution);
	sciantix_variable[sv["Chromia precipitate"]].setFinalValue(Cr2O3_precipitate);

	matrix[0].setChromiumSolubility(Cr_solubility);
	matrix[0].setChromiaSolubility(Cr2O3_solubility);
	matrix[0].setChromiumSolution(Cr_solution);
	matrix[0].setChromiumPrecipitate(Cr_precipitate);
	matrix[0].setChromiaSolution(Cr203_solution);
	matrix[0].setChromiaPrecipitate(Cr2O3_precipitate);

	model[model_index].setRef(reference);
}