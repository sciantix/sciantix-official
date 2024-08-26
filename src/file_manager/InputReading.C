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
//  Year: 2023                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "InputReading.h"

/**
 * @brief Read a single setting from the input file.
 * @param variable_name The name of the variable to be read.
 * @param input_file Input file stream from which the setting is read.
 * @param output_file Output file stream where the read setting is logged.
 * @return An unsigned short int containing the value of the setting.
 */
unsigned short int ReadOneSetting(std::string variable_name, std::ifstream& input_file, std::ofstream& output_file)
{
	char comment;
	unsigned short int variable;
	input_file >> variable;
	input_file >> comment;
	if (comment == '#') input_file.ignore(256, '\n');
	output_file << variable_name << " = " << variable << std::endl;
	return variable;
}

/**
 * @brief Read a single parameter from the input file.
 * @param variable_name The name of the parameter to be read.
 * @param input_file Input file stream from which the parameter is read.
 * @param output_file Output file stream where the read parameter is logged.
 * @return A double containing the value of the parameter.
 */
double ReadOneParameter(std::string variable_name, std::ifstream& input_file, std::ofstream& output_file)
{
	char comment;
	double variable;
	input_file >> variable;
	input_file >> comment;
	if (comment == '#') input_file.ignore(256, '\n');
	output_file << variable_name << " = " << variable << std::endl;
	return variable;
}

/**
 * @brief Read several parameters from a single line in the input file.
 * @param variable_name The name for the parameters to be logged.
 * @param input_file Input file stream from which the parameters are read.
 * @param output_file Output file stream where the read parameters are logged.
 * @return A vector of doubles containing the parameters read from the file.
 */
std::vector<double> ReadSeveralParameters(std::string variable_name, std::ifstream& input_file, std::ofstream& output_file)
{
	char comment;
	double variable;
	short int K(0);

	std::vector<double> vector_read;
	std::string timestring("");
	std::getline(input_file, timestring);
	std::istringstream timestream(timestring);

	while (timestream >> variable)
	{
		vector_read.push_back(variable);
		output_file << variable_name << K << " = " << vector_read[K] << std::endl;
		++K;
	}

	input_file >> comment;

	if (comment == '#') input_file.ignore(256, '\n');

	return vector_read;
}

void InputReading(
	int Sciantix_options[], 
	double Sciantix_variables[], 
	double Sciantix_scaling_factors[],
	int &Input_history_points,
	std::vector<double> &Time_input, 
	std::vector<double> &Temperature_input,
	std::vector<double> &Fissionrate_input,
	std::vector<double> &Hydrostaticstress_input,
	std::vector<double> &Steampressure_input,
	double &Time_end_h,
	double &Time_end_s
	)
{
    /**
     * Besides the two input files, this routines creates an input_check.txt file
     * reporting all the inputs provided in the other files.
     * It is highly recommended checking this file, since eventual errors
     * are reported in it.
     */

	std::ofstream input_check(TestPath + "input_check.txt", std::ios::out);

	// Abort execution if any of the input files does not exist
	std::ifstream input_settings(TestPath + "input_settings.txt", std::ios::in);
	if (!input_settings)
		ErrorMessages::MissingInputFile("input_settings.txt");

	std::ifstream input_initial_conditions(TestPath + "input_initial_conditions.txt", std::ios::in);
	if (!input_initial_conditions)
		ErrorMessages::MissingInputFile("input_initial_conditions.txt");

	std::ifstream input_history(TestPath + "input_history.txt", std::ios::in);
	if (!input_history)
		ErrorMessages::MissingInputFile("input_history.txt");
	
	// This is optional so no error if not present
	std::ifstream input_scaling_factors(TestPath + "input_scaling_factors.txt", std::ios::in);

	Sciantix_options[0] = ReadOneSetting("iGrainGrowth", input_settings, input_check);
	Sciantix_options[1] = ReadOneSetting("iFissionGasDiffusivity", input_settings, input_check);
	Sciantix_options[2] = ReadOneSetting("iDiffusionSolver", input_settings, input_check);
	Sciantix_options[3] = ReadOneSetting("iIntraGranularBubbleBehavior", input_settings, input_check);
	Sciantix_options[4] = ReadOneSetting("iResolutionRate", input_settings, input_check);
	Sciantix_options[5] = ReadOneSetting("iTrappingRate", input_settings, input_check);
	Sciantix_options[6] = ReadOneSetting("iNucleationRate", input_settings, input_check);
	Sciantix_options[7] = ReadOneSetting("iOutput", input_settings, input_check);
	Sciantix_options[8] = ReadOneSetting("iGrainBoundaryVacancyDiffusivity", input_settings, input_check);
	Sciantix_options[9] = ReadOneSetting("iGrainBoundaryBehaviour", input_settings, input_check);
	Sciantix_options[10] = ReadOneSetting("iGrainBoundaryMicroCracking", input_settings, input_check);
	Sciantix_options[11] = ReadOneSetting("iFuelMatrix", input_settings, input_check);
	Sciantix_options[12] = ReadOneSetting("iGrainBoundaryVenting", input_settings, input_check);
	Sciantix_options[13] = ReadOneSetting("iRadioactiveFissionGas", input_settings, input_check);
	Sciantix_options[14] = ReadOneSetting("iHelium", input_settings, input_check);
	Sciantix_options[15] = ReadOneSetting("iHeDiffusivity", input_settings, input_check);
	Sciantix_options[16] = ReadOneSetting("iGrainBoundarySweeping", input_settings, input_check);
	Sciantix_options[17] = ReadOneSetting("iHighBurnupStructureFormation", input_settings, input_check);
	Sciantix_options[18] = ReadOneSetting("iHighBurnupStructurePorosity", input_settings, input_check);
	Sciantix_options[19] = ReadOneSetting("iHeliumProductionRate", input_settings, input_check);
	Sciantix_options[20] = ReadOneSetting("iStoichiometryDeviation", input_settings, input_check);
	Sciantix_options[21] = ReadOneSetting("iBubbleDiffusivity",input_settings,input_check);
	
	if (!input_initial_conditions.fail())
	{
		Sciantix_variables[0] = ReadOneParameter("Grain radius[0]", input_initial_conditions, input_check);

		std::vector<double> initial_composition_Xe;
		initial_composition_Xe = ReadSeveralParameters("Initial composition Xe", input_initial_conditions, input_check);
		Sciantix_variables[1] = initial_composition_Xe[0];
		Sciantix_variables[2] = initial_composition_Xe[1];
		Sciantix_variables[3] = initial_composition_Xe[2];
		Sciantix_variables[4] = initial_composition_Xe[3];
		Sciantix_variables[5] = initial_composition_Xe[4];
		Sciantix_variables[6] = initial_composition_Xe[5];

		std::vector<double> initial_composition_Kr;
		initial_composition_Kr = ReadSeveralParameters("Initial composition Kr", input_initial_conditions, input_check);
		Sciantix_variables[7] = initial_composition_Kr[0];
		Sciantix_variables[8] = initial_composition_Kr[1];
		Sciantix_variables[9] = initial_composition_Kr[2];
		Sciantix_variables[10] = initial_composition_Kr[3];
		Sciantix_variables[11] = initial_composition_Kr[4];
		Sciantix_variables[12] = initial_composition_Kr[5];

		std::vector<double> initial_composition_He;
		initial_composition_He = ReadSeveralParameters("Initial composition He", input_initial_conditions, input_check);
		Sciantix_variables[13] = initial_composition_He[0];
		Sciantix_variables[14] = initial_composition_He[1];
		Sciantix_variables[15] = initial_composition_He[2];
		Sciantix_variables[16] = initial_composition_He[3];
		Sciantix_variables[17] = initial_composition_He[4];
		Sciantix_variables[18] = initial_composition_He[5];

		std::vector<double> initial_intragranular_bubbles;
		initial_intragranular_bubbles = ReadSeveralParameters("Initial intragranular bubbles", input_initial_conditions, input_check);
		Sciantix_variables[19] = initial_intragranular_bubbles[0];
		Sciantix_variables[20] = initial_intragranular_bubbles[1];

		Sciantix_variables[38] = ReadOneParameter("Burn_up[0]", input_initial_conditions, input_check);
		Sciantix_variables[39] = ReadOneParameter("Effective_burn_up[0]", input_initial_conditions, input_check);
		Sciantix_variables[65] = ReadOneParameter("Irradiation_time[0]", input_initial_conditions, input_check);
		Sciantix_variables[40] = ReadOneParameter("Fuel_density[0]", input_initial_conditions, input_check);

		std::vector<double> initial_composition_U;
		initial_composition_U = ReadSeveralParameters("Initial composition U", input_initial_conditions, input_check);

		Sciantix_variables[41] = initial_composition_U[0]; // U-234
		Sciantix_variables[42] = initial_composition_U[1]; // U-235
		Sciantix_variables[43] = initial_composition_U[2]; // U-236
		Sciantix_variables[44] = initial_composition_U[3]; // U-237
		Sciantix_variables[45] = initial_composition_U[4]; // U-238

		std::vector<double> initial_composition_Xe133;
		initial_composition_Xe133 = ReadSeveralParameters("Initial composition Xe133", input_initial_conditions, input_check);

		Sciantix_variables[48] = initial_composition_Xe133[0];
		Sciantix_variables[49] = initial_composition_Xe133[1];
		Sciantix_variables[50] = initial_composition_Xe133[2];
		Sciantix_variables[51] = initial_composition_Xe133[3];
		Sciantix_variables[52] = initial_composition_Xe133[4];
		Sciantix_variables[53] = initial_composition_Xe133[5];
		Sciantix_variables[54] = initial_composition_Xe133[6];

		std::vector<double> initial_composition_Kr85m;
		initial_composition_Kr85m = ReadSeveralParameters("Initial composition Kr85m", input_initial_conditions, input_check);

		Sciantix_variables[57] = initial_composition_Kr85m[0];
		Sciantix_variables[58] = initial_composition_Kr85m[1];
		Sciantix_variables[59] = initial_composition_Kr85m[2];
		Sciantix_variables[60] = initial_composition_Kr85m[3];
		Sciantix_variables[61] = initial_composition_Kr85m[4];
		Sciantix_variables[62] = initial_composition_Kr85m[5];
		Sciantix_variables[63] = initial_composition_Kr85m[6];

		Sciantix_variables[66] = ReadOneParameter("Initial stoichiometry deviation[0]", input_initial_conditions, input_check);
	}

	int n = 0;
	while (!input_history.eof())
	{
		input_history >> Time_input[n];
		input_history >> Temperature_input[n];
		input_history >> Fissionrate_input[n];
		input_history >> Hydrostaticstress_input[n];

		if(Sciantix_options[20] > 0)
			input_history >> Steampressure_input[n];

		input_check << Time_input[n] << "\t";
		input_check << Temperature_input[n] << "\t";
		input_check << Fissionrate_input[n] << "\t";
		input_check << Hydrostaticstress_input[n] << "\t";

		if(Sciantix_options[20] > 0)
			input_check << Steampressure_input[n] << "\t";

		input_check << std::endl;

		n++;
		Input_history_points = n;
	}

	Time_input.resize(Input_history_points);
	Temperature_input.resize(Input_history_points);
	Fissionrate_input.resize(Input_history_points);
	Hydrostaticstress_input.resize(Input_history_points);
		
	if(Sciantix_options[20] > 0)
		Steampressure_input.resize(Input_history_points);

	Time_end_h = Time_input[Input_history_points - 1];
	Time_end_s = Time_end_h * 3600.0;

	if (!input_scaling_factors.fail())
	{
		Sciantix_scaling_factors[0] = ReadOneParameter("sf_resolution_rate", input_scaling_factors, input_check);
		Sciantix_scaling_factors[1] = ReadOneParameter("sf_trapping_rate", input_scaling_factors, input_check);
		Sciantix_scaling_factors[2] = ReadOneParameter("sf_nucleation_rate", input_scaling_factors, input_check);
		Sciantix_scaling_factors[3] = ReadOneParameter("sf_diffusivity", input_scaling_factors, input_check);
		Sciantix_scaling_factors[4] = ReadOneParameter("sf_temperature", input_scaling_factors, input_check);
		Sciantix_scaling_factors[5] = ReadOneParameter("sf_fission_rate", input_scaling_factors, input_check);
		Sciantix_scaling_factors[6] = ReadOneParameter("sf_cent_parameter", input_scaling_factors, input_check);
		Sciantix_scaling_factors[7] = ReadOneParameter("sf_helium_production_rate", input_scaling_factors, input_check);
		Sciantix_scaling_factors[8] = ReadOneParameter("sf_dummy", input_scaling_factors, input_check);
	}
	else
	{
		Sciantix_scaling_factors[0] = 1.0;
		Sciantix_scaling_factors[1] = 1.0;
		Sciantix_scaling_factors[2] = 1.0;
		Sciantix_scaling_factors[3] = 1.0;
		Sciantix_scaling_factors[4] = 1.0;
		Sciantix_scaling_factors[5] = 1.0;
		Sciantix_scaling_factors[6] = 1.0;
		Sciantix_scaling_factors[7] = 1.0;
		Sciantix_scaling_factors[8] = 1.0;
	}

	input_check.close();
	input_settings.close();
	input_initial_conditions.close();
	input_history.close();
	input_scaling_factors.close();
}