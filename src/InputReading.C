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
//  Authors: D. Pizzocri, T. Barani.                                                //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

/// InputReading
/// This routine reads the input files.
/// Sciantix requires three input files:
/// (1) input_settings.txt
/// (2) input_history.txt.
/// (3) input_initial_conditions.txt
/// The first contains all the model selection variables, whereas the second contains temperature, fission rate and hydrostatic stress as a function of time.
/// The third file contains the initial conditions for the physics variables.

#include "InputReading.h"

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

void InputReading()
{
	/// Besides the two input files, this routines creates an input_check.txt file
	/// reporting all the inputs provided in the other files.
	/// It is highly recommended checking this file, since eventual errors
	/// are reported in it.

	std::ofstream input_check("input_check.txt", std::ios::out);

	// Abort execution if any of the input files does not exist
	std::ifstream input_settings("input_settings.txt", std::ios::in);
	if (!input_settings)
		ErrorMessages::MissingInputFile("input_settings.txt");

	std::ifstream input_initial_conditions("input_initial_conditions.txt", std::ios::in);
	if (!input_initial_conditions)
		ErrorMessages::MissingInputFile("input_initial_conditions.txt");

	std::ifstream input_history("input_history.txt", std::ios::in);
	if (!input_history)
		ErrorMessages::MissingInputFile("input_history.txt");
	
	// This is optional so no error if not present
	std::ifstream input_scaling_factors("input_scaling_factors.txt", std::ios::in);

	Sciantix_options[0] = ReadOneSetting("iOutput", input_settings, input_check);
	Sciantix_options[1] = ReadOneSetting("iOption2", input_settings, input_check);
	Sciantix_options[2] = ReadOneSetting("iOption3", input_settings, input_check);

	if (!input_initial_conditions.fail())
	{
		std::vector<double> initial_conditions;
		initial_conditions = ReadSeveralParameters("Initial conditions", input_initial_conditions, input_check);
		Sciantix_variables[1] = initial_conditions[0]; //Xe gap
		Sciantix_variables[2] = initial_conditions[1]; //Xe decayed
		Sciantix_variables[3] = initial_conditions[2]; //Xe released
		
		Sciantix_variables[4] = initial_conditions[3]; //release to coolant
		
		Sciantix_variables[5] = initial_conditions[4]; //initial gap pressure
		Sciantix_variables[6] = initial_conditions[4];
		std::cout << "pression initiale" << std::endl;
		std::cout << initial_conditions[4] << std::endl;
		std::cout << "pression initiale normalement" << std::endl;
		std::cout << Sciantix_variables[6] << std::endl;
		
		Sciantix_variables[7] = initial_conditions[5]; //gap volume
		
		Sciantix_variables[9] = initial_conditions[6]; //H gap
		
	}

	int n = 0;
	while (!input_history.eof())
	{
		input_history >> Time_input[n];
		input_history >> Temperature_input[n];
		input_history >> Xe_Release_rate_fuel_input[n];
		input_history >> Xe133_Release_rate_fuel_input[n];
		input_history >> Kr85m_Release_rate_fuel_input[n];

		input_check << Time_input[n] << "\t";
		input_check << Temperature_input[n] << "\t";
		input_check << Xe_Release_rate_fuel_input[n] << "\t";
		input_check << Xe133_Release_rate_fuel_input[n] << "\t";
		input_check << Kr85m_Release_rate_fuel_input[n] << "\t";

		input_check << std::endl;

		n++;
		Input_history_points = n;
	}

	Time_input.resize(Input_history_points);
	Temperature_input.resize(Input_history_points);
	Xe_Release_rate_fuel_input.resize(Input_history_points);
	Xe133_Release_rate_fuel_input.resize(Input_history_points);
	Kr85m_Release_rate_fuel_input.resize(Input_history_points);
	
	Time_end_h = Time_input[Input_history_points - 1];
	Time_end_s = Time_end_h * 3600.0;


	// std::cout << Time_end_h << std::endl;

	Sciantix_scaling_factors[0] = 1.0;
	Sciantix_scaling_factors[1] = 1.0;
	Sciantix_scaling_factors[2] = 1.0;
	Sciantix_scaling_factors[3] = 1.0;
	Sciantix_scaling_factors[4] = 1.0;
	Sciantix_scaling_factors[5] = 1.0;
	Sciantix_scaling_factors[6] = 1.0;
	Sciantix_scaling_factors[7] = 1.0;
	Sciantix_scaling_factors[8] = 1.0;

	input_check.close();
	input_settings.close();
	input_initial_conditions.close();
	input_history.close();
	input_scaling_factors.close();
}
