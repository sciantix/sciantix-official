#include "InputReading.h"
#include <Global.h>

void InputReading()
{
    // Output file, where we write the results of this function
    std::ofstream input_check(TestPath + "input_check.txt", std::ios::out);

    // Input files
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




    // Read settings
    readSettings(input_settings, input_check);
    

    // Read inital conditions
    if (!input_initial_conditions.fail())
    {
        readParameters(input_initial_conditions, input_check);
    }

    // Read history
    int n = 0;
	while (!input_history.eof())
	{
        // Read history
		input_history >> Time_input[n];
		input_history >> Temperature_input[n];
		input_history >> Fissionrate_input[n];
		input_history >> Hydrostaticstress_input[n];
			
        // Write it in output file
		input_check << Time_input[n] << "\t"
		            << Temperature_input[n] << "\t"
		            << Fissionrate_input[n] << "\t"
		            << Hydrostaticstress_input[n] << "\t";

		if(Sciantix_options[20] > 0)
        {
            input_history >> Steampressure_input[n];
			input_check << Steampressure_input[n] << "\t";
        }

		input_check << std::endl;

		n++;
		Input_history_points = n;
	}

    // Resize arrays for memory usage
	Time_input.resize(Input_history_points);
	Temperature_input.resize(Input_history_points);
	Fissionrate_input.resize(Input_history_points);
	Hydrostaticstress_input.resize(Input_history_points);
	if(Sciantix_options[20] > 0)
		Steampressure_input.resize(Input_history_points);

	Time_end_h = Time_input[Input_history_points - 1];

    // Read scaling factors
    if (!input_scaling_factors.fail())
    {
        readParameters(input_scaling_factors, input_check);
    }
    else
    {
        // Default value
        for (int i = 0; i < 8; i++)
        {
            Sciantix_scaling_factors[i] = 1.0;
        }
    }


    // Close all files
    input_check.close();
	input_settings.close();
	input_initial_conditions.close();
	input_history.close();
	input_scaling_factors.close();
}


void readSettings(std::ifstream &input, std::ofstream &output)
{
    std::string line;
    int i = 0, comment_pos;
    unsigned short variable;

    /* 
        We read each line. 
        For each of them, we separate the value (integer) and its name (in the comment, after the '#').
        We them put the value in Sciantix_options and write it in the output file with its name.
    */
    while (std::getline(input, line))
    {
        comment_pos = line.find("#");
        variable = stoi(line.substr(0, comment_pos));
        Sciantix_options[i] = variable;
        output << line.substr(comment_pos + 2, line.find(" ", comment_pos + 2) - comment_pos-2) << " = " << variable << std::endl; 
    }
    
}


void readParameters(std::ifstream &input, std::ofstream &output)
{
    std::string line;
    std::vector<double> values;
    std::vector<std::string> names;

    while (getline(input, line))
    {
        if (line[0] == '#')
        {
            line = line.substr(2, line.size());

            int coma_pos = line.find(',');
            while (coma_pos != std::string::npos)
            {
                names.push_back(line.substr(0, coma_pos));
                line = line.substr(coma_pos+2, line.size());
                coma_pos = line.find(',');
            }
            if (line.find("(\\)") != std::string::npos) line = line.substr(0, line.find("(\\)"));
            names.push_back(line);
        }
        else 
        {
            std::stringstream line_stream(line);
            std::string current;
            while (line_stream >> current)
            {
                values.push_back(stod(current));
            }
        }
    }

    // Error if no correspondance between names and values
    if (values.size() < names.size()) {
        std::cerr << "Error in parameters reading : no correspondance between names and values" << std::endl; 
        exit(-1);
    }

    for (int i = 0; i < values.size(); i++)
    {
        output << names[i] << " = " << values[i] << std::endl;
        Sciantix_variables[i] = values[i];
    }
}