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

/*!
 * \mainpage SCIANTIX
 *
 * \section Introduction
 *
 * SCIANTIX is a 0D code developed at Politecnico di Milano.
 * The objective of SCIANTIX is to represent the behaviour of a single grain of nuclear fuel.
 * The modelling of inert gas behaviour is the main aspect considered.
 * Engineering models are used, allowing for future integration in industrial fuel performance codes.
 * Nevertheless, physically-based model are preferred to empirical models.
 * This facilitates the incorporation of information from lower length scale calculations.
 *
 *
 * At present, this version of the code is validated against experiments for
 * - intragranular gaseous swelling
 * - intergranular gaseous swelling
 * - helium behaviour and release in annealing conditions
 * - release of radioactive fission gases
 * The validation database is accessible in the *regression* folder.
 *
 */

#include "Sciantix.h"
#include "InputInterpolation.h"
#include "InputReading.h"
#include "Initialization.h"
#include "TimeStepCalculation.h"
#include "Global.h"
#include "ErrorMessages.h"
#include <iostream>
#include <fstream>
#include <ctime>

using namespace std;



/**
 * \brief Logs the execution time of the simulation.
 * @param timer The total execution time measured in seconds.
 * @param time_step_number The total number of time steps executed during the simulation.
 */
void logExecutionTime(double timer, int time_step_number, std::ofstream &Execution_file);



int main(int argc, char **argv)
{
	clock_t timer, timer_time_step;

	int Sciantix_options[40];
	double Sciantix_history[20];
	double Sciantix_variables[300];
	double Sciantix_scaling_factors[10];
	double Sciantix_diffusion_modes[1000];

	long long int Time_step_number(0);
	double  Time_h(0.0), dTime_h(0.0), Time_end_h(0.0); // (h)
	double  Time_s(0.0), Time_end_s(0.0); // (s)
	double  Number_of_time_steps_per_interval(100);

	std::ofstream Output_file;
	std::ofstream Execution_file;

	int Input_history_points(1000);
	int Temperature_input_points;
	int Fissionrate_input_points;
	int Hydrostaticstress_input_points;
	int Stempressure_input_points;
	std::vector<double> Time_temperature_input;
	std::vector<double> Time_fissionrate_input;
	std::vector<double> Time_hydrostaticstress_input;
	std::vector<double> Time_steampressure_input;
	std::vector<double> Time_input(1000, 0.0);
	std::vector<double> Temperature_input(1000, 0.0);
	std::vector<double> Fissionrate_input(1000, 0.0);
	std::vector<double> Hydrostaticstress_input(1000, 0.0);
	std::vector<double> Steampressure_input(1000, 0.0);


    if (argc < 2) {
        std::cerr << "Warning: No path specified, using current directory." << std::endl;
        std::cout << "Try to use ./sciantix.x ../regression/your-test-file-path/ in the bin directory" << std::endl;
        TestPath = "./";
    } else {
        TestPath = argv[1];
        if (TestPath.back() != '/') {
            TestPath += '/';
        }
    }


	InputReading(
		Sciantix_options, 
		Sciantix_variables, 
		Sciantix_scaling_factors,
		Input_history_points,
		Time_input, 
		Temperature_input,
		Fissionrate_input,
		Hydrostaticstress_input,
		Steampressure_input,
		Time_end_h,
		Time_end_s
	);

    Initialization(
		Sciantix_history,
		Sciantix_variables,
		Sciantix_diffusion_modes,
		Temperature_input,
		Fissionrate_input,
		Hydrostaticstress_input,
		Steampressure_input
	);

	std::string outputPath = TestPath + "output.txt";

	remove(outputPath.c_str());

	Execution_file.open(TestPath + "execution.txt", std::ios::out);

	timer = clock();


    while (Time_h <= Time_end_h)
    {
        Sciantix_history[0] = Sciantix_history[1];
		Sciantix_history[1] = InputInterpolation(Time_h, Time_input, Temperature_input, Input_history_points);
		Sciantix_history[2] = Sciantix_history[3];
		Sciantix_history[3] = InputInterpolation(Time_h, Time_input, Fissionrate_input, Input_history_points);
		if (Sciantix_history[3] < 0.0)
			Sciantix_history[3] = 0.0;
		Sciantix_history[4] = Sciantix_history[5];
		Sciantix_history[5] = InputInterpolation(Time_h, Time_input, Hydrostaticstress_input, Input_history_points);
		Sciantix_history[7] = Time_h;
		Sciantix_history[8] = static_cast<double>(Time_step_number);
		Sciantix_history[9] = Sciantix_history[10];
		Sciantix_history[10] = InputInterpolation(Time_h, Time_input, Steampressure_input, Input_history_points);


        Sciantix(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes);


        dTime_h = TimeStepCalculation(
			Input_history_points,
			Time_h,
			Time_input,
			Number_of_time_steps_per_interval
		);
		Sciantix_history[6] = dTime_h * 3600;

		// break;

		if (Time_h < Time_end_h)
		{
			Time_step_number++;
			Time_h += dTime_h;
			Time_s += Sciantix_history[6];
		}
		else
			break;
    }

    timer = clock() - timer;


	logExecutionTime((double)timer / CLOCKS_PER_SEC, Time_step_number, Execution_file);
	Execution_file.close();


	return 0;
}


void logExecutionTime(double timer, int time_step_number, std::ofstream &Execution_file)
{
	Execution_file << std::setprecision(12) << std::scientific << timer << "\t" << CLOCKS_PER_SEC << "\t" << (double)timer * CLOCKS_PER_SEC << "\t" << time_step_number << std::endl;
}