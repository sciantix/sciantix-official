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

#include "MainVariables.h"
#include "Sciantix.h"
#include "InputInterpolation.h"
#include "InputReading.h"
#include "Initialization.h"
#include "TimeStepCalculation.h"
#include <iostream>
#include <fstream>
#include <ctime>

void logExecutionTime(double timer, int time_step_number);

int main()
{
	InputReading();

	Initialization();

	remove("output.txt");

	Execution_file.open("execution.txt", std::ios::out);

	timer = clock();

	// std::cout << "france" << std::endl;
	std::cout << "history" << std::endl;
	std::cout << Sciantix_history[0] << std::endl;
	std::cout << Sciantix_history[1] << std::endl;
	std::cout << Sciantix_history[2] << std::endl;
	std::cout << Sciantix_history[3] << std::endl;
	std::cout << Sciantix_history[4] << std::endl;
	std::cout << Sciantix_history[5] << std::endl;
	std::cout << Sciantix_history[6] << std::endl;


	while (Time_h <= Time_end_h)
	{
		// temperature
		Sciantix_history[0] = Sciantix_history[1];
		Sciantix_history[1] = InputInterpolation(Time_h, Time_input, Temperature_input, Input_history_points);


		// release rate
		Sciantix_history[2] = Sciantix_history[3];
		Sciantix_history[3] = InputInterpolation(Time_h, Time_input, Release_rate_fuel_input, Input_history_points);

		Sciantix_history[4] = Time_h;
		Sciantix_history[5] = static_cast<double>(Time_step_number);

		Sciantix(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes);

		dTime_h = TimeStepCalculation();
		Sciantix_history[6] = dTime_h * 3600;

		if (Time_h < Time_end_h)
		{
			Time_step_number++;
			Time_h += dTime_h;
			Time_s += Sciantix_history[6];
		}
		else break;
	}

	timer = clock() - timer;

	logExecutionTime((double)timer / CLOCKS_PER_SEC, Time_step_number);
	Execution_file.close();

	return 0;
}


void logExecutionTime(double timer, int time_step_number)
{
	Execution_file << std::setprecision(12) << std::scientific << timer << "\t" << CLOCKS_PER_SEC << "\t" << (double)timer * CLOCKS_PER_SEC << "\t" << time_step_number << std::endl;
}
  