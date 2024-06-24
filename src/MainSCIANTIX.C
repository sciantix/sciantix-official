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

     //   Print history settings
    std::cout << "History Variables:\n";
    for(int i = 0; i < 20; ++i) { // Assuming there are 11 history variables
        std::cout << "History " << i << ": " << Sciantix_history[i] << std::endl;
    }

	remove("output.txt");

	Execution_file.open("execution.txt", std::ios::out);

	timer = clock();

	while (Time_h <= Time_end_h)
	{
		Sciantix_history[0] = Sciantix_history[1];
		Sciantix_history[1] = InputInterpolation(Time_h, Time_input, Temperature_input, Input_history_points);
		Sciantix_history[2] = Sciantix_history[3];
		Sciantix_history[3] = InputInterpolation(Time_h, Time_input, Fissionrate_input, Input_history_points);
		if (Sciantix_history[3] < 0.0) Sciantix_history[3] = 0.0;
		Sciantix_history[4] = Sciantix_history[5];
		Sciantix_history[5] = InputInterpolation(Time_h, Time_input, Hydrostaticstress_input, Input_history_points);
		Sciantix_history[7] = Time_h;
		Sciantix_history[8] = static_cast<double>(Time_step_number);
		Sciantix_history[9] = Sciantix_history[10];
		Sciantix_history[10] = InputInterpolation(Time_h, Time_input, Steampressure_input, Input_history_points);

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

	// // Print variables
    // std::cout << "Sciantix Variables à la fin :\n";
    // for(int i = 0; i < 300; ++i) { 
    //     std::cout << "Variable " << i << ": " << Sciantix_variables[i] << std::endl;
    // }

    // std::cout << "Sciantix_history à la fin :\n";
    // for(int i = 0; i < 20; ++i) { 
    //     std::cout << "Variable " << i << ": " << Sciantix_history[i] << std::endl;
    // }

    // std::cout << "Sciantix_diffusion_modes à la fin :\n";
    // for(int i = 0; i < 1000; ++i) { 
    //     std::cout << "Variable " << i << ": " << Sciantix_diffusion_modes[i] << std::endl;
    // }

    // std::cout << "Sciantix_options à la fin :\n";
    // for(int i = 0; i < 40; ++i) { 
    //     std::cout << "Variable " << i << ": " << Sciantix_options[i] << std::endl;
    // }

    // std::cout << "Sciantix_scaling_factors à la fin :\n";
    // for(int i = 0; i < 10; ++i) { 
    //     std::cout << "Variable " << i << ": " << Sciantix_scaling_factors[i] << std::endl;
    // }

    // std::cout << "Time_step_number à la fin : " << Time_step_number << std::endl;
    // std::cout << "Time_h à la fin : " << Time_h << std::endl;
    // std::cout << "dTime_h à la fin : " << dTime_h << std::endl;
    // std::cout << "Time_end_h à la fin : " << Time_end_h << std::endl;
    // std::cout << "Time_s à la fin : " << Time_s << std::endl;

    // std::cout << "input_history_Point " << Input_history_points << std::endl;

    // std::cout << "Temperature_input_points à la fin :\n";
    // for(int i = 0; i < Temperature_input.size(); ++i) { 
    //     std::cout << "Point " << i << ": " << Temperature_input[i] << std::endl;
    // }

    // std::cout << "Fissionrate_input_points à la fin :\n";
    // for(int i = 0; i < Fissionrate_input.size(); ++i) { 
    //     std::cout << "Point " << i << ": " << Fissionrate_input[i] << std::endl;
    // }

    // std::cout << "Hydrostaticstress_input_points à la fin :\n";
    // for(int i = 0; i < Hydrostaticstress_input.size(); ++i) { 
    //     std::cout << "Point " << i << ": " << Hydrostaticstress_input[i] << std::endl;
    // }

    // std::cout << "Steampressure_input_points à la fin :\n";
    // for(int i = 0; i < Steampressure_input.size(); ++i) { 
    //     std::cout << "Point " << i << ": " << Steampressure_input[i] << std::endl;
    // }


	timer = clock() - timer;

	logExecutionTime((double)timer / CLOCKS_PER_SEC, Time_step_number);
	Execution_file.close();

	return 0;
}

void logExecutionTime(double timer, int time_step_number)
{
	Execution_file << std::setprecision(12) << std::scientific << timer << "\t" << CLOCKS_PER_SEC << "\t" << (double)timer * CLOCKS_PER_SEC << "\t" << time_step_number << std::endl;
}
