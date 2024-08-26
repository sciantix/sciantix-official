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

#include "Sciantix.h"
#include "InputInterpolation.h"
#include "InputReading.h"
#include "Initialization.h"
#include "TimeStepCalculation.h"
#include "MainVariables.h"
#include "ErrorMessages.h"
#include <iostream>
#include <fstream>
#include <ctime>

using namespace std;

/**
 * @brief Logs the execution time of the simulation.
 * @param timer The total execution time measured in seconds.
 * @param time_step_number The total number of time steps executed during the simulation.
 */
void logExecutionTime(double timer, int time_step_number, std::ofstream &Execution_file);

/**
 * @brief Main entry point for the SCIANTIX program. 
 *
 * SCIANTIX is a 0D code developed at Politecnico di Milano.
 * The objective of SCIANTIX is to represent the behaviour of a single grain of nuclear fuel.
 * The modelling of inert gas behaviour is the main aspect considered.
 * Engineering models are used, allowing for future integration in industrial fuel performance codes.
 * Nevertheless, physically-based model are preferred to empirical models.
 * This facilitates the incorporation of information from lower length scale calculations.
 *
 * @see <a href="https://www.sciencedirect.com/science/article/pii/S0022311519313868" target="_blank">Pizzocri D. et al (2020). Journal of Nuclear Materials, 532, 152042.</a>
 * @see <a href="https://www.sciencedirect.com/science/article/pii/S0022311523005111" target="_blank">Zullo G. et al (2023). Journal of Nuclear Materials, 587, 154744.</a>
 *
 * At present, this version of the code is validated against experiments for
 * - intragranular gaseous swelling
 * - intergranular gaseous swelling
 * - helium behaviour and release in annealing conditions
 * - release of radioactive fission gases
 * The validation database is accessible in the *regression* folder.
 * @param argc Number of command-line arguments.
 * @param argv Array of command-line arguments, where argv[1] is expected to be the path to the input file.
 * @return Returns 0 upon successful completion of the program.
 */
int main(int argc, char **argv)
{
    clock_t timer, timer_time_step;

    if (argc < 2)
    {
        std::cerr << "Using current directory." << std::endl;
        std::cout << "Use ./sciantix.x ../input_path/ in the working directory" << std::endl;
        TestPath = "./";
    }
    else
    {
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