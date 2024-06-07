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
//  Authors: D. Pizzocri, T. Barani                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include <vector>
#include <iostream>
#include <fstream>
#include <ctime>

extern clock_t timer, timer_time_step;

extern int Sciantix_options[40];
extern double Sciantix_history[20];
extern double Sciantix_variables[300];
extern double Sciantix_scaling_factors[10];
extern double Sciantix_diffusion_modes[1000];

extern long long int Time_step_number;
extern double  Time_h, dTime_h, Time_end_h;
extern double  Time_s, Time_end_s;
extern double  Number_of_time_steps_per_interval;

extern std::ofstream Output_file;
extern std::ofstream Execution_file;

extern int Input_history_points;
extern int Temperature_input_points;
extern int Fissionrate_input_points;
extern int Hydrostaticstress_input_points;
extern int Stempressure_input_points;
extern std::vector<double> Time_temperature_input;
extern std::vector<double> Time_fissionrate_input;
extern std::vector<double> Time_hydrostaticstress_input;
extern std::vector<double> Time_steampressure_input;
extern std::vector<double> Time_input;
extern std::vector<double> Temperature_input;
extern std::vector<double> Fissionrate_input;
extern std::vector<double> Hydrostaticstress_input;
extern std::vector<double> Steampressure_input;
