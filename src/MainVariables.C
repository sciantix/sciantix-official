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

#include "MainVariables.h"

clock_t timer, timer_time_step;

int    Sciantix_options[40];
double Sciantix_history[20];
double Sciantix_variables[300];
double Sciantix_scaling_factors[10];
double Sciantix_diffusion_modes[1000];

long long int Time_step_number(0);
double  Time_h(0.0), dTime_h(0.0), Time_end_h(0.0); // (h)
double  Time_s(0.0), Time_end_s(0.0); // (s)
double  Number_of_time_steps_per_interval(400);

std::ofstream Output_file;
std::ofstream Execution_file;

int Input_history_points(1000);
std::vector<double> Time_input(1000, 0.0);
std::vector<double> Temperature_input(1000, 0.0);
std::vector<double> Fissionrate_input(1000, 0.0);
std::vector<double> Hydrostaticstress_input(1000, 0.0);
std::vector<double> Steampressure_input(1000, 0.0);