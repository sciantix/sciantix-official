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
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef MAIN_VARIABLES_H
#define MAIN_VARIABLES_H

#include "Source.h"
#include <vector>
#include <fstream>

/**
 * @file MainVariables.h
 * @brief This header file contains declarations for variables that are used in MainSCIANTIX.C.
 * 
 * @author D. Pizzocri
 * @author T. Barani
 * @author G. Zullo
 * 
 */

extern int Sciantix_options[40];
extern double Sciantix_history[20];
extern double Sciantix_variables[300];
extern double Sciantix_scaling_factors[10];
extern double Sciantix_diffusion_modes[720];

extern long long int Time_step_number;
extern double  Time_h, dTime_h, Time_end_h;
extern double  Time_s, Time_end_s;
extern double  Number_of_time_steps_per_interval;

extern std::ofstream Output_file;
extern std::ofstream Execution_file;
extern std::string TestPath; 

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
extern std::vector<Source> sources_input;
extern std::vector<Source> sources_interp;

#endif