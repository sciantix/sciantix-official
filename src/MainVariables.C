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
//  Version: 2.2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "MainVariables.h"

int    Sciantix_options[40];
double Sciantix_history[20];
double Sciantix_variables[300];
double Sciantix_scaling_factors[20];
// CODE DEVELOPMENT : INCREASE IN DIFFUSION MODES FROM 720 TO 1300
double Sciantix_diffusion_modes[1300];
double Sciantix_thermochemistry[300];
ThermochemistrySettings Sciantix_thermochemistry_settings;
//

long long int Time_step_number(0);
double        Time_h(0.0), dTime_h(0.0), Time_end_h(0.0);  // (h)
double        Time_s(0.0), Time_end_s(0.0);                // (s)
double        Number_of_time_steps_per_interval(100);

std::ofstream Output_file;
std::ofstream Execution_file;
std::string   TestPath;

int                 Input_history_points(1000);
int                 Temperature_input_points;
int                 Fissionrate_input_points;
int                 Hydrostaticstress_input_points;
int                 Stempressure_input_points;
// CODE DEVELOPMENT : SYSTEM PRESSURE AND O/M RATIO INPUTS
int                 Systempressure_input_points;
int                 OMratio_input_points;
//
std::vector<double> Time_temperature_input;
std::vector<double> Time_fissionrate_input;
std::vector<double> Time_hydrostaticstress_input;
std::vector<double> Time_steampressure_input;
// CODE DEVELOPMENT : SYSTEM PRESSURE AND O/M RATIO INPUTS
std::vector<double> Time_systempressure_input;
std::vector<double> Time_OMratio_input;
//
std::vector<double> Time_input(1000, 0.0);
std::vector<double> Temperature_input(1000, 0.0);
std::vector<double> Fissionrate_input(1000, 0.0);
std::vector<double> Hydrostaticstress_input(1000, 0.0);
std::vector<double> Steampressure_input(1000, 0.0);
// CODE DEVELOPMENT : SYSTEM PRESSURE AND O/M RATIO INPUTS
std::vector<double> Systempressure_input(1000, 0.0);
std::vector<double> OMratio_input(1000, 2.0);
//