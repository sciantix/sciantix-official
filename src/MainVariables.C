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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "MainVariables.h"

int                      Sciantix_options[SCIANTIX_OPTIONS_SIZE];
double                   Sciantix_history[SCIANTIX_HISTORY_SIZE];
double                   Sciantix_variables[SCIANTIX_VARIABLES_SIZE];
double                   Sciantix_scaling_factors[SCIANTIX_SCALING_FACTORS_SIZE];
double                   Sciantix_diffusion_modes[SCIANTIX_DIFFUSION_MODES_SIZE];
double                   Sciantix_thermochemistry[SCIANTIX_THERMOCHEMISTRY_SIZE];
ThermochemistrySettings* Sciantix_thermochemistry_settings = nullptr;

long long int Time_step_number(0);
double        Time_h(0.0), dTime_h(0.0), Time_end_h(0.0);  // (h)
double        Time_s(0.0), Time_end_s(0.0);                // (s)
double        Number_of_time_steps_per_interval(100);

std::ofstream Output_file;
std::ofstream Execution_file;
std::string   TestPath;

int Input_history_points(0);
int Temperature_input_points;
int Fissionrate_input_points;
int Hydrostaticstress_input_points;
int Stempressure_input_points;
int Systempressure_input_points;
int OMratio_input_points;

std::vector<double> Time_temperature_input;
std::vector<double> Time_fissionrate_input;
std::vector<double> Time_hydrostaticstress_input;
std::vector<double> Time_steampressure_input;
std::vector<double> Time_systempressure_input;
std::vector<double> Time_OMratio_input;
std::vector<double> Time_input;
std::vector<double> Temperature_input;
std::vector<double> Fissionrate_input;
std::vector<double> Hydrostaticstress_input;
std::vector<double> Steampressure_input;
std::vector<double> Systempressure_input;
std::vector<double> OMratio_input;