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

#ifndef MAIN_VARIABLES_H
#define MAIN_VARIABLES_H

#include <fstream>
#include <vector>

/**
 * @file MainVariables.h
 * @brief This header file contains declarations for variables that are used in MainSCIANTIX.C.
 *
 * @author D. Pizzocri
 * @author T. Barani
 * @author G. Zullo
 *
 */

// Sizes of the global state arrays. The diffusion-mode array holds N_MODE_BLOCKS
// contiguous blocks of N_DIFFUSION_MODES spectral modes each: (Xe, Kr, He, Xe133,
// Kr85m, Xe in HBS) x (initial conditions, solution, bubbles).
constexpr int SCIANTIX_OPTIONS_SIZE         = 40;
constexpr int SCIANTIX_HISTORY_SIZE         = 20;
constexpr int SCIANTIX_VARIABLES_SIZE       = 300;
constexpr int SCIANTIX_SCALING_FACTORS_SIZE = 20;
constexpr int N_DIFFUSION_MODES             = 40;
constexpr int N_MODE_BLOCKS                 = 18;
constexpr int SCIANTIX_DIFFUSION_MODES_SIZE = N_MODE_BLOCKS * N_DIFFUSION_MODES;

extern int    Sciantix_options[SCIANTIX_OPTIONS_SIZE];
extern double Sciantix_history[SCIANTIX_HISTORY_SIZE];
extern double Sciantix_variables[SCIANTIX_VARIABLES_SIZE];
extern double Sciantix_scaling_factors[SCIANTIX_SCALING_FACTORS_SIZE];
extern double Sciantix_diffusion_modes[SCIANTIX_DIFFUSION_MODES_SIZE];

extern long long int Time_step_number;
extern double        Time_h, dTime_h, Time_end_h;
extern double        Time_s, Time_end_s;
extern double        Number_of_time_steps_per_interval;

extern std::ofstream Output_file;
extern std::ofstream Execution_file;
extern std::string   TestPath;

extern int                 Input_history_points;
extern int                 Temperature_input_points;
extern int                 Fissionrate_input_points;
extern int                 Hydrostaticstress_input_points;
extern int                 Stempressure_input_points;
extern std::vector<double> Time_temperature_input;
extern std::vector<double> Time_fissionrate_input;
extern std::vector<double> Time_hydrostaticstress_input;
extern std::vector<double> Time_steampressure_input;
extern std::vector<double> Time_input;
extern std::vector<double> Temperature_input;
extern std::vector<double> Fissionrate_input;
extern std::vector<double> Hydrostaticstress_input;
extern std::vector<double> Steampressure_input;

#endif