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

#ifndef INPUT_READING_H
#define INPUT_READING_H

#include "ErrorMessages.h"
#include <numeric>
#include <sstream>
#include <string>
#include <vector>

/**
 * @brief Handles all input processing for the simulation.
 * It opens necessary input files, reads configuration and initial condition data,
 * logs this data for verification, and manages any missing file errors.
 *
 * @author D. Pizzocri
 * @author T. Barani
 * @author G. Zullo
 * @author F. Bastien
 *
 */
void InputReading(int Sciantix_options[], double Sciantix_variables[],
                  double Sciantix_scaling_factors[], int& Input_history_points,
                  std::vector<double>& Time_input, std::vector<double>& Temperature_input,
                  std::vector<double>& Fissionrate_input,
                  std::vector<double>& Hydrostaticstress_input,
                  std::vector<double>& Steampressure_input, double& Time_end_h, double& Time_end_s);

unsigned short int ReadOneSetting(std::string variable_name, std::ifstream& input_file,
                                  std::ofstream& output_file);
double             ReadOneParameter(std::string variable_name, std::ifstream& input_file,
                                    std::ofstream& output_file);

#endif  // INPUT_READING_H
