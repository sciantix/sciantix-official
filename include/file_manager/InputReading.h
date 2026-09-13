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

#ifndef INPUT_READING_H
#define INPUT_READING_H

#include "ErrorMessages.h"
#include "ThermochemistrySettings.h"
#include <fstream>
#include <map>
#include <numeric>
#include <set>
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
void InputReading(int                       Sciantix_options[],
                  double                    Sciantix_variables[],
                  double                    Sciantix_scaling_factors[],
                  double                    Sciantix_thermochemistry[],
                  ThermochemistrySettings*& Sciantix_thermochemistry_settings,
                  int&                      Input_history_points,
                  std::vector<double>&      Time_input,
                  std::vector<double>&      Temperature_input,
                  std::vector<double>&      Fissionrate_input,
                  std::vector<double>&      Hydrostaticstress_input,
                  std::vector<double>&      Steampressure_input,
                  std::vector<double>&      Systempressure_input,
                  std::vector<double>&      OMratio_input,
                  double&                   Time_end_h,
                  double&                   Time_end_s);

/**
 * @brief The keyed entries of an input file ("<value(s)> # <Key> (<description>)" lines).
 *
 * See ParseNamedEntries in InputReading.C for the format.
 */
struct NamedInput
{
    std::map<std::string, std::string> value;     ///< key -> the text preceding the '#'
    mutable std::set<std::string>      consumed;  ///< keys some reader actually asked for
};

NamedInput ParseNamedEntries(const std::string& path);

/// Stops the run if the file holds an entry no reader asked for (a misspelt or retired key).
void ReportUnrecognisedEntries(const std::string& file_name, const NamedInput& parsed);

unsigned short int
ReadOneSetting(const std::string& variable_name, const NamedInput& settings, std::ofstream& output_file);

double ReadOneParameter(const std::string& variable_name,
                        const NamedInput&  parsed,
                        std::ofstream&     output_file,
                        double             fallback);

std::vector<double> ReadSeveralParameters(const std::string& variable_name,
                                          const NamedInput&  parsed,
                                          std::size_t        count,
                                          std::ofstream&     output_file,
                                          double             fallback);

/**
 * @brief Reads input_scaling_factors.txt. A missing file, or a missing entry, gives a factor of 1.0.
 */
void ReadScalingFactors(const std::string& path, double Sciantix_scaling_factors[], std::ofstream& input_check);

#endif  // INPUT_READING_H
