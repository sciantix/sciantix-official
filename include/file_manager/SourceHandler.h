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

#ifndef SOURCEHANDLER_H
#define SOURCEHANDLER_H

/**
 *
 * This file contains functions for handling source data in Sciantix.
 * It includes loading source data from a file, interpolating values, printing
 * the data, and writing results to a file.
 *
 * @author A. Zayat
 */

#include <vector>
#include "Source.h"
/**
 * @brief Loads source data from a file into the sources vector.
 * @param sources A reference to a vector of Source objects.
 * @note The file format should match expected input values.
 */
void loadSourcesFromFile(std::vector<Source> &sources);
/**
 * @brief Performs interpolation on the provided sources to create smoother data.
 * @param sources A constant reference to the input source data.
 * @param interpolation_size Number of intermediate points to generate.
 * @return A vector containing interpolated Source objects.
 */
std::vector<Source> sourceInterpolation(const std::vector<Source> &sources, int interpolation_size);
/**
 * @brief Prints the interpolated sources for debugging.
 * @param sources A constant reference to the source data to print.
 */
void printInterpolatedSources(const std::vector<Source> &sources);
/**
 * @brief Writes the processed source data to an output file.
 * @param sources A constant reference to the source data to be written.
 */
void writeToFile(const std::vector<Source> &sources);
/**
 * @brief Retrieves the current value of a source at a given time.
 * @param sources A constant reference to a vector of Source objects.
 * @param time The specific time for which the source value is requested.
 * @return The interpolated source value at the given time.
 */

void computeAndSaveSourcesToFile(const std::vector<Source> &sources, const std::string &outputFilePath, double scale_factor, double step);

#endif // SOURCEHANDLER_H
