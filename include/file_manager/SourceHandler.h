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
 * This file contains functions for handling the non uniform source data in Sciantix.
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
void loadSourcesFromFile(const std::string &filePath, std::vector<Source> &sources);
void loadICFromFile(const std::string &filePath, std::vector<Source> &ics, bool &fileFound);


/**
 * @brief Performs interpolation on the provided sources to create smoother data.
 * @param sources A constant reference to the input source data.
 * @param interpolation_size Number of intermediate points to generate.
 * @return A vector containing interpolated Source objects.
 */
std::vector<Source> sourceInterpolation(const std::vector<Source> &sources, int interpolation_size);

/**
 * @brief Prints the sources for debugging.
 * @param sources A constant reference to a vector containing data of class "source" to print.
 */
void printSources(const std::vector<Source> &sources);

/**
 * @brief Prints the source for debugging.
 * @param source A constant reference to the source data to print.
 */
void printSource(const Source &source);

/**
 * @brief Writes the processed source data to an output file in the same format as the input file with an extra column for the average value of the source across the grain.
 * @param sources A constant reference to the sources data to be written.
 * @param GrainRadius The grain radius is used to compute the volume averaged value of the source across the grain.
 */
void writeToFile(const std::vector<Source> &interpolatedSources, double GrainRadius);

/**
 * @brief Retrieves the current shape of a source across the grain radius at a given time as well as the volume averaged value of the source across the grain.
 * @param sources A constant reference to a vector of Source objects.
 * @param time The specific time for which the source shape is requested.
 * @param GrainRadius
 */
 void computeAndSaveSourcesToFile(const std::vector<Source> &sources, const std::string &outputFilePath, double step, double GrainRadius);
 void computeAndSaveICToFile(const std::vector<Source> &ics,
    const std::string &outputFilePath,
    double step,
    double GrainRadius);
 
    /**
  * @brief Gives the spatial averaged value of the fission rate (Fdot)
  * @param GrainRadius
  * @param source
  */
 double Source_Volume_Average(double GrainRadius, Source source);

#endif // SOURCEHANDLER_H
