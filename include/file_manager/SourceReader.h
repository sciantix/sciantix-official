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

#ifndef SOURCE_READER_H
#define SOURCE_READER_H
#include <fstream>
#include <string>
#include <iostream>
#include "Source.h"


/**
 * @brief Source Reader contains 2 functions
 * ReadSource that is dedicated to read the source.txt file for SpectralDiffusion 2.0 Solver
 * ReadNonUniformSource that is dedicated to read the non_uniform_source.txt file for SpectralDiffusion NUS Solver
 * @author A. Zayat
 */




/**
 * @brief Reads the source values (slope and intercept) from the "source.txt" file.
 * The slope and intercept can be of any value.
 * SDA 2.0 runs using the values given by the user
 * If the file is source.txt is missing, SDA 1.0 runs (the previous version of the solver)
 * If the file contains 0 for both the slope and intercept, it treats it as a missing file
 * If the slope and intercept are empty, it treats it as a missing file
 * If the slope is empty, by default it is set to 0
 * If the intercept is empty the intercept will be set to 0
 * 
 * @param Source_slope_input Reference to a double where the source slope value will be stored.
 * @param Source_intercept_input Reference to a double where the source intercept value will be stored.
 * @author A. Zayat
 */
void ReadSource(double &Source_slope_input, double &Source_intercept_input) 
{   
    // Initialize variables
    Source_slope_input = 0.0;
    Source_intercept_input = 0.0;

    // Attempt to open the file
    std::ifstream source_file(TestPath + "source.txt", std::ios::in);
    
    if (source_file.is_open()) 
    {
        double slope, intercept;
        std::string line;

        while (std::getline(source_file, line)) 
        {
            // Skip comments and empty lines
            if (line.empty() || line[0] == '#') 
            {
                continue;
            }

            std::istringstream iss(line);
            if (iss >> slope >> intercept) // Read two values from the line
            {
                Source_slope_input = slope;
                Source_intercept_input = intercept;
                break; // Stop reading after the first valid line
            }
        }

        // Close the file
        source_file.close();
    } 
}

/**
 * @brief Reads the multiregion source values (normalized domain, slopes and intercepts) from the "general_source.txt" file.
 * This reader is a dedicated reader for the SpectralDiffusion_General_Source solver.
 * If the file is general_source.txt is missing, the solver doesn't run.
 
 * @param non_uniform_source Reference to a source where the data will be stored
 * @param time the time at which we have this source functiom
 * @author A. Zayat
 */
void ReadNonUniformSource(Source &non_uniform_source)
{
    // Attempt to open the file
    std::ifstream source_file(TestPath + "non_uniform_source.txt", std::ios::in);
    
    // Missing file
    if (!source_file.is_open()) 
    {
        std::cerr << "Error: Unable to open file | File is missing, cannot run: " << TestPath + "general_source.txt" << "\n";
        exit(EXIT_FAILURE); // Terminate the program with an error code
    }

    std::string line, token;
    std::getline(source_file, line); // Read the entire file into a single string
    std::stringstream ss(line);

    // Parse the time
    std::getline(ss, token, '#');
    non_uniform_source.time = std::stod(token);

    // Parse Normalized Domain
    std::getline(ss, token, '#');
    std::stringstream NormalizedDomainStream(token);
    double value;
    while (NormalizedDomainStream >> value) 
    {
        non_uniform_source.NormalizedDomain.push_back(value);
    }

    // Parse Slopes
    std::getline(ss, token, '#');
    std::stringstream SlopesStream(token);
    while (SlopesStream >> value) 
    {
        non_uniform_source.Slopes.push_back(value);
    }

    // Parse Intercepts
    std::getline(ss, token, '#');
    std::stringstream InterceptsStream(token);
    while (InterceptsStream >> value) 
    {
        non_uniform_source.Intercepts.push_back(value);
    }
  
}

#endif // SOURCE_READER_H