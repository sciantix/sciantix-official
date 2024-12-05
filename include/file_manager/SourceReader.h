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
 * @brief Reads the source values (slope and intercept) from the "source.txt" file.
 * The slope and intercept can be of any value.
 * SDA 2.0 runs using the values given by the user
 * If the file is source.txt is missing, SDA 1.0 runs (the previous version of the solver)
 * If the file contains 0 for both the slope and intercept, it treats it as a missing file
 * If the slope and intercept are empty, it treats it as a missing file
 * If the slope is empty, by default it is set to 0
 * If the intercept is empty, the boolean EC is set to true and so in the solver the intercept will be by default the getProductionRate()
 * 
 * @param Source_slope_input Reference to a double where the source slope value will be stored.
 * @param Source_intercept_input Reference to a double where the source intercept value will be stored.
 * @param EC Intercept empty boolean
 * @author A. Zayat
 */
void ReadSource(double &Source_slope_input,double &Source_intercept_input, bool EC) 
{
    // Attempt to open the file
    std::ifstream source_file(TestPath + "source.txt", std::ios::in);
    EC = false;
    
    if (source_file.is_open()) 
    {
        std::string line;
        bool ARead = false; // To track if 'slope' is read
        bool BRead = false; // To track if 'intercept' is read

    while (std::getline(source_file, line)) 
    {
        // Skip comments
        if (line.empty() || line[0] == '#') 
        {
            continue;
        }

        // Assign first numerical value to 'slope'
        if (!ARead) 
        {
            Source_slope_input = std::stod(line); // Convert line to double
            ARead = true;        // Mark 'slope' as read
        }
        // Assign second numerical value to 'intercept'
        else if (!BRead) 
        {
            Source_intercept_input = std::stod(line); // Convert line to double
            BRead = true;        // Mark 'intercept' as read
        }

        // Stop processing once both values are read
        if (ARead && BRead) 
        {
            break;
        }
    }
        // Assign default value to 'slope' if it is still empty
        if (!ARead) 
        {
            Source_slope_input = 0;
        }
        // Assign default value to 'intercept' if it is still empty
        if (!BRead) 
        {
            EC = true;
        }
        // Close File
        source_file.close();
    } 
    else 
    {
        // If the file is missing, default to 0
        Source_slope_input = 0.0;
        Source_intercept_input = 0.0;
    }
}

/**
 * @brief Reads the multiregion source values (normalized domain, slopes and intercepts) from the "general_source.txt" file.
 * This reader is a dedicated reader for the SpectralDiffusion_General_Source solver
 * If the file is general_source.txt is missing, the solver doesn't run
 * If the file contains 0s, the solver doesn't run
 * If the file is empty, , the solver doesn't run
 
 * @param general_slope Reference to a source where the data will be stored
 * @param time the time at which we have this source functiom
 * @author A. Zayat
 */
void ReadGeneralSourceFile(Source general_source, double time)
{
    // Attempt to open the file
    std::ifstream source_file(TestPath + "general_source.txt", std::ios::in);
    
    // Missing file
    if (!source_file.is_open()) 
    {
        std::cerr << "Error: Unable to open file | File is missing, cannot run: " << filePath << "\n";
        exit(EXIT_FAILURE); // Terminate the program with an error code
    }

    std::string line, token;
    std::getline(source_file, line); // Read the entire file into a single string
    std::stringstream ss(line);

    // Parse the time
    std::getline(ss, token, '#');
    time = std::stod(token);

    // Parse Normalized Domain
    std::getline(ss, token, '#');
    std::stringstream NormalizedDomainStream(token);
    double value;
    while (NormalizedDomainStream >> value) 
    {
        general_source.NormalizedDomain.push_back(value);
    }

    // Parse Slopes
    std::getline(ss, token, '#');
    std::stringstream SlopesStream(token);
    while (SlopesStream >> value) 
    {
        general_source.Slopes.push_back(value);
    }

    // Parse Intercepts
    std::getline(ss, token, '#');
    std::stringstream InterceptsStream(token);
    while (InterceptsStream >> value) 
    {
        general_source.Intercepts.push_back(value);
    }

}

#endif // SOURCE_READER_H