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

/**
 * @brief Reads the source slope value from the "source.txt" file.
 * @param Source_slope_input Reference to a double where the source slope value will be stored.
 * @param Source_intercept_input Reference to a double where the source intercept value will be stored.
 * @author A. Zayat
 */
void ReadSource(double &Source_slope_input,double &Source_intercept_input) 
{
    // Attempt to open the file
    std::ifstream source_file(TestPath + "source.txt", std::ios::in);
    
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
        source_file.close();
    } 
    else 
    {
        // If the file is missing, default to 0
        Source_slope_input = 0.0;
        Source_intercept_input = 0.0;
    }
}

#endif // SOURCE_READER_H