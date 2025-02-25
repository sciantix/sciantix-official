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


#include <iostream>
#include <vector>
#include <cmath>
#include <fstream>
#include <sstream>
#include <iomanip> // for setting precision
#include "SourceHandler.h"
#include "MainVariables.h"


void loadSourcesFromFile(std::vector<Source> &sources)
{
    std::ifstream source_file(TestPath + "non_uniform_source.txt", std::ios::in);

    if (!source_file.is_open())
    {
        std::cerr << "Error: Unable to open file | File is missing, cannot run.\n";
        exit(EXIT_FAILURE);
    }

    std::string line, token;

    // Read the entire file line by line
    while (std::getline(source_file, line))
    {
        std::stringstream ss(line);

        Source non_uniform_source; // Create a new Source object for each line

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

        // Add the parsed Source object to the sources vector
        sources.push_back(non_uniform_source);
    }
}

/**
 * @brief Linear interpolation function for NormalizedDomain, Slopes, and Intercepts between time instances
 * @param sources The source vector containing Source objects with time and corresponding data
 * @param num_steps The number of steps to interpolate between two time instances
 * @return A vector of Source objects with interpolated values
 */
std::vector<Source> sourceInterpolation(const std::vector<Source> &sources, int num_steps)
{
    std::vector<Source> interpolated_sources;

    for (size_t i = 0; i < sources.size() - 1; ++i)
    {
        const Source &start = sources[i];
        const Source &end = sources[i + 1];

        double time_step = (end.time - start.time) / num_steps;

        for (int j = 0; j <= num_steps; ++j)
        {
            Source interpolated_source;
            interpolated_source.time = start.time + j * time_step;

            // Interpolate NormalizedDomain (we assume it's the same across all time steps for each region)
            interpolated_source.NormalizedDomain.clear();
            for (size_t k = 0; k < start.NormalizedDomain.size(); ++k)
            {
                double domain_interp = start.NormalizedDomain[k] + (end.NormalizedDomain[k] - start.NormalizedDomain[k]) * j / num_steps;
                interpolated_source.NormalizedDomain.push_back(domain_interp);
            }

            // Interpolate Slopes and Intercepts
            interpolated_source.Slopes.clear();
            interpolated_source.Intercepts.clear();

            for (size_t k = 0; k < start.Slopes.size(); ++k)
            {
                double slope_interp = start.Slopes[k] + (end.Slopes[k] - start.Slopes[k]) * j / num_steps;
                double intercept_interp = start.Intercepts[k] + (end.Intercepts[k] - start.Intercepts[k]) * j / num_steps;

                interpolated_source.Slopes.push_back(slope_interp);
                interpolated_source.Intercepts.push_back(intercept_interp);
            }

            interpolated_sources.push_back(interpolated_source);
        }
    }

    return interpolated_sources;
}

/**
 * @brief Prints all the interpolated source data
 * @param interpolated_sources The interpolated source data to print
 */
void printInterpolatedSources(const std::vector<Source> &interpolated_sources)
{
    for (const auto &source : interpolated_sources)
    {
        std::cout << "Time: " << source.time << std::endl;
        std::cout << "Normalized Domain: ";
        for (const auto &val : source.NormalizedDomain)
        {
            std::cout << val << " ";
        }
        std::cout << std::endl;

        std::cout << "Slopes: ";
        for (const auto &val : source.Slopes)
        {
            std::cout << val << " ";
        }
        std::cout << std::endl;

        std::cout << "Intercepts: ";
        for (const auto &val : source.Intercepts)
        {
            std::cout << val << " ";
        }
        std::cout << std::endl;
    }
}
void writeToFile(const std::vector<Source>& interpolatedSources) {
    // Define the output file path using the TestPath
    std::string outputFile = TestPath + "source_output.txt";

    // Open the output file for writing
    std::ofstream outFile(outputFile);

    if (!outFile.is_open()) {
        std::cerr << "Error: Unable to open output file " << outputFile << std::endl;
        return;
    }

    // Loop through the interpolated sources and write the data to the file
    for (const auto& source : interpolatedSources) {
        // Write time and normalized domain
        outFile << source.time << " # ";
        for (size_t i = 0; i < source.NormalizedDomain.size(); ++i) {
            outFile << source.NormalizedDomain[i];
            if (i != source.NormalizedDomain.size() - 1) {
                outFile << " ";  // Separate values with space
            }
        }
        outFile << " # ";

        // Write slopes
        for (size_t i = 0; i < source.Slopes.size(); ++i) {
            outFile << source.Slopes[i];
            if (i != source.Slopes.size() - 1) {
                outFile << " ";  // Separate values with space
            }
        }
        outFile << " # ";

        // Write intercepts
        for (size_t i = 0; i < source.Intercepts.size(); ++i) {
            outFile << source.Intercepts[i];
            if (i != source.Intercepts.size() - 1) {
                outFile << " ";  // Separate values with space
            }
        }
        outFile << " " << std::endl;  // Add new line at the end of each entry
    }

    // Close the file after writing
    outFile.close();
}

// Function to get the final values based on current time step
Source getCurrentSource(const std::vector<Source>& sources, double t_current) {
    // Variable to store the best match (time <= t_current)
    Source finalSource;
    finalSource.time = 0;
    finalSource.NormalizedDomain = {};
    finalSource.Slopes = {};
    finalSource.Intercepts = {};

    // Iterate over the sources and find the one with the largest time <= t_current
    for (const auto& source : sources) {
        if (source.time <= t_current) {
            finalSource = source;  // Store the source if it's a match
        }
    }

    // If no match is found (no source time is <= t_current), we return an empty source
    if (finalSource.NormalizedDomain.empty()) {
        std::cerr << "Error: No source found with time <= " << t_current << std::endl;
    }

    return finalSource;
}
Source& getNonUniformSource();

void computeAndSaveSourcesToFile(const std::vector<Source>& sources, const std::string& outputFilePath, double scale_factor, double step) 
{
    std::ofstream outFile(outputFilePath);

    if (!outFile.is_open()) {
        std::cerr << "Error: Unable to open output file " << outputFilePath << std::endl;
        return;
    }

    for (const auto& source : sources) {
        outFile << "Time: " << source.time << " s\n";
        outFile << "r (micron)\tS (at/m^3.s)\n";

        for (size_t i = 0; i < source.NormalizedDomain.size() - 1; ++i) {
            double nd_start = source.NormalizedDomain[i];
            double nd_end = source.NormalizedDomain[i + 1];
            double A = source.Slopes[i];
            double B = source.Intercepts[i];

            // Modified loop condition to include the endpoint
            for (double nd = nd_start; nd < nd_end + step; nd += step) {
                double r = nd * scale_factor;
                double S = A * r + B;
                outFile << r << "\t" << S << "\n";
            }
        }
        outFile << "\n"; // Separate time instances
    }

    outFile.close();
}
