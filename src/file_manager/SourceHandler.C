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
 * @param total_steps The number of steps to interpolate along the entire time period
 * @return A vector of Source objects with interpolated values
 */
std::vector<Source> sourceInterpolation(const std::vector<Source> &sources, int total_steps)
{
    std::vector<Source> interpolated_sources;
    if (sources.empty()) return interpolated_sources;

    // Define global time range
    double t_min = sources.front().time;
    double t_max = sources.back().time;
    double time_step = (t_max - t_min) / (total_steps - 1);

    // Generate 100 equally spaced time points
    for (int i = 0; i < total_steps; ++i)
    {
        double t = t_min + i * time_step;
        
        // Find the interval [start, end] that contains t
        size_t idx = 0;
        while (idx < sources.size() - 1 && sources[idx + 1].time < t)
            ++idx;

        const Source &start = sources[idx];
        const Source &end = sources[std::min(idx + 1, sources.size() - 1)];

        // Compute interpolation factor
        double alpha = (t - start.time) / (end.time - start.time);

        // Interpolate source values
        Source interpolated_source;
        interpolated_source.time = t;

        // Interpolate NormalizedDomain
        interpolated_source.NormalizedDomain.clear();
        for (size_t k = 0; k < start.NormalizedDomain.size(); ++k)
        {
            double domain_interp = start.NormalizedDomain[k] + alpha * (end.NormalizedDomain[k] - start.NormalizedDomain[k]);
            interpolated_source.NormalizedDomain.push_back(domain_interp);
        }

        // Interpolate Slopes and Intercepts
        interpolated_source.Slopes.clear();
        interpolated_source.Intercepts.clear();
        for (size_t k = 0; k < start.Slopes.size(); ++k)
        {
            double slope_interp = start.Slopes[k] + alpha * (end.Slopes[k] - start.Slopes[k]);
            double intercept_interp = start.Intercepts[k] + alpha * (end.Intercepts[k] - start.Intercepts[k]);
            interpolated_source.Slopes.push_back(slope_interp);
            interpolated_source.Intercepts.push_back(intercept_interp);
        }

        interpolated_sources.push_back(interpolated_source);
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

// Function to get the current source based on the current time step and isources vector
Source getCurrentSource(const std::vector<Source>& isources, size_t currentIndex) {
    // Ensure the index is within bounds
    if (currentIndex >= isources.size()) {
        std::cerr << "Error: Index out of bounds (" << currentIndex << ")" << std::endl;
        return {}; // Return an empty Source if the index is invalid
    }

    // Return the source at the current index
    return isources[currentIndex];
}