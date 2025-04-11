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

void loadSourcesFromFile(const std::string &filePath, std::vector<Source> &sources)
{
    std::ifstream source_file(TestPath + filePath, std::ios::in);

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

std::vector<Source> sourceInterpolation(const std::vector<Source> &sources, int points_per_interval)
{
    std::vector<Source> interpolated_sources;
    if (sources.empty())
        return interpolated_sources;

    // Copy the first source exactly
    interpolated_sources.push_back(sources.front());

    // Iterate over each interval between given source points
    for (size_t i = 0; i < sources.size() - 1; ++i)
    {
        const Source &start = sources[i];
        const Source &end = sources[i + 1];

        double time_step = (end.time - start.time) / (points_per_interval);

        // Generate interpolated points between start and end
        for (int j = 1; j < points_per_interval; ++j)
        {
            double t = start.time + j * time_step;
            double alpha = (t - start.time) / (end.time - start.time);

            Source interpolated_source;
            interpolated_source.time = t;

            // Interpolate NormalizedDomain
            interpolated_source.NormalizedDomain.resize(start.NormalizedDomain.size());
            for (size_t k = 0; k < start.NormalizedDomain.size(); ++k)
            {
                interpolated_source.NormalizedDomain[k] = start.NormalizedDomain[k] + alpha * (end.NormalizedDomain[k] - start.NormalizedDomain[k]);
            }

            // Interpolate Slopes and Intercepts
            interpolated_source.Slopes.resize(start.Slopes.size());
            interpolated_source.Intercepts.resize(start.Intercepts.size());
            for (size_t k = 0; k < start.Slopes.size(); ++k)
            {
                interpolated_source.Slopes[k] = start.Slopes[k] + alpha * (end.Slopes[k] - start.Slopes[k]);
                interpolated_source.Intercepts[k] = start.Intercepts[k] + alpha * (end.Intercepts[k] - start.Intercepts[k]);
            }

            interpolated_sources.push_back(interpolated_source);
        }

        // Push the exact end source to maintain the original points
        interpolated_sources.push_back(end);
    }

    return interpolated_sources;
}

void printSources(const std::vector<Source> &sources)
{
    for (const auto &source : sources)
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

void printSource(const Source &source)
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

void writeToFile(const std::vector<Source> &interpolatedSources, double GrainRadius)
{
    // Define the output file path using the TestPath
    std::string outputFile = TestPath + "source_output.txt";

    // Open the output file for writing
    std::ofstream outFile(outputFile);

    if (!outFile.is_open())
    {
        std::cerr << "Error: Unable to open output file " << outputFile << std::endl;
        return;
    }

    // Write header line
    outFile << "t (h) # Normalized Domain (-) # Slopes (at/m2.s) # Intercepts (at/m3.s) # Volume Average (at/m3.s)\n";

    // Loop through the interpolated sources and write the data to the file
    for (const auto &source : interpolatedSources)
    {
        // Compute volume average
        double volumeAverage = Source_Volume_Average(GrainRadius, source);

        // Write time
        outFile << source.time << " # ";

        // Write domain
        for (size_t i = 0; i < source.NormalizedDomain.size(); ++i)
        {
            outFile << source.NormalizedDomain[i];
            if (i != source.NormalizedDomain.size() - 1)
                outFile << " "; // Separate values with space
        }
        outFile << " # ";

        // Write slopes
        for (size_t i = 0; i < source.Slopes.size(); ++i)
        {
            outFile << source.Slopes[i];
            if (i != source.Slopes.size() - 1)
                outFile << " "; // Separate values with space
        }
        outFile << " # ";

        // Write intercepts
        for (size_t i = 0; i < source.Intercepts.size(); ++i)
        {
            outFile << source.Intercepts[i];
            if (i != source.Intercepts.size() - 1)
                outFile << " "; // Separate values with space
        }

        // Write volume average in the last column
        outFile << " # " << volumeAverage << "\n";
    }

    // Close the file after writing
    outFile.close();
}

void computeAndSaveSourcesToFile(const std::vector<Source> &sources, const std::string &outputFilePath, double step, double GrainRadius)
{
    std::ofstream outFile(outputFilePath);

    if (!outFile.is_open())
    {
        std::cerr << "Error: Unable to open output file " << outputFilePath << std::endl;
        return;
    }

    for (const auto &source : sources)
    {
        // Compute volume average for the current source
        double volumeAverage = Source_Volume_Average(GrainRadius, source);

        // Write the time and the volume average
        outFile << "Time: " << source.time << " s # Volume Average: " << volumeAverage << " at/m^3.s\n";
        outFile << "r (micron)\tS (at/m^3.s)\n";

        // Loop through the source's normalized domain and compute S
        for (size_t i = 0; i < source.Slopes.size(); ++i)
        {
            double nd_start = source.NormalizedDomain[i];
            double nd_end = source.NormalizedDomain[i + 1];
            double A = source.Slopes[i];
            double B = source.Intercepts[i];

            // Modified loop condition to include the endpoint
            for (double nd = nd_start; nd < nd_end + step; nd += step)
            {
                double r = nd * GrainRadius * 1e6;
                double S = A * r * 1e-06 + B;       // Compute source value S
                outFile << r << "\t" << S << "\n"; // Write r and S to file
            }
        }
        outFile << "\n"; // Separate time instances
    }

    outFile.close();
}

double Source_Volume_Average(double GrainRadius, Source source)
{
    size_t NumberofRegions = source.Slopes.size(); // Number of regions
    double VA = 0.0;  // Initialize the volume average
    double totalVolume = 0.0;

    if (NumberofRegions < 1) return 0.0; // Early exit for invalid input

    // Convert normalized domain to actual radii
    std::vector<double> domain(source.NormalizedDomain.size());
    std::transform(source.NormalizedDomain.begin(), source.NormalizedDomain.end(), domain.begin(),
                   [GrainRadius](double x) { return x * GrainRadius; });

    // Compute volume integral and total volume
    for (size_t j = 0; j < NumberofRegions; ++j)
    {
        double r0 = domain[j];
        double r1 = domain[j + 1];

        double A = source.Slopes[j];
        double B = source.Intercepts[j];

        double integral = A * (pow(r1, 4) - pow(r0, 4)) / 4.0 + B * (pow(r1, 3) - pow(r0, 3)) / 3.0;
        double volume = (pow(r1, 3) - pow(r0, 3)) / 3.0;

        VA += integral;
        totalVolume += volume;
    }

    return (totalVolume != 0.0) ? (VA / totalVolume) : 0.0;
}
