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

void loadICFromFile(const std::string &filePath, std::vector<Source> &ics, bool &fileFound)
{
    std::ifstream source_file(TestPath + filePath, std::ios::in);

    if (!source_file.is_open())
    {
        std::cerr << "Default: Uniform Initial Conditions | File is missing: " << filePath << "\n";
        fileFound = false;
        return;
    }

    fileFound = true;
    std::string line, token;

    while (std::getline(source_file, line))
    {
        std::stringstream ss(line);
        Source ic;

        // Parse the time
        std::getline(ss, token, '#');
        ic.time = std::stod(token);

        // Parse Normalized Domain
        std::getline(ss, token, '#');
        std::stringstream NormalizedDomainStream(token);
        double value;
        while (NormalizedDomainStream >> value)
        {
            ic.NormalizedDomain.push_back(value);
        }

        // Parse Slopes
        std::getline(ss, token, '#');
        std::stringstream SlopesStream(token);
        while (SlopesStream >> value)
        {
            ic.Slopes.push_back(value);
        }

        // Parse Intercepts
        std::getline(ss, token, '#');
        std::stringstream InterceptsStream(token);
        while (InterceptsStream >> value)
        {
            ic.Intercepts.push_back(value);
        }

        ics.push_back(ic);
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
    outFile << "t (h)\tNormalized Domain (-)\tSlopes (at/m2.s)\tIntercepts (at/m3.s)\tVolume Average (at/m3.s)\n";

    // Loop through the interpolated sources and write the data to the file
    for (const auto &source : interpolatedSources)
    {
        // Compute volume average
        double volumeAverage = Source_Volume_Average(GrainRadius, source);

        // Write time
        outFile << source.time << "\t";

        // Write domain
        for (size_t i = 0; i < source.NormalizedDomain.size(); ++i)
        {
            outFile << source.NormalizedDomain[i];
            if (i != source.NormalizedDomain.size() - 1)
                outFile << " "; // Separate values with space
        }
        outFile << "\t";

        // Write slopes
        for (size_t i = 0; i < source.Slopes.size(); ++i)
        {
            outFile << source.Slopes[i];
            if (i != source.Slopes.size() - 1)
                outFile << " "; // Separate values with space
        }
        outFile << "\t";

        // Write intercepts
        for (size_t i = 0; i < source.Intercepts.size(); ++i)
        {
            outFile << source.Intercepts[i];
            if (i != source.Intercepts.size() - 1)
                outFile << " "; // Separate values with space
        }

        // Write volume average in the last column
        outFile << "\t" << volumeAverage << "\n";
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

    // Generate common r values (in microns)
    std::vector<double> r_values;
    for (double nd = 0.0; nd <= 1.0 + 1e-12; nd += step)
        r_values.push_back(nd * GrainRadius * 1e6);

    // Prepare S(r, t) table
    std::vector<std::vector<double>> S_table(r_values.size(), std::vector<double>(sources.size(), 0.0));
    std::vector<double> volumeAverages(sources.size());

    for (size_t t_idx = 0; t_idx < sources.size(); ++t_idx)
    {
        const auto &source = sources[t_idx];
        volumeAverages[t_idx] = Source_Volume_Average(GrainRadius, source);

        for (size_t seg = 0; seg < source.Slopes.size(); ++seg)
        {
            double nd_start = source.NormalizedDomain[seg];
            double nd_end = source.NormalizedDomain[seg + 1];
            double A = source.Slopes[seg];
            double B = source.Intercepts[seg];

            for (size_t i = 0; i < r_values.size(); ++i)
            {
                double nd = r_values[i] / (GrainRadius * 1e6);
                if (nd >= nd_start && nd <= nd_end + 1e-12)
                {
                    double r_m = r_values[i] * 1e-6;
                    S_table[i][t_idx] = A * r_m + B;
                }
            }
        }
    }

    // Write header
    outFile << std::fixed << std::setprecision(3);
    outFile << "r (micron)";
    for (size_t t_idx = 0; t_idx < sources.size(); ++t_idx)
    {
        outFile << "\tS(t=" << std::fixed << std::setprecision(2) << sources[t_idx].time
                << " hr, avg=" << std::scientific << std::setprecision(3)
                << volumeAverages[t_idx] << ")";
    }
    outFile << "\n";

    // Write data rows
    for (size_t i = 0; i < r_values.size(); ++i)
    {
        outFile << std::fixed << std::setprecision(3) << r_values[i];
        for (size_t t_idx = 0; t_idx < sources.size(); ++t_idx)
        {
            outFile << "\t" << std::scientific << std::setprecision(6) << S_table[i][t_idx];
        }
        outFile << "\n";
    }

    outFile.close();
}

void computeAndSaveICToFile(const std::vector<Source> &ics, const std::string &outputFilePath, double step, double GrainRadius)
{
    const std::vector<std::string> elementNames = {"Xe", "Kr", "He", "Xe133", "Kr85", "XeHBS"};
    const std::vector<std::string> componentNames = {
        "grain", "solution", "bubble",
        "grain", "solution", "bubble",
        "grain", "solution", "bubble",
        "grain", "solution", "bubble",
        "grain", "solution", "bubble",
        "HBS", "solution", "bubble"};

    std::ofstream outFile(outputFilePath);
    if (!outFile.is_open())
    {
        std::cerr << "Error: Unable to open output file " << outputFilePath << std::endl;
        return;
    }

    // Generate r values (micron)
    std::vector<double> r_values;
    for (double nd = 0.0; nd <= 1.0 + 1e-12; nd += step)
        r_values.push_back(nd * GrainRadius * 1e6);

    std::vector<std::vector<double>> C_table(r_values.size(), std::vector<double>(18, 0.0));
    std::vector<double> volumeAverages(18);

    for (size_t idx = 0; idx < ics.size(); ++idx)
    {
        const auto &ic = ics[idx];
        volumeAverages[idx] = Source_Volume_Average(GrainRadius, ic);

        for (size_t seg = 0; seg < ic.Slopes.size(); ++seg)
        {
            double nd_start = ic.NormalizedDomain[seg];
            double nd_end = ic.NormalizedDomain[seg + 1];
            double A = ic.Slopes[seg];
            double B = ic.Intercepts[seg];

            for (size_t i = 0; i < r_values.size(); ++i)
            {
                double nd = r_values[i] / (GrainRadius * 1e6);
                if (nd >= nd_start && nd <= nd_end + 1e-12)
                {
                    double r_m = r_values[i] * 1e-6;
                    C_table[i][idx] = A * r_m + B;
                }
            }
        }
    }

    // Write header
    outFile << std::fixed << std::setprecision(3);
    outFile << "r (micron)";
    for (size_t idx = 0; idx < ics.size(); ++idx)
    {
        std::string element = elementNames[idx / 3];
        std::string component = componentNames[idx];
        outFile << "\tC_" << element << "_" << component
                << " (avg=" << std::scientific << std::setprecision(3)
                << volumeAverages[idx] << ")";
    }
    outFile << "\n";

    // Write data
    for (size_t i = 0; i < r_values.size(); ++i)
    {
        outFile << std::fixed << std::setprecision(3) << r_values[i];
        for (size_t j = 0; j < 18; ++j)
        {
            outFile << "\t" << std::scientific << std::setprecision(6) << C_table[i][j];
        }
        outFile << "\n";
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