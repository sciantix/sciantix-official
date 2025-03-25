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

//This Function is dedicated to organize the inputs and outputs for the verification

#ifndef TIME_MANAGER_H
#define TIME_MANAGER_H

#include <iostream>
#include <fstream>
#include <vector>
#include <iomanip>

void readFirstColumn(const std::string& filename, std::vector<double>& first_column)
{
    std::ifstream input_file(filename);
    if (!input_file.is_open())
    {
        std::cerr << "Could not open the file for reading!" << std::endl;
        return;
    }

    double col1;
    std::string line;

    // Read only the first column
    while (input_file >> col1)
    {
        first_column.push_back(col1);
        input_file.ignore(std::numeric_limits<std::streamsize>::max(), '\n'); // Ignore the rest of the line
    }

    input_file.close();
};

void interpolateTimes(const std::vector<double>& time_vector, std::vector<double>& interpolated_times, double num_bins)
{
    if (time_vector.size() < 2)
    {
        std::cerr << "Error: time_vector must contain at least two values (start and end times)!" << std::endl;
        return;
    }

    double start_time = time_vector[0];
    double end_time = time_vector[1];

    // Calculate the step size (total number of intervals is num_bins)
    double step_size = (end_time - start_time) / num_bins;

    // Clear the output vector before filling it
    interpolated_times.clear();

    // Fill the interpolated_times vector with equally spaced points
    for (int i = 0; i <= num_bins; ++i)
    {
        interpolated_times.push_back(start_time + i * step_size);
    }
};

void writeInterpolatedTimes(const std::string& filename, 
    const std::vector<double>& time, 
    const std::vector<double>& interpolated_times)
{
if (time.size() != interpolated_times.size()) {
std::cerr << "Error: Time and interpolated_times vectors must have the same size!" << std::endl;
return;
}

std::ofstream output_file(filename);
if (!output_file.is_open())
{
std::cerr << "Could not open the file for writing!" << std::endl;
return;
}

// Write the header
output_file << "Time (h)\tManufactured Solution (at/m³)\n";

// Write time values and corresponding interpolated times
for (size_t i = 0; i < time.size(); ++i)
{
output_file << std::setprecision(12) << std::fixed 
<< time[i] << "\t" << interpolated_times[i] << "\n";
}

output_file.close();
};

#endif // TIME_MANAGER_H