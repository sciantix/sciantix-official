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

#ifndef TIME_MANAGER_H
#define TIME_MANAGER_H

#include <iostream>
#include <fstream>
#include <vector>
#include <iomanip>

void interpolateTimes(const std::string& filename, std::vector<double>& interpolated_times)
{
    // Read the start and end times from the file
    double start_time, end_time;

    std::ifstream input_file(filename);
    if (!input_file.is_open())
    {
        std::cerr << "Could not open the file!" << std::endl;
        return;
    }

    input_file >> start_time >> end_time;
    input_file.close();

    // Calculate the step size (total number of intervals is 99 because 100 points are required)
    double step_size = (end_time - start_time) / 99.0;

    // Fill the interpolated_times vector with 100 equally spaced points
    for (int i = 0; i < 100; ++i)
    {
        interpolated_times.push_back(start_time + i * step_size);
    }
};

#endif // TIME_MANAGER_H