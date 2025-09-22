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

#include <string>
#include <vector>

void readFirstColumn(const std::string& filename, std::vector<double>& first_column);
void interpolateTimes(const std::vector<double>& time_vector,
                      std::vector<double>& interpolated_times, double num_bins);
void writeInterpolatedTimes(const std::string& filename,
                            const std::vector<double>& time,
                            const std::vector<double>& interpolated_times);

#endif // TIME_MANAGER_H