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

#ifndef TIME_STEP_CALCULATION_H
#define TIME_STEP_CALCULATION_H

#include <vector>

/**
 * @brief This routine calculates the time step length 
 * by dividing the time intervals provided in input
 * in a fixed number of time steps (also set by input).
 * 
 * @author D. Pizzocri
 * @author T. Barani
 * 
 */
double TimeStepCalculation(
    int Input_history_points,
    double Time_h,
    std::vector<double> Time_input,
    double  Number_of_time_steps_per_interval
);

#endif // TIME_STEP_CALCULATION_H