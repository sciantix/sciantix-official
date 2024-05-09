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
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, T. Barani                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

/// TimeStepCalculation
/// This routine calculates the time step length
/// by dividing the time intervals provided in input
/// in a fixed number of time steps (also set by input).

#include "TimeStepCalculation.h"

double TimeStepCalculation( )
{
  double time_step(0.0);

  // Find the current time interval
  double lower_bound(0.0), upper_bound(0.0);
  for (int n=0; n<Input_history_points-1; n++)
    if (Time_h >= Time_input[n] && Time_h < Time_input[n+1])
    {
      lower_bound = Time_input[n];
      upper_bound = Time_input[n+1];
    }

  // Divide the interval in time steps
  time_step = (upper_bound - lower_bound) / Number_of_time_steps_per_interval;

  if ((Time_h+time_step) > upper_bound)
    {
    time_step = upper_bound - Time_h;

    }

  return time_step;
}
