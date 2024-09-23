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
//  Year: 2023                                                                      //
//  Authors: E. Travaglia                                             //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

/**
 * @brief This is a file required for TRANSURANUS-SCIANTIX coupling.
 * With this file, two functions are defined (getSciantixOptions and callSciantix) in SCIANTIX.
 * These two functions are also defined in TRANSURANUS, and are essentially required to create the
 * communication channel between the two codes.
 *
 * @author E. Travaglia
 *
 */

#ifndef TUSRCCOUPLING_H
#define TUSRCCOUPLING_H

#ifdef __cplusplus
extern "C"
{
  #endif
  void getSciantixOptions(int options[], double scaling_factors[]);
  void callSciantix(int options[], double history[], double variables[],
                    double scaling_factors[], double diffusion_modes[]);

  #ifdef __cplusplus
}
#endif

#endif // TUSRCCOUPLING_H