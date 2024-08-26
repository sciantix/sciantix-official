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
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SCIANTIX_H
#define SCIANTIX_H

#include "Simulation.h"

/**
 * @brief Executes the main SCIANTIX simulation program.
 * It runs the setup and execution of various simulation stages,
 * including gas behavior, matrix transformations, environmental interactions,
 * and system updates. It concludes with generating output files and updating the system's history.
 *
 * @param Sciantix_options Array of integers specifying simulation options.
 * @param Sciantix_history Array of doubles sets up with the old variables of the simulation.
 * @param Sciantix_variables Array of doubles representing current variables in the simulation.
 * @param Sciantix_scaling_factors Array of doubles used to scale various parameters within the model.
 * @param Sciantix_diffusion_modes Array of doubles representing diffusion modes used in the simulation.
 * 
 * @author D. Pizzocri
 * @author T. Barani
 * @author G. Zullo
 * 
 */
void Sciantix(int Sciantix_options[], double Sciantix_history[], double Sciantix_variables[], double Sciantix_scaling_factors[], double Sciantix_diffusion_modes[]);

#endif // SCIANTIX_H