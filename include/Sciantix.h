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
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include <iostream>
#include <map>
#include <string>
#include <vector>
#include "SetVariables.h"
#include "MaterialDeclaration.h"
#include "SetGas.h"
#include "SetModel.h"
#include "SetMatrix.h"
#include "SetSystem.h"
#include "UpdateVariables.h"
#include "Output.h"
#include "Simulation.h"
#include "FiguresOfMerit.h"

/**
 * \brief Executes the main Sciantix simulation program.
 * It runs the setup and execution of various simulation stages,
 * including gas behavior, matrix transformations, environmental interactions,
 * and system updates. It concludes with generating output files and updating the system's history.
 *
 * @param Sciantix_options Array of integers specifying simulation options.
 * @param Sciantix_history Array of doubles sets up with the old variables of the simulation.
 * @param Sciantix_variables Array of doubles representing current variables in the simulation.
 * @param Sciantix_scaling_factors Array of doubles used to scale various parameters within the model.
 * @param Sciantix_diffusion_modes Array of doubles representing diffusion modes used in the simulation.
 * @param testFilePath String representing the file path where the simulation outputs will be written.
 */
void Sciantix(int Sciantix_options[], double Sciantix_history[], double Sciantix_variables[], double Sciantix_scaling_factors[], double Sciantix_diffusion_modes[], std::string testFilePath);
