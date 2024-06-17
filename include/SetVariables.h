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

#include <vector>

#include "PhysicsVariableDeclaration.h"
#include "HistoryVariableDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "InputVariableDeclaration.h"
#include "SciantixDiffusionModeDeclaration.h"
#include "SciantixScalingFactorDeclaration.h"

#include "MapHistoryVariable.h"
#include "MapSciantixVariable.h"
#include "MapInputVariable.h"
#include "MapPhysicsVariable.h"


/**
 * \brief This routine initializes vectors that hold physics variables, history variables,
 * Sciantix variables, and input variables. It also sets up the diffusion modes,
 * maps, and scaling factors required for the simulation. The function ensures that
 * each variable is correctly initialized based on the model options provided by the user.
 *
 * @param Sciantix_options Array of integers specifying user-selected options for various simulation models.
 * @param Sciantix_history Array of doubles used to store historical variables of the simulation.
 * @param Sciantix_variables Array of doubles representing the current variables in the simulation.
 * @param Sciantix_scaling_factors Array of doubles used for scaling various parameters within the model.
 * @param Sciantix_diffusion_modes Array of doubles that describe diffusion modes used in gas behavior modeling.
 */
void SetVariables(int Sciantix_options[], double Sciantix_history[], double Sciantix_variables[], double Sciantix_scaling_factors[], double Sciantix_diffusion_modes[]);
