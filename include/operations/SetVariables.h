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
//  Version: 2.2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SET_VARIABLE_H
#define SET_VARIABLE_H

#include <vector>

#include "InputVariable.h"
#include "SciantixArray.h"
#include "SetVariablesFunctions.h"

/**
 * @brief This routine initializes vectors that hold physics variables, history variables,
 * Sciantix variables, and input variables. It also sets up the diffusion modes,
 * maps, and scaling factors required for the simulation. The function ensures that
 * each variable is correctly initialized based on the model options provided by the user.
 *
 * @param Sciantix_options Array of integers specifying user-selected options for various simulation
 * models.
 * @param Sciantix_history Array of doubles used to store historical variables of the simulation.
 * @param Sciantix_variables Array of doubles representing the current variables in the simulation.
 * @param Sciantix_scaling_factors Array of doubles used for scaling various parameters within the
 * model.
 * @param Sciantix_diffusion_modes Array of doubles that describe diffusion modes used in gas
 * behavior modeling.
 * @param input_variable Reference to SciantixArray of InputVariable objects.
 * @param history_variable Reference to SciantixArray of SciantixVariable objects for history tracking.
 * @param sciantix_variable Reference to SciantixArray of SciantixVariable objects.
 * @param physics_variable Reference to SciantixArray of SciantixVariable objects for physics models.
 * @param modes_initial_conditions Vector of doubles for initial conditions of diffusion modes.
 * @param scaling_factors Reference to SciantixArray of Variable objects for user-defined scaling.
 *
 * @author D. Pizzocri
 * @author T. Barani
 * @author G. Zullo
 * @author F. Bastien
 *
 */
void SetVariables(int                              Sciantix_options[],
                  double                           Sciantix_history[],
                  double                           Sciantix_variables[],
                  double                           Sciantix_scaling_factors[],
                  double                           Sciantix_diffusion_modes[],
                  SciantixArray<InputVariable>&    input_variable,
                  SciantixArray<SciantixVariable>& history_variable,
                  SciantixArray<SciantixVariable>& sciantix_variable,
                  SciantixArray<SciantixVariable>& physics_variable,
                  std::vector<double>&             modes_initial_conditions,
                  SciantixArray<Variable>&         scaling_factors);

#endif