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
#include "PropertyVariableDeclaration.h"
#include "InputVariableDeclaration.h"
#include "InputPropertyDeclaration.h"
#include "SciantixDiffusionModeDeclaration.h"
#include "SciantixScalingFactorDeclaration.h"

#include "MapHistoryVariable.h"
#include "MapSciantixVariable.h"
#include "MapPropertyVariable.h"
#include "MapInputVariable.h"
#include "MapInputProperty.h"
#include "MapPhysicsVariable.h"

void SetVariables(int Sciantix_options[], int Property_options[], double Sciantix_history[], double Sciantix_variables[], double Sciantix_properties[], double Sciantix_scaling_factors[], double Sciantix_diffusion_modes[]);
