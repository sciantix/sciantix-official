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

#include "GasDeclaration.h"
#include "ModelDeclaration.h"
#include "InputVariableDeclaration.h"
#include "HistoryVariableDeclaration.h"
#include "MatrixDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "SystemDeclaration.h"
#include "SolverDeclaration.h"
#include "PhysicsVariableDeclaration.h"

#include "MapInputVariable.h"
#include "MapHistoryVariable.h"
#include "MapPhysicsVariable.h"
#include "MapGas.h"
#include "MapModel.h"
#include "MapMatrix.h"
#include "MapSciantixVariable.h"
#include "MapSystem.h"

#include "GrainGrowth.h"
#include "GrainBoundarySweeping.h"
#include "GasProduction.h"
#include "GasDiffusion.h"
#include "IntraGranularBubbleEvolution.h"
#include "Burnup.h"
#include "EffectiveBurnup.h"
#include "InterGranularBubbleEvolution.h"
#include "GrainBoundaryMicroCracking.h"
#include "GrainBoundaryVenting.h"
#include "HighBurnupStructureFormation.h"
#include "StoichiometryDeviation.h"
#include "EnvironmentComposition.h"
#include "UO2Thermochemistry.h"

#include "ErrorMessages.h"

#include <iostream>

#include <cmath>

void SetModel();
