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

#ifndef GRAIN_BOUNDARY_VENTING_H
#define GRAIN_BOUNDARY_VENTING_H

#include "SciantixVariableDeclaration.h"
#include "HistoryVariableDeclaration.h"
#include "ModelDeclaration.h"
#include "SciantixScalingFactorDeclaration.h"
#include "SolverDeclaration.h"
#include "MapSciantixVariable.h"
#include "MapHistoryVariable.h"

void GrainBoundaryVenting();

double openPorosity(double fabrication_porosity);

double athermalVentingFactor(double open_p, double theta, double p, double l, double bu, double T, double F);
/**
 * @brief This function computes the athermal venting factor with a ANN-based fitting function
 * 
 * @author
 * A. Pagani
 * G. Zullo
 * 
*/

#endif // GRAIN_BOUNDARY_VENTING_H