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

#ifndef GAS_DIFFUSION_H
#define GAS_DIFFUSION_H

#include "ModelDeclaration.h"
#include "MapModel.h"
#include "SciantixVariableDeclaration.h"
#include "MapSciantixVariable.h"
#include "SystemDeclaration.h"
#include "MapSystem.h"
#include "SciantixDiffusionModeDeclaration.h"
#include "GasDeclaration.h"
#include "MapGas.h"
#include "SetMatrix.h"
#include "MatrixDeclaration.h"
#include "MapMatrix.h"

void GasDiffusion();
/**
 * Defines models for gas diffusion within the fuel grain.
 * 
 * This function computes diffusion models for gas atoms within the fuel grain
 * based on the selected diffusion solver option.
 */

void defineSpectralDiffusion1Equation();
/**
 * Defines diffusion models using the spectral diffusion with one equation.
 */

void defineSpectralDiffusion2Equations();
/**
 * Defines diffusion models using the spectral diffusion with two equations.
 */

void defineSpectralDiffusion3Equations();
/**
 * Defines diffusion models using the spectral diffusion with three equations.
 * 
 * The first equation is for xenon in non-restructured matrix - dynamic solution
 * The second equation is for xenon in non-restructured matrix - intragranular bubbles
 * The third equation is for xenon in restructured matrix
 * 
 */

void errorHandling();
/**
 * Handles unsupported diffusion solver options.
 */

#endif // GAS_DIFFUSION_H
