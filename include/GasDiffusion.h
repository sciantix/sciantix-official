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

/**
 * @brief Defines models for gas diffusion within the fuel grain.
 * 
 * This function computes diffusion models for gas atoms within the fuel grain
 * based on the selected diffusion solver option.
 */
void GasDiffusion();

/**
 * @brief Defines diffusion models using the spectral diffusion with one equation.
 */
void defineSpectralDiffusion1Equation();

/**
 * @brief Defines diffusion models using the spectral diffusion with two equations.
 */
void defineSpectralDiffusion2Equations();

/**
 * @brief Handles unsupported diffusion solver options.
 */
void errorHandling();

#endif // GAS_DIFFUSION_H
