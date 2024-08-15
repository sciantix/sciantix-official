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
 * @file GasDiffusion.h
 * @brief Declares functions for modeling gas diffusion within the fuel grain.
 *
 * This header file contains the declarations of functions used to model the diffusion
 * of gas atoms (e.g., xenon) within the fuel grain matrix, based on different spectral
 * diffusion equations and solver options.
 * 
 * @author G. Zullo
 */

/**
 * @brief The GasDiffusion function sets up and computes the diffusion models for gas atoms
 * within the fuel grain matrix, taking into account the selected diffusion solver option.
 */
void GasDiffusion();

/**
 * @brief Defines the spectral diffusion model using a single equation.
 * 
 * This function sets up the diffusion model for gas atoms within the fuel grain
 * using one equation in the spectral diffusion approach.
 */
void defineSpectralDiffusion1Equation();

/**
 * @brief Defines the spectral diffusion model using two equations.
 * 
 * This function sets up the diffusion model for gas atoms within the fuel grain
 * using two equations in the spectral diffusion approach.
 */
void defineSpectralDiffusion2Equations();

/**
 * @brief Defines the spectral diffusion model using three equations.
 * 
 * This function sets up the diffusion model for gas atoms within the fuel grain
 * using three equations in the spectral diffusion approach:
 * 
 * - **First Equation**: For xenon in the non-restructured matrix with a dynamic solution.
 * - **Second Equation**: For xenon in the non-restructured matrix within intragranular bubbles.
 * - **Third Equation**: For xenon in the restructured matrix.
 */
void defineSpectralDiffusion3Equations();

/**
 * @brief Handles errors related to unsupported diffusion solver options.
 * 
 * This function manages cases where an unsupported diffusion solver option is selected,
 * ensuring that appropriate error messages are provided to the user.
 */
void errorHandling();

/**
 * @brief Retrieves diffusion modes for a specified gas.
 * 
 * This function returns a pointer to the array of diffusion modes corresponding to the
 * specified gas name.
 * 
 * @param gas_name The name of the gas (e.g., xenon) for which diffusion modes are requested.
 * @return A pointer to an array of diffusion modes for the specified gas.
 */
double* getDiffusionModes(std::string gas_name);

/**
 * @brief Retrieves diffusion mode solutions for a specified gas.
 * 
 * This function returns a pointer to the array of diffusion mode solutions corresponding
 * to the specified gas name.
 * 
 * @param gas_name The name of the gas (e.g., xenon) for which diffusion mode solutions are requested.
 * @return A pointer to an array of diffusion mode solutions for the specified gas.
 */
double* getDiffusionModesSolution(std::string gas_name);

/**
 * @brief Retrieves diffusion modes related to bubbles for a specified gas.
 * 
 * This function returns a pointer to the array of diffusion modes related to bubble formation
 * corresponding to the specified gas name.
 * 
 * @param gas_name The name of the gas (e.g., xenon) for which diffusion modes related to bubbles are requested.
 * @return A pointer to an array of diffusion modes related to bubbles for the specified gas.
 */
double* getDiffusionModesBubbles(std::string gas_name);

#endif // GAS_DIFFUSION_H
