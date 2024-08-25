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
//  Year: 2023                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef GAS_DIFFUSION_H
#define GAS_DIFFUSION_H

#include "Simulation.h"

/**
 * @brief Defines diffusion models using the spectral diffusion with one equation.
 * 
 * @author D. Pizzocri
 * @author T. Barani
 * @author G. Zullo
 * 
 */
void defineSpectralDiffusion1Equation(SciantixArray<System> &sciantix_system, SciantixArray<Model> &model, int n_modes);

/**
 * @brief Defines diffusion models using the spectral diffusion with two equations.
 * 
 * @author D. Pizzocri
 * @author T. Barani
 * @author G. Zullo
 * 
 */
void defineSpectralDiffusion2Equations(SciantixArray<System> &sciantix_system, SciantixArray<Model> &model, int n_modes);

/**
 * @brief Defines diffusion models using the spectral diffusion with three equations.
 *
 * The first equation is for xenon in non-restructured matrix - dynamic solution
 * The second equation is for xenon in non-restructured matrix - intragranular bubbles
 * The third equation is for xenon in restructured matrix
 * 
 * @author G. Zullo
 *
 */
void defineSpectralDiffusion3Equations(SciantixArray<System> &sciantix_system, SciantixArray<Model> &model, 
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> physics_variable, int n_modes);

/**
 * @brief Handles unsupported diffusion solver options.
 */
void errorHandling(SciantixArray<InputVariable> input_variable);

#endif // GAS_DIFFUSION_H
