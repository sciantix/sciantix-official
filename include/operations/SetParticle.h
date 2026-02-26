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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SET_PARTICLE_H
#define SET_PARTICLE_H

#include <vector>
#include "Particle.h"
#include "SciantixArray.h"

/**
 * @brief Sets up the particle properties in the simulation.
 * 
 * This function initializes and maps properties
 * used in the simulation. Each particle is set up with specific attributes and then mapped
 * for easy access throughout the simulation.
 * 
 * @author G. Zullo
 * @author F. Bastien
 */

void molybdenum(SciantixArray<Particle> &particle);
void ruthenium(SciantixArray<Particle> &particle);
void rhodium(SciantixArray<Particle> &particle);
void technetium(SciantixArray<Particle> &particle);
void palladium(SciantixArray<Particle> &particle);

#endif // SET_PARTICLE_H