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

#include "SetParticle.h"
#include "Simulation.h"

void Simulation::setParticle()
{
    molybdenum(particle);
    ruthenium(particle);
    rhodium(particle);
    technetium(particle);
    palladium(particle);
}

void molybdenum(SciantixArray<Particle> &particle)
{
    Particle particle_;    
    particle_.setName("Mo");
    particle_.setAtomicNumber(42);
    particle_.setMassNumber(95);
    particle_.setDecayRate(0.0);
    particle_.setChemicallyActive(1.00);
    particle_.setPrecursorFactor(1.00);
    particle.push(particle_);
}

void ruthenium(SciantixArray<Particle> &particle)
{
    Particle particle_;    
    particle_.setName("Ru");
    particle_.setAtomicNumber(44);
    particle_.setMassNumber(101);
    particle_.setDecayRate(0.0);
    particle_.setChemicallyActive(1.00);
    particle_.setPrecursorFactor(1.00);
    particle.push(particle_);
}

void rhodium(SciantixArray<Particle> &particle)
{
    Particle particle_;    
    particle_.setName("Rh");
    particle_.setAtomicNumber(45);
    particle_.setMassNumber(103);
    particle_.setDecayRate(0.0);
    particle_.setChemicallyActive(1.00);
    particle_.setPrecursorFactor(1.00);
    particle.push(particle_);
}

void technetium(SciantixArray<Particle> &particle)
{
    Particle particle_;    
    particle_.setName("Tc");
    particle_.setAtomicNumber(43);
    particle_.setMassNumber(99);
    particle_.setDecayRate(0.0);
    particle_.setChemicallyActive(1.00);
    particle_.setPrecursorFactor(1.00);
    particle.push(particle_);
}

void palladium(SciantixArray<Particle> &particle)
{
    Particle particle_;    
    particle_.setName("Pd");
    particle_.setAtomicNumber(46);
    particle_.setMassNumber(105);
    particle_.setDecayRate(0.0);
    particle_.setChemicallyActive(1.00);
    particle_.setPrecursorFactor(1.00);
    particle.push(particle_);
}
