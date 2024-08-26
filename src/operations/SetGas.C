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

#include "SetGas.h"
#include "Simulation.h"

void Simulation::setGas()
{
    xenon(gas);
    krypton(gas);
    helium(gas);
}

void xenon(SciantixArray<Gas> &gas)
{
    Gas gas_;    
    gas_.setName("Xe");
	gas_.setAtomicNumber(54);
	gas_.setMassNumber(135);
	gas_.setVanDerWaalsVolume(8.48e-29);
	gas_.setDecayRate(0.0);
	gas_.setPrecursorFactor(1.00);
    gas.push(gas_);

	gas_.setName("Xe133");
	gas_.setAtomicNumber(54);
	gas_.setMassNumber(133);
	gas_.setVanDerWaalsVolume(8.48e-29);
	gas_.setDecayRate(1.53e-6);
	gas_.setPrecursorFactor(1.25);
    gas.push(gas_);
}

void krypton(SciantixArray<Gas> &gas)
{
    Gas gas_;
	gas_.setName("Kr");
	gas_.setAtomicNumber(36);
	gas_.setVanDerWaalsVolume(6.61e-29);
	gas_.setDecayRate(0.0);
	gas_.setPrecursorFactor(1.00);
    gas.push(gas_);

	gas_.setName("Kr85m");
	gas_.setAtomicNumber(36);
	gas_.setMassNumber(85);
	gas_.setVanDerWaalsVolume(6.61e-29);
	gas_.setDecayRate(4.3e-5);
	gas_.setPrecursorFactor(1.31);
    gas.push(gas_);
}

void helium(SciantixArray<Gas> &gas)
{
    Gas gas_;
	gas_.setName("He");
	gas_.setAtomicNumber(2);
	gas_.setVanDerWaalsVolume(9.97e-30);
	gas_.setDecayRate(0.0);
	gas_.setPrecursorFactor(1.00);
    gas.push(gas_);
}