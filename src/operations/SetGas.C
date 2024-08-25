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
    Gas xe;    
    xe.setName("Xe");
	xe.setAtomicNumber(54);
	xe.setMassNumber(135);
	xe.setVanDerWaalsVolume(8.48e-29);
	xe.setDecayRate(0.0);
	xe.setPrecursorFactor(1.00);
    gas.push(xe);

	Gas xe133;	
	xe133.setName("Xe133");
	xe133.setAtomicNumber(54);
	xe133.setMassNumber(133);
	xe133.setVanDerWaalsVolume(8.48e-29);
	xe133.setDecayRate(1.53e-6);
	xe133.setPrecursorFactor(1.25);
    gas.push(xe133);
}

void krypton(SciantixArray<Gas> &gas)
{
    Gas kr;
	kr.setName("Kr");
	kr.setAtomicNumber(36);
	kr.setVanDerWaalsVolume(6.61e-29);
	kr.setDecayRate(0.0);
	kr.setPrecursorFactor(1.00);
    gas.push(kr);

	Gas kr85m;
	kr85m.setName("Kr85m");
	kr85m.setAtomicNumber(36);
	kr85m.setMassNumber(85);
	kr85m.setVanDerWaalsVolume(6.61e-29);
	kr85m.setDecayRate(4.3e-5);
	kr85m.setPrecursorFactor(1.31);
    gas.push(kr85m);
}

void helium(SciantixArray<Gas> &gas)
{
    Gas he;
	he.setName("He");
	he.setAtomicNumber(2);
	he.setVanDerWaalsVolume(9.97e-30);
	he.setDecayRate(0.0);
	he.setPrecursorFactor(1.00);
    gas.push(he);
}