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

#include "Xenon.h"

void Xenon()
{
	gas.emplace_back();
	int index = int(gas.size()) - 1;

	gas[index].setName("Xe");
	gas[index].setAtomicNumber(54);
	gas[index].setMassNumber(135);
	gas[index].setVanDerWaalsVolume(8.48e-29);
	gas[index].setDecayRate(0.0);
	gas[index].setPrecursorFactor(1.00);

	gas.emplace_back();
	++index;
	
	gas[index].setName("Xe133");
	gas[index].setAtomicNumber(54);
	gas[index].setMassNumber(133);
	gas[index].setVanDerWaalsVolume(8.48e-29);
	gas[index].setDecayRate(1.53e-6);
	gas[index].setPrecursorFactor(1.25);
}
