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

#include "Krypton.h"

/// Krypton

void Krypton()
{
	gas.emplace_back();
	int index = int(gas.size() - 1);
	gas[index].setName("Kr");
	gas[index].setAtomicNumber(36);
	gas[index].setVanDerWaalsVolume(6.61e-29);
	gas[index].setDecayRate(0.0);
	gas[index].setPrecursorFactor(1.00);

	++index;
	gas.emplace_back();
	gas[index].setName("Kr85m");
	gas[index].setAtomicNumber(36);
	gas[index].setMassNumber(85);
	gas[index].setVanDerWaalsVolume(6.61e-29);
	gas[index].setDecayRate(4.3e-5);
	gas[index].setPrecursorFactor(1.31);
}
