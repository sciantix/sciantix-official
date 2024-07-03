#include "Iodine.h"

/// Iodine

void Iodine()
{
	gas.emplace_back();
	int index = int(gas.size() - 1);
	gas[index].setName("I");
	gas[index].setAtomicNumber(53);
	gas[index].setVanDerWaalsVolume(9.33e-29);
	gas[index].setDecayRate(0.0);
	gas[index].setPrecursorFactor(1);

	++index;
	gas.emplace_back();
	gas[index].setName("I131");
	gas[index].setAtomicNumber(53);
	gas[index].setMassNumber(131);
	gas[index].setVanDerWaalsVolume(9.33e-29);
	gas[index].setDecayRate(9.98e-7);
	gas[index].setPrecursorFactor(1);
}