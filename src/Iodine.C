#include "Iodine.h"

/// Iodine

void Iodine()
{
	int index = int(gas.size() - 1);

	gas.emplace_back();
	gas[index].setName("I");
	gas[index].setAtomicNumber(53);
	gas[index].setMassNumber(131);
	gas[index].setVanDerWaalsVolume(9.33e-29);
	gas[index].setDecayRate(9.98e-7);
	gas[index].setPrecursorFactor(1);
	gas[index].setGibbsEnergy(70282.832);
}