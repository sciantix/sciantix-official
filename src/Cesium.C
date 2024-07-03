#include "Cesium.h"

/// Iodine

void Cesium()
{
	gas.emplace_back();
	int index = int(gas.size() - 1);
	gas[index].setName("Cs");
	gas[index].setAtomicNumber(55);
	gas[index].setVanDerWaalsVolume(1.69e-28);
	gas[index].setDecayRate(0.0);
	gas[index].setPrecursorFactor(1);

	++index;
	gas.emplace_back();
	gas[index].setName("Cs137");
	gas[index].setAtomicNumber(53);
	gas[index].setMassNumber(137);
	gas[index].setVanDerWaalsVolume(1.69e-28);
	gas[index].setDecayRate(7.28e-10);
	gas[index].setPrecursorFactor(1);
}