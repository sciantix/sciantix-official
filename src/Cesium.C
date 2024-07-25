#include "Cesium.h"

void Cesium()
{
	gas.emplace_back();
	int index = int(gas.size() - 1);
	gas[index].setName("Cs");
	gas[index].setAtomicNumber(55);
	gas[index].setMassNumber(137);
	gas[index].setVanDerWaalsVolume(1.69e-28);
	gas[index].setDecayRate(7.28e-10);
	gas[index].setPrecursorFactor(1);
	gas[index].setGibbsEnergy(49789.7);
}