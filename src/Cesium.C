#include "Cesium.h"

/// Helium

void Cesium()
{
	gas.emplace_back();
	int index = int(gas.size()) - 1;
	gas[index].setName("Cs133");
	gas[index].setAtomicNumber(55);
	gas[index].setVanDerWaalsVolume(9.97e-30);
	gas[index].setDecayRate(0.0);
	gas[index].setPrecursorFactor(1.00);
    gas[index].setReleaseRateCoefficient(0.0);
}