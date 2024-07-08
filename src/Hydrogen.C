
#include "Hydrogen.h"

/// Hydrogen

void Hydrogen()
{
	gas.emplace_back();
	int index = int(gas.size()) - 1;
	gas[index].setName("H");
	gas[index].setAtomicNumber(1);
	gas[index].setVanDerWaalsVolume(7.24e-30);
	gas[index].setDecayRate(0.0);
	gas[index].setPrecursorFactor(1.00);
	gas[index].setReleaseRateCoefficient(2*7.14*pow(10,-6)/4);
}
