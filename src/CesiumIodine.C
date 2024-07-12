#include "CesiumIodine.h"

/// CesiumIodine

void CesiumIodine()
{
	gas.emplace_back();
	int index = int(gas.size() - 1);
	gas[index].setName("CsI");
	gas[index].setAtomicNumber(0);
	gas[index].setVanDerWaalsVolume(2.623e-28); //by addition of the I- and Cs+ van der waals Volume
	gas[index].setDecayRate(0.0);
	gas[index].setPrecursorFactor(1);
	gas[index].setGibbsEnergy(-333700);
}