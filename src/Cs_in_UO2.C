#include "Cs_in_UO2.h"

void Cs_in_UO2()
{
	sciantix_system.emplace_back();
	int index = int(sciantix_system.size() - 1);

	sciantix_system[index].setName("Cs in UO2");
	sciantix_system[index].setGasName("Cs");
	sciantix_system[index].setMatrixName("UO2");
	sciantix_system[index].setRestructuredMatrix(0);
	sciantix_system[index].setChemicalBehaviour(1);
	sciantix_system[index].setYield(0.0);
	sciantix_system[index].setRadiusInLattice(0.21e-9);
	sciantix_system[index].setVolumeInLattice(matrix[sma["UO2"]].getSchottkyVolume());
	sciantix_system[index].setHenryConstant(0.0);
	sciantix_system[index].setProductionRate(1);
	sciantix_system[index].setFissionGasDiffusivity(int(input_variable[iv["iFGDiffusionCoefficient"]].getValue()));
	sciantix_system[index].setBubbleDiffusivity(int(input_variable[iv["iBubbleDiffusivity"]].getValue()));
	sciantix_system[index].setResolutionRate(99);
	sciantix_system[index].setTrappingRate(99);
	sciantix_system[index].setNucleationRate(99);
}