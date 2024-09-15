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

#include "MOX.h"

/**
 * @brief Defines the physical properties of the MOX matrix.
 */
void MOX(double enrichment)
{
	matrix.emplace_back();
	int index = int(matrix.size()) - 1;

	matrix[index].setInitialUraniumComposition({double(sciantix_variable[sv["U235"]].getFinalValue()), 
												double(sciantix_variable[sv["U238"]].getFinalValue())});
	matrix[index].setInitialPlutoniumComposition({double(sciantix_variable[sv["Pu238"]].getFinalValue()),
												 double(sciantix_variable[sv["Pu239"]].getFinalValue()), 
												 double(sciantix_variable[sv["Pu240"]].getFinalValue()),
												 double(sciantix_variable[sv["Pu241"]].getFinalValue()), 
												 double(sciantix_variable[sv["Pu242"]].getFinalValue())});																								 
	matrix[index].setMoxPuEnrichment(sciantix_variable[sv["MOX PuO2 percentage"]].getFinalValue());
	matrix[index].setName("MOX");
	matrix[index].setRef("\n\t");
	matrix[index].setDensity(10960.0 + 490*enrichment); //(kg/m3) reference from "A review of the thermohpysical properties of MOX and UO2 fuels" by Juan J. Carbajo
	matrix[index].setGrainBoundaryMobility(int(input_variable[iv["iGrainGrowth"]].getValue()));
	matrix[index].setSurfaceTension(0.626); // (N/m) reference from "A methodology to predict a fission gas release ratio of MOX fuel with heterogeneous microstructure" by K. Kitano
	matrix[index].setFFinfluenceRadius(1.0e-9); // (m)
	matrix[index].setFFrange(6.0e-6); // (m)
	matrix[index].setSchottkyVolume(4.09e-29); // sum of the volumes of the atoms (by now kept the same as UO2)
	matrix[index].setOIS(7.8e-30); // (m3) ???
	matrix[index].setSemidihedralAngle(0.97); // (rad) 
	matrix[index].setGrainBoundaryThickness(5.0e-10); // (m)
	matrix[index].setLenticularShapeFactor(0.168610764); // function in "Physics-based modelling of fission gas swelling and release in UO2 applied to integral fuel rod analysis" by Giovanni Pastorea, Lelio Luzzi a,∗, Valentino Di Marcello b, Paul Van Uffelenb
	matrix[index].setGrainRadius(sciantix_variable[sv["Grain radius"]].getFinalValue()); //
	matrix[index].setHealingTemperatureThreshold((2744 + 273.15)/2); // half of the melting temperatura by now
	matrix[index].setGrainBoundaryVacancyDiffusivity(int(input_variable[iv["iGrainBoundaryVacancyDiffusivity"]].getValue())); // (m2/s)
	matrix[index].setPoreNucleationRate();
	matrix[index].setPoreResolutionRate();
	matrix[index].setPoreTrappingRate(); // 
	matrix[index].setLatticeParameter(5.47 - 0.074*enrichment); //
}
