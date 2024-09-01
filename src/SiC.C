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

#include "SiC.h"

/**
 * @brief Defines the physical properties of the SiC matrix.
 */
void SiC()
{
	matrix.emplace_back();
	int index = int(matrix.size()) - 1;
	matrix[index].setName("SiC");
	matrix[index].setRef("\n\t");
	matrix[index].setDensity(3160); //(kg/m3) "A review of TRISO fuel performance model" J. J. Powers, B. D. Wirth
	matrix[index].setGrainBoundaryMobility(int(input_variable[iv["iGrainGrowth"]].getValue())); //did not found
	// matrix[index].setSurfaceTension(0.626); // !!did not find!!
	matrix[index].setFFinfluenceRadius(1.0e-9); // (m) //to be discussed (don't know how to calculate it with SRIM)
	matrix[index].setFFrange(9.75e-6); // (m) determined by SRIM simulation (some perplexities and criticism about the results)
	// matrix[index].setSchottkyVolume(4.09e-29); // !!don't know how to calculate, SHOULD BE CHANGED!!
	// matrix[index].setOIS(1.17e-30); // (m3) octahedral interstitial site volume (sphalerite structure, approximated to fcc) ==> should discuss how much it is right
	// matrix[index].setSemidihedralAngle(1.052); // (rad) ref. "Dihedral angles in silicon carbide" A. Gubernat, L. Stobiersky
	// matrix[index].setGrainBoundaryThickness(5.0e-10); // (m) !!did not find!!
	// matrix[index].setLenticularShapeFactor(); // !!to be discussed! How to calculate?? Is it proper to be calculated with Triso (no restructuring)??!!
	// matrix[index].setGrainRadius(sciantix_variable[sv["Grain radius"]].getFinalValue()); // "Grain size dependence of hardness in nanocrystalline silicon carbide" C. Pan et al. (valore medio di 6 nm), altri propongono anche meno 
	matrix[index].setHealingTemperatureThreshold((2744 + 273.15)/2); // helf of the melting temperature by now (prolly va bene lo stesso, però di natura è fragile quindi avrà lo stesso una temperatura di healing???)
	// matrix[index].setGrainBoundaryVacancyDiffusivity(int(input_variable[iv["iGrainBoundaryVacancyDiffusivity"]].getValue())); // (m2/s) !!did not find!!
	// matrix[index].setPoreNucleationRate(); // !!did not find!!
	matrix[index].setPoreResolutionRate(); // Grain boundary phenomena not analyzed due to a lack of informations
	matrix[index].setPoreTrappingRate(); // 
	// matrix[index].setLatticeParameter(4.3596); // [A] ref "https://www.ioffe.ru/SVA/NSM/Semicond/SiC/basic.html#:~:text=Basic%20Parameters%20of%20Silicon%20Carbide%20%28SiC%29%20Silicon%20carbide,unit%20cell%2C%20wurtzile%29%3B%2015R%20-SiC%20%28rhombohedral%20unit%20cell%29." and
	// "https://www.researchgate.net/figure/Calculated-lattice-parameters-of-SiC-polytypes_tbl1_50348600"
	matrix[index].setIs_shell(int(input_variable[iv["is_shell"]].getValue()));
}

//"Grain-boundary type and distribution in silicon carbide coatings and wafers" F.C.Trejo useful for grains sizes and diffusion models (risultati leggermente più bassi da consultare)