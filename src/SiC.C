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


template <typename T>
void SiC(T& container)
{

    container.emplace_back();
    int index = static_cast<int>(container.size()) - 1; 

    container[index].setName("SiC");
    container[index].setRef("\n\t");
    container[index].setDensity(3160); // (kg/m3)
    // obj.setGrainBoundaryMobility(int(input_variable[iv["iGrainGrowth"]].getValue()));
    // obj.setSurfaceTension(0.626); // 
    // obj.setFFinfluenceRadius(1.0e-9); // (m)
    // obj.setFFrange(9.75e-6); // (m)
    // obj.setSchottkyVolume(4.09e-29); // 
    // obj.setOIS(1.17e-30); // (m3) 
    // obj.setSemidihedralAngle(1.052); // (rad) 
    // obj.setGrainBoundaryThickness(5.0e-10); // (m) 
    // obj.setLenticularShapeFactor(); // 
    // obj.setGrainRadius(sciantix_variable[sv["Grain radius"]].getFinalValue()); // Non impostato
    // obj.setHealingTemperatureThreshold((2744 + 273.15)/2); // Temperatura media di fusione
    // obj.setGrainBoundaryVacancyDiffusivity(int(input_variable[iv["iGrainBoundaryVacancyDiffusivity"]].getValue())); // Non impostato
    // obj.setPoreNucleationRate(); 
    // obj.setPoreResolutionRate(); 
    // obj.setPoreTrappingRate();
    // obj.setLatticeParameter(4.3596); // [A] 
}

template void SiC<std::vector<Shell>>(std::vector<Shell>&);
template void SiC<std::vector<Matrix>>(std::vector<Matrix>&);
