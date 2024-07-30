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

#include "CheckBounds.h"

void checkBounds(){

    // History Variables
    history_variable[hv["Time"]].isWithinBounds(history_variable[hv["Time"]].getFinalValue());
    history_variable[hv["Time step number"]].isWithinBounds(history_variable[hv["Time step number"]].getFinalValue());
    history_variable[hv["Temperature"]].isWithinBounds(history_variable[hv["Temperature"]].getFinalValue());
    history_variable[hv["Fission rate"]].isWithinBounds(history_variable[hv["Fission rate"]].getFinalValue());
    history_variable[hv["Hydrostatic stress"]].isWithinBounds(history_variable[hv["Hydrostatic stress"]].getFinalValue());
    history_variable[hv["Steam pressure"]].isWithinBounds(history_variable[hv["Steam pressure"]].getFinalValue());

    // Sciantix Variables
	sciantix_variable[sv["Grain radius"]].isWithinBounds(sciantix_variable[sv["Grain radius"]].getFinalValue());
    sciantix_variable[sv["Xe produced"]].isWithinBounds(sciantix_variable[sv["Xe produced"]].getFinalValue());
    sciantix_variable[sv["Xe produced in HBS"]].isWithinBounds(sciantix_variable[sv["Xe produced in HBS"]].getFinalValue());
    sciantix_variable[sv["Xe in grain"]].isWithinBounds(sciantix_variable[sv["Xe in grain"]].getFinalValue());
    sciantix_variable[sv["Xe in grain HBS"]].isWithinBounds(sciantix_variable[sv["Xe in grain HBS"]].getFinalValue());
    sciantix_variable[sv["Xe in intragranular solution"]].isWithinBounds(sciantix_variable[sv["Xe in intragranular solution"]].getFinalValue());
    sciantix_variable[sv["Xe in intragranular bubbles"]].isWithinBounds(sciantix_variable[sv["Xe in intragranular bubbles"]].getFinalValue());
    sciantix_variable[sv["Xe at grain boundary"]].isWithinBounds( sciantix_variable[sv["Xe at grain boundary"]].getFinalValue());
    sciantix_variable[sv["Xe released"]].isWithinBounds(sciantix_variable[sv["Xe released"]].getFinalValue());
    sciantix_variable[sv["Kr produced"]].isWithinBounds(sciantix_variable[sv["Kr produced"]].getFinalValue());
    sciantix_variable[sv["Kr in grain"]].isWithinBounds(sciantix_variable[sv["Kr in grain"]].getFinalValue());
    sciantix_variable[sv["Kr in intragranular solution"]].isWithinBounds(sciantix_variable[sv["Kr in intragranular solution"]].getFinalValue());
    sciantix_variable[sv["Kr in intragranular bubbles"]].isWithinBounds(sciantix_variable[sv["Kr in intragranular bubbles"]].getFinalValue());
    sciantix_variable[sv["Kr at grain boundary"]].isWithinBounds(sciantix_variable[sv["Kr at grain boundary"]].getFinalValue());
    sciantix_variable[sv["Kr released"]].isWithinBounds(sciantix_variable[sv["Kr released"]].getFinalValue());
    sciantix_variable[sv["He produced"]].isWithinBounds(sciantix_variable[sv["He produced"]].getFinalValue());
    sciantix_variable[sv["He in grain"]].isWithinBounds(sciantix_variable[sv["He in grain"]].getFinalValue());
    sciantix_variable[sv["He in intragranular solution"]].isWithinBounds(sciantix_variable[sv["He in intragranular solution"]].getFinalValue());
    sciantix_variable[sv["He in intragranular bubbles"]].isWithinBounds(sciantix_variable[sv["He in intragranular bubbles"]].getFinalValue());
    sciantix_variable[sv["He at grain boundary"]].isWithinBounds(sciantix_variable[sv["He at grain boundary"]].getFinalValue());
    sciantix_variable[sv["He released"]].isWithinBounds(sciantix_variable[sv["He released"]].getFinalValue());
    sciantix_variable[sv["Intragranular bubble concentration"]].isWithinBounds(sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue());
    sciantix_variable[sv["Intragranular bubble radius"]].isWithinBounds(sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue());
    sciantix_variable[sv["Intragranular Xe atoms per bubble"]].isWithinBounds(sciantix_variable[sv["Intragranular Xe atoms per bubble"]].getFinalValue());
    sciantix_variable[sv["Intragranular Kr atoms per bubble"]].isWithinBounds(sciantix_variable[sv["Intragranular Kr atoms per bubble"]].getFinalValue());
    sciantix_variable[sv["Intragranular He atoms per bubble"]].isWithinBounds(sciantix_variable[sv["Intragranular He atoms per bubble"]].getFinalValue());
    sciantix_variable[sv["Intragranular gas bubble swelling"]].isWithinBounds(sciantix_variable[sv["Intragranular gas bubble swelling"]].getFinalValue());
    sciantix_variable[sv["Intragranular gas solution swelling"]].isWithinBounds(sciantix_variable[sv["Intragranular gas solution swelling"]].getFinalValue());
    sciantix_variable[sv["Intergranular bubble concentration"]].isWithinBounds(sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue());
    sciantix_variable[sv["Intergranular Xe atoms per bubble"]].isWithinBounds(sciantix_variable[sv["Intergranular Xe atoms per bubble"]].getFinalValue());
    sciantix_variable[sv["Intergranular Kr atoms per bubble"]].isWithinBounds(sciantix_variable[sv["Intergranular Kr atoms per bubble"]].getFinalValue());
    sciantix_variable[sv["Intergranular He atoms per bubble"]].isWithinBounds(sciantix_variable[sv["Intergranular He atoms per bubble"]].getFinalValue());
    sciantix_variable[sv["Intergranular atoms per bubble"]].isWithinBounds(sciantix_variable[sv["Intergranular atoms per bubble"]].getFinalValue());
    sciantix_variable[sv["Intergranular vacancies per bubble"]].isWithinBounds(sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue());
    sciantix_variable[sv["Intergranular bubble radius"]].isWithinBounds(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue());
    sciantix_variable[sv["Intergranular bubble area"]].isWithinBounds(sciantix_variable[sv["Intergranular bubble area"]].getFinalValue());
    sciantix_variable[sv["Intergranular bubble volume"]].isWithinBounds(sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue());
    sciantix_variable[sv["Intergranular fractional coverage"]].isWithinBounds(sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue());
    sciantix_variable[sv["Intergranular saturation fractional coverage"]].isWithinBounds(sciantix_variable[sv["Intergranular saturation fractional coverage"]].getFinalValue());
    sciantix_variable[sv["Intergranular gas swelling"]].isWithinBounds(sciantix_variable[sv["Intergranular gas swelling"]].getFinalValue());
    sciantix_variable[sv["Intergranular fractional intactness"]].isWithinBounds(sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue());
    sciantix_variable[sv["Burnup"]].isWithinBounds(sciantix_variable[sv["Burnup"]].getFinalValue());
    sciantix_variable[sv["Effective burnup"]].isWithinBounds(sciantix_variable[sv["Effective burnup"]].getFinalValue());
    sciantix_variable[sv["Fuel density"]].isWithinBounds(sciantix_variable[sv["Fuel density"]].getFinalValue());
    sciantix_variable[sv["U234"]].isWithinBounds(sciantix_variable[sv["U234"]].getFinalValue());
    sciantix_variable[sv["U235"]].isWithinBounds(sciantix_variable[sv["U235"]].getFinalValue());
    sciantix_variable[sv["U236"]].isWithinBounds(sciantix_variable[sv["U236"]].getFinalValue());
    sciantix_variable[sv["U237"]].isWithinBounds(sciantix_variable[sv["U237"]].getFinalValue());
    sciantix_variable[sv["U238"]].isWithinBounds(sciantix_variable[sv["U238"]].getFinalValue());
    sciantix_variable[sv["Intergranular vented fraction"]].isWithinBounds(sciantix_variable[sv["Intergranular vented fraction"]].getFinalValue());
    sciantix_variable[sv["Intergranular venting probability"]].isWithinBounds(sciantix_variable[sv["Intergranular venting probability"]].getFinalValue());
    sciantix_variable[sv["Xe133 produced"]].isWithinBounds(sciantix_variable[sv["Xe133 produced"]].getFinalValue());
    sciantix_variable[sv["Xe133 in grain"]].isWithinBounds(sciantix_variable[sv["Xe133 in grain"]].getFinalValue());
    sciantix_variable[sv["Xe133 in intragranular solution"]].isWithinBounds(sciantix_variable[sv["Xe133 in intragranular solution"]].getFinalValue());
    sciantix_variable[sv["Xe133 in intragranular bubbles"]].isWithinBounds(sciantix_variable[sv["Xe133 in intragranular bubbles"]].getFinalValue());
    sciantix_variable[sv["Xe133 decayed"]].isWithinBounds(sciantix_variable[sv["Xe133 decayed"]].getFinalValue());
    sciantix_variable[sv["Xe133 at grain boundary"]].isWithinBounds(sciantix_variable[sv["Xe133 at grain boundary"]].getFinalValue());
    sciantix_variable[sv["Xe133 released"]].isWithinBounds(sciantix_variable[sv["Xe133 released"]].getFinalValue());
    sciantix_variable[sv["Restructured volume fraction"]].isWithinBounds(sciantix_variable[sv["Restructured volume fraction"]].getFinalValue());
    sciantix_variable[sv["HBS porosity"]].isWithinBounds(sciantix_variable[sv["HBS porosity"]].getFinalValue());
    sciantix_variable[sv["Kr85m produced"]].isWithinBounds(sciantix_variable[sv["Kr85m produced"]].getFinalValue());
    sciantix_variable[sv["Kr85m in grain"]].isWithinBounds(sciantix_variable[sv["Kr85m in grain"]].getFinalValue());
    sciantix_variable[sv["Kr85m in intragranular solution"]].isWithinBounds(sciantix_variable[sv["Kr85m in intragranular solution"]].getFinalValue());
    sciantix_variable[sv["Kr85m in intragranular bubbles"]].isWithinBounds(sciantix_variable[sv["Kr85m in intragranular bubbles"]].getFinalValue());
    sciantix_variable[sv["Kr85m decayed"]].isWithinBounds(sciantix_variable[sv["Kr85m decayed"]].getFinalValue());
    sciantix_variable[sv["Kr85m at grain boundary"]].isWithinBounds(sciantix_variable[sv["Kr85m at grain boundary"]].getFinalValue());
    sciantix_variable[sv["Kr85m released"]].isWithinBounds(sciantix_variable[sv["Kr85m released"]].getFinalValue());
    sciantix_variable[sv["Intragranular similarity ratio"]].isWithinBounds(sciantix_variable[sv["Intragranular similarity ratio"]].getFinalValue());
    sciantix_variable[sv["Irradiation time"]].isWithinBounds(sciantix_variable[sv["Irradiation time"]].getFinalValue());
    sciantix_variable[sv["Stoichiometry deviation"]].isWithinBounds(sciantix_variable[sv["Stoichiometry deviation"]].getFinalValue());
    sciantix_variable[sv["Fuel oxygen partial pressure"]].isWithinBounds(sciantix_variable[sv["Fuel oxygen partial pressure"]].getFinalValue());
    sciantix_variable[sv["FIMA"]].isWithinBounds(sciantix_variable[sv["FIMA"]].getFinalValue());
    sciantix_variable[sv["HBS pore density"]].isWithinBounds(sciantix_variable[sv["HBS pore density"]].getFinalValue());
    sciantix_variable[sv["HBS pore volume"]].isWithinBounds(sciantix_variable[sv["HBS pore volume"]].getFinalValue());
    sciantix_variable[sv["HBS pore radius"]].isWithinBounds(sciantix_variable[sv["HBS pore radius"]].getFinalValue());
    sciantix_variable[sv["Xe in HBS pores"]].isWithinBounds(sciantix_variable[sv["Xe in HBS pores"]].getFinalValue());
    sciantix_variable[sv["Xe in HBS pores - variance"]].isWithinBounds(sciantix_variable[sv["Xe in HBS pores - variance"]].getFinalValue());
    sciantix_variable[sv["Xe atoms per HBS pore"]].isWithinBounds(sciantix_variable[sv["Xe atoms per HBS pore"]].getFinalValue());
    sciantix_variable[sv["Xe atoms per HBS pore - variance"]].isWithinBounds(sciantix_variable[sv["Xe atoms per HBS pore - variance"]].getFinalValue());

    // System variables
    sciantix_system[sy["radius_in_lattice"]].isWithinBounds("radius_in_lattice");
    sciantix_system[sy["volume_in_lattice"]].isWithinBounds("volume_in_lattice");
    sciantix_system[sy["diffusivity"]].isWithinBounds("diffusivity"); 
    sciantix_system[sy["bubble_diffusivity"]].isWithinBounds("bubble_diffusivity");
    sciantix_system[sy["resolution_rate"]].isWithinBounds("resolution_rate");
    sciantix_system[sy["trapping_rate"]].isWithinBounds("trapping_rate");
    sciantix_system[sy["nucleation_rate"]].isWithinBounds("nucleation_rate");
    sciantix_system[sy["pore_nucleation_rate"]].isWithinBounds("pore_nucleation_rate");
    sciantix_system[sy["production_rate"]].isWithinBounds("production_rate");
}