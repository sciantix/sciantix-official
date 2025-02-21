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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

void Simulation::HighBurnupStructureFragmentation()
{
    // pore pressure - Hard Spheres Equation of state HS-EoS as propored by Carnhan-Starling
    double Xe_HS_diameter = 4.45e-10 * ( 0.8542 - 0.03996 * log(history_variable["Temperature"].getFinalValue()/231.2) ); // Brearley and MacInnes
    
    double packing_fraction = M_PI /6.0 * sciantix_variable["Xe in HBS pores"].getFinalValue() * pow(Xe_HS_diameter, 3.0); 
    
    double f_packing_fraction = (1 + packing_fraction + pow(packing_fraction, 2.0) - pow(packing_fraction, 3.0)) / pow(1 - packing_fraction, 3);

    double HBS_pore_pressure = f_packing_fraction * boltzmann_constant * history_variable["Temperature"].getFinalValue() * sciantix_variable["Xe atoms per HBS pore"].getFinalValue() / sciantix_variable["HBS pore volume"].getFinalValue(); //Pa
    std::cout << "HBS_pore_pressure (MPa): " << HBS_pore_pressure*1e-6 << std::endl;

    double equilibriumpressure = 2.0 * matrices["UO2HBS"].getSurfaceTension() / sciantix_variable["HBS pore radius"].getFinalValue() - history_variable["Hydrostatic stress"].getFinalValue() * 1e6; //Pa
    std::cout << "equilibrium pressure (MPa): " << equilibriumpressure*1e-6 << std::endl;
	
    // Most active slip planes in UO2 (FCC) is {110}
    // b = 0.5 * lattice constant * <100> 
    double burgervector = 0.5 * matrices["UO2HBS"].getLatticeParameter() * pow(2, 0.5); // m
    
    double dislocation_punching_pressure = burgervector * matrices["UO2HBS"].getShearModulus() *1e6 / sciantix_variable["HBS pore radius"].getFinalValue(); // Pa

    std::cout << "dislocation_punching_pressure (MPa): " << dislocation_punching_pressure*1e-6 << std::endl;
    std::cout << "Elastic modulus (MPa): " << matrices["UO2HBS"].getElasticModulus() << std::endl;

    double K_IC = sqrt(matrices["UO2HBS"].getElasticModulus() * 1e6 * matrices["UO2HBS"].getGrainBoundaryFractureEnergy() / (1.0 - pow(matrices["UO2HBS"].getPoissonRatio(), 2))) * 1e-6; // (MPa m0.5)

    std::cout << "K_IC (MPa m0.5): " << K_IC << std::endl;

    double fracture_stress = K_IC * 1e6 / (sqrt( M_PI * sciantix_variable["HBS pore radius"].getFinalValue()));
    
    std::cout << "fracture_stress (MPa): " << fracture_stress*1e-6 << std::endl;	
}