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

#include "GrainBoundaryRupture.h"

void GrainBoundaryRupture()
{
    /**
     * @brief Model: Grain-boundary rupture
     * 
     * @author G. Zullo
     * 
     * @ref
     * Chackraborty et al. JNM 452 (2014) 95-101
     * 
    */

	// if (!input_variable[iv["iGrainBoundaryMicroCracking"]].getValue()) return;

	model.emplace_back();
	int model_index = int(model.size()) - 1;
	model[model_index].setName("Grain-boundary rupture");
	std::vector<double> parameter;

    const double boltzmann_constant = CONSTANT_NUMBERS_H::PhysicsConstants::boltzmann_constant;
    const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

    double E =  matrix[sma["UO2"]].getElasticModulus() * 1e6; // Pa
    double nu =  matrix[sma["UO2"]].getPoissonRatio();
    double G_gb =  matrix[sma["UO2"]].getGrainBoundaryFractureEnergy(); // J/m2

    // Fracture toughness
    // K_IC = sqrt(elasticmodulus*grainboundaryenergy/(1-poissonratio**2))
    sciantix_variable[sv["Fracture toughness"]].setFinalValue(sqrt(E * G_gb / (1.0 - pow(nu, 2))) * 1e-6); // (MPa m0.5)
    
    // Stress intensification at GB tip
    // kt = 1 + crackdiameter / crackheight
    //double stressintensification = 1 + 2*sin(matrix[sma["UO2"]].getSemidihedralAngle())/(1-cos(matrix[sma["UO2"]].getSemidihedralAngle()));
    double stressintensification = 3.33;
    std::cout << "Stress intensification factor ="<< stressintensification <<std::endl;

    // Equilibrium pressure by capillary pressure and hydrostatic stress
    // P = (2*effective_surface_tension)/bubble radius + Phydrostatic
    double equilibriumpressure = 2.0 * matrix[sma["UO2"]].getSurfaceTension()*(1-cos(matrix[sma["UO2"]].getSemidihedralAngle())) / sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() -
				history_variable[hv["Hydrostatic stress"]].getFinalValue() * 1e6; //Pa
    sciantix_variable[sv["Equilibrium bubble pressure"]].setFinalValue(equilibriumpressure * 1e-6);

    // JERNKVIST 2019

    // Polynomial fit for the (dimensionless) stress intensity factor
    // @ref Jernkvist 2019: Fi = (+ 0.568Fc**2 + 0.059Fc + 0.5587)
    double factorJ = (0.568 * pow(sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue(),2) + 0.059 * sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue() + 0.5587);
    
    // Pcrit = Peq + (1/(pi*F))*Kic*sqrt(pi/radius)*(1/kt)  //Pa
    double critical_bubble_pressureJ = equilibriumpressure + 
        (1 / (pi*factorJ)) * sciantix_variable[sv["Fracture toughness"]].getFinalValue()*1e6* sqrt(pi/(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue()*sin(matrix[sma["UO2"]].getSemidihedralAngle()))) * 1/stressintensification;    
   
    // THIS WORK

    // Geometrical factor accounting for fractional coverage of the grain face
    // F = 2*(sqrt(1/sqrt(Fc)-1)
    double factor = 2*sqrt(pow(sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue(), -0.5)-1);
    
    // Pcrit = Peq + (1-2/(pi*F))*Kic*sqrt(pi/radius)*(1/kt) //Pa
    double geometrical_factor = sf_geometrical_parameter;
    double fracture_stress = sciantix_variable[sv["Fracture toughness"]].getFinalValue()*1e6*sqrt(pi/(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue()*sin(matrix[sma["UO2"]].getSemidihedralAngle())))*(1/stressintensification)*(geometrical_factor)*(1-1/(factor*pi*geometrical_factor));
    double critical_bubble_pressure = equilibriumpressure + fracture_stress;    //Pa
    
    sciantix_variable[sv["Fracture stress"]].setFinalValue(fracture_stress*1e-6); //MPa
    sciantix_variable[sv["Critical intergranular bubble pressure"]].setFinalValue(critical_bubble_pressure*1e-6); //MPa
    
    
    double bubble_pressure = (boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue() * //Pa
      sciantix_variable[sv["Intergranular atoms per bubble"]].getFinalValue() /
      (sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[sma["UO2"]].getSchottkyVolume()));
    sciantix_variable[sv["Intergranular bubble pressure"]].setFinalValue(bubble_pressure*1e-6); //MPa
    
    double bubbleoverpressure = bubble_pressure-critical_bubble_pressure;
    
    std::cout << "Grain Boundary rupture --------------------------------"<<std::endl;
    if (bubbleoverpressure > 0)
    {
        std::cout <<"WARNING: critical bubble pressure exceeded"<<std::endl;
        std::cout << "Critical pressure (Pa): "<<critical_bubble_pressure<<std::endl;
        std::cout << "Critical pressure Jernkvist (Pa): "<<critical_bubble_pressureJ<<std::endl;
        std::cout << "Bubble pressure (Pa): "<<bubble_pressure<<std::endl;
    }
    
    parameter.push_back(critical_bubble_pressure);

	model[model_index].setParameter(parameter);
	model[model_index].setRef("Under development");
}

