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
	if (input_variable[iv["iGrainBoundaryMicroCracking"]].getValue()==2)
    {
        model.emplace_back();
        int model_index = int(model.size()) - 1;
        model[model_index].setName("Grain-boundary rupture");
        std::vector<double> parameter;

        double E = matrix[0].getElasticModulus() * 1e6;
        double nu = matrix[0].getPoissonRatio();
        double G_gb = matrix[0].getGrainBoundaryFractureEnergy();

        // Polynomial fit for the (dimensionless) stress intensity factor
        // @ref Jernkvist 2019      : Fi = + 0.568Fc + 0.059Fc + 0.5587
        double factor = CONSTANT_NUMBERS_H::MathConstants::pi * (0.568 * pow(sciantix_variable[sv["Integranular fractional coverage"]].getFinalValue(),2) + 0.059 * sciantix_variable[sv["Integranular fractional coverage"]].getFinalValue() + 0.5587);

        double critical_bubble_pressure = - history_variable[hv["Hydrostatic stress"]].getFinalValue() * 1e6 + 2.0 * matrix[0].getSurfaceTension() / sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() + 
            1 / factor * sqrt(CONSTANT_NUMBERS_H::MathConstants::pi * E * G_gb / ((1.0 - pow(nu, 2)) * sciantix_variable[sv["Integranular bubble radius"]].getFinalValue()));    

        sciantix_variable[sv["Critical intergranular bubble pressure"]].setFinalValue(critical_bubble_pressure * 1e-6);

        parameter.push_back(critical_bubble_pressure);

        model[model_index].setParameter(parameter);
        model[model_index].setRef("Under development");
    }
}

