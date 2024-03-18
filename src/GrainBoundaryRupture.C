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

        double E = matrix[sma["UO2"]].getElasticModulus();
        double nu = matrix[sma["UO2"]].getPoissonRatio();
        double G_gb = matrix[sma["UO2"]].getGrainBoundaryFractureEnergy();

        // Polynomial fit for the (dimensionless) stress intensity factor
        // @ref Jernkvist 2019      : Fi = + 0.568Fc^2 + 0.059Fc + 0.5587
        double Fc = sciantix_variable[sv["Integranular fractional coverage"]].getFinalValue();
        double beta = + 0.568 * pow(Fc,2) + 0.059 * Fc + 0.5587;

        // Stress intensity factor
        // G = K^2 (1-nu^2) / E
        double K_IC = sqrt(G_gb * E / (1.0 - pow(nu, 2)));

        double s_hyd = history_variable[hv["Hydrostatic stress"]].getFinalValue() * 1e6; // Pa
        double crack_length = sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue();

        // K_IC = beta * stress * np.sqrt(np.pi * a) --> stress = K_IC / (beta * sqrt(pi * a))
        double sigma_crack = K_IC / (beta * sqrt(CONSTANT_NUMBERS_H::MathConstants::pi * crack_length)); // overpressure

        double p_l = 2.0 * matrix[sma["UO2"]].getSurfaceTension() / sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue();

        sciantix_variable[sv["Critical intergranular bubble pressure"]].setFinalValue((sigma_crack - s_hyd + p_l)*1e-6);
        parameter.push_back(sigma_crack);

        model[model_index].setParameter(parameter);
        model[model_index].setRef("Under development");
    }
}

