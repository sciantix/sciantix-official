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
//  Authors: G. Zullo, G. Petrosillo                                                //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "StoichiometryDeviation.h"

void StoichiometryDeviation()
{
  /**
   * @brief This routine sets the model to estimate the stoichiometry deviation of the fuel.
   * Currently, UO2+x is considered, under external oxidizing environment.
   * 
   */

	if (!input_variable[iv["iStoichiometryDeviation"]].getValue()) return;

  model.emplace_back();
  int model_index = int(model.size()) - 1;

  std::string reference;
  std::vector<double> parameter;
  
  double surface_to_volume = 3 / sciantix_variable[sv["Grain radius"]].getFinalValue(); // (1/m)

  model[model_index].setName("Stoichiometry deviation");
  switch (int(input_variable[iv["iStoichiometryDeviation"]].getValue()))
  {
    case 0 :
    {
      /**
       * @brief iStoichiometryDeviation = 0 neglects the stoichiometry deviation of the fuel.
       * 
       */

      reference += "not considered.";

      parameter.push_back(0);
      parameter.push_back(0);

      model[model_index].setParameter(parameter);
      model[model_index].setRef(reference);

      break;
		}

    case 1 :
    {
      /**
       * @brief
       * The model for fuel oxidation and stoichimetry deviation evolution is described with a semi-empirical model.
       * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
       * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
       * 
       * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by experimments.
       * It is expressed as alpha = A exp(−Q/T).
       * S/V = 3/a (1/m), surface-to-volume ratio of the fuel sample
       * x_eq : equilbrium stoichiometry deviation (evaluated by UO2Thermochemistry model)
       * 
       * ### iStoichiometryDeviation = 1
       * 
       * Range of utilization:
       * - Pure steam
       * - Temperature range: 1273-1923
       * @ref Cox et al. NUREG/CP-0078 (1986), U.S. NRC
       * 
       */

      reference += "Carter and Lay, J. Nucl. Mater., 36:77-86, Cox et al. NUREG/CP-0078 (1986), U.S. NRC.";

      double surface_exchange_coefficient = 0.365 * exp(-23500/history_variable[hv["Temperature"]].getFinalValue());
      
      double decay_rate = surface_exchange_coefficient * sqrt(history_variable[hv["Steam pressure"]].getFinalValue()) * surface_to_volume; 
      double source_rate = surface_exchange_coefficient * sqrt(history_variable[hv["Steam pressure"]].getFinalValue()) * sciantix_variable[sv["Equilibrium stoichiometry deviation"]].getFinalValue() * surface_to_volume;

      parameter.push_back(decay_rate);
      parameter.push_back(source_rate);
    
      model[model_index].setParameter(parameter);
      model[model_index].setRef(reference);

      break;
    }

    case 2 :
    {
      /**
       * @brief
       * The model for fuel oxidation and stoichimetry deviation evolution is described with a semi-empirical model.
       * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
       * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
       * 
       * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by experimments.
       * It is expressed as alpha = A exp(−Q/T).
       * S/V = 3/a (1/m), surface-to-volume ratio of the fuel sample
       * x_eq : equilbrium stoichiometry deviation (evaluated by UO2Thermochemistry model)
       * 
       * ### iStoichiometryDeviation = 2
       * 
       * Range of utilization:
       * - Pure steam
       * - Temperature range: 1158-2108
       * @ref Bittel et al., J. Amer. Ceram. Soc., 52:446–451, 1969, reanalysed by Cox et al. NUREG/CP-0078 (1986), U.S. NRC
       * 
       */

      reference += "Carter and Lay, J. Nucl. Mater., 36:77-86, 1970. Bittel et al., J. Amer. Ceram. Soc., 52:446-451, 1969.";

      double surface_exchange_coefficient = 0.194 * exp(-19900/history_variable[hv["Temperature"]].getFinalValue());

      double decay_rate = surface_exchange_coefficient * sqrt(history_variable[hv["Steam pressure"]].getFinalValue()) * (surface_to_volume); 
      double source_rate = surface_exchange_coefficient * sqrt(history_variable[hv["Steam pressure"]].getFinalValue())  * sciantix_variable[sv["Equilibrium stoichiometry deviation"]].getFinalValue() * (surface_to_volume);

      parameter.push_back(decay_rate);
      parameter.push_back(source_rate);

      model[model_index].setParameter(parameter);
      model[model_index].setRef(reference);

      break;
    }

    case 3 : 
    {
      /**
       * @brief
       * The model for fuel oxidation and stoichimetry deviation evolution is described with a semi-empirical model.
       * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
       * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
       * 
       * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by experimments.
       * It is expressed as alpha = A exp(−Q/T).
       * S/V = 3/a (1/m), surface-to-volume ratio of the fuel sample
       * x_eq : equilbrium stoichiometry deviation (evaluated by UO2Thermochemistry model)
       * 
       * ### iStoichiometryDeviation = 3
       * 
       * Range of utilization:
       * - Pure steam
       * - Polycrystal sample
       * - Temperature range: 1273-1673
       * @ref Abrefah et al., JNM., 208:98–110, 1994.
       * 
       */

      reference += "Carter and Lay, J. Nucl. Mater., 36:77-86, 1970. Abrefah, JNM., 208:98-110, 1994.";

      double surface_exchange_coefficient = 0.382 * exp(-22080/history_variable[hv["Temperature"]].getFinalValue());

      double decay_rate = surface_exchange_coefficient * sqrt(history_variable[hv["Steam pressure"]].getFinalValue()) * (surface_to_volume); 
      double source_rate = surface_exchange_coefficient * sqrt(history_variable[hv["Steam pressure"]].getFinalValue()) * sciantix_variable[sv["Equilibrium stoichiometry deviation"]].getFinalValue() * (surface_to_volume);

      parameter.push_back(decay_rate);
      parameter.push_back(source_rate);

      model[model_index].setParameter(parameter);
      model[model_index].setRef(reference);

      break;
    }

    case 4 : 
    {
      /**
       * @brief
       * The model for fuel oxidation and stoichimetry deviation evolution is described with a semi-empirical model.
       * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
       * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
       * 
       * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by experiments.
       * It is expressed as alpha = A exp(−Q/T).
       * S/V = 3/a (1/m), surface-to-volume ratio of the fuel sample
       * x_eq : equilbrium stoichiometry deviation (evaluated by UO2Thermochemistry model)
       * 
       * ### iStoichiometryDeviation = 4
       * 
       * Range of utilization:
       * - Pure steam
       * - Temperature range: 1073-1473
       * @ref Imamura and. Une, JNM, 247:131–137, 1997.
       * 
       */

      reference += "Carter and Lay, J. Nucl. Mater., 36:77-86, 1970. Imamura and. Une, JNM, 247:131-137, 1997.";

      double surface_exchange_coefficient = 0.000341 * exp(-15876/history_variable[hv["Temperature"]].getFinalValue());

      double decay_rate = surface_exchange_coefficient * sqrt(history_variable[hv["Steam pressure"]].getFinalValue()/0.12) * surface_to_volume; 
      double source_rate = surface_exchange_coefficient * sqrt(history_variable[hv["Steam pressure"]].getFinalValue()/0.12) * sciantix_variable[sv["Equilibrium stoichiometry deviation"]].getFinalValue() * surface_to_volume;

      parameter.push_back(decay_rate);
      parameter.push_back(source_rate);

      model[model_index].setParameter(parameter);
      model[model_index].setRef(reference);

      break;
    }

    case 5 :
    {
      /**
       * @brief
       * The model for fuel oxidation and stoichimetry deviation evolution is described with a mechanistic Langmuir-based approach
       * @ref Massih, A. R. "UO2 fuel oxidation and fission gas release." Swedish Radiation Safety Authority report, Report 2018 (2018): 25.
       * The oxidation rate follows: dx/dt = theta/tau (1 - sqrt(Po2(x)/Po2))
       * In SCIANTIX, the ODE is rewritten as: dx / dt = K (1 - beta * exp(alpha * x))
       * 
       * ### iStoichiometryDeviation = 5
       * 
       * Range of utilization:
       * - Pure steam
       * - Temperature range: 1073-1673 K
       * 
       */
      const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;
      
      double k_star = 1e4 * exp(-21253.0/history_variable[hv["Temperature"]].getFinalValue()-2.43); // (mol/m2 s)
      double tau_inv = k_star * (surface_to_volume)/8.0e4;
      double s = 0.023;
      double ka = 1.0e13 * exp(-21557.0/history_variable[hv["Temperature"]].getFinalValue());
      double B = s/sqrt(2*pi*8.314*history_variable[hv["Temperature"]].getFinalValue()*0.018);
      double A = 1.0135e5*B/(1.66e-6*ka); // (1/atm)
      double theta = A*history_variable[hv["Steam pressure"]].getFinalValue()*1.013e5 /(1+A*history_variable[hv["Steam pressure"]].getFinalValue()*1.013e5);
      double gamma = sqrt(exp(-32700.0/history_variable[hv["Temperature"]].getFinalValue()+9.92)*1.013e5);
      double rad_c = sqrt(0.0004);
      double beta;

      if(sciantix_variable[sv["Gap oxygen partial pressure"]].getFinalValue() > 0.0)
        beta = rad_c*gamma/sqrt(sciantix_variable[sv["Gap oxygen partial pressure"]].getFinalValue()*1.013e5);
      else 
        beta = 0.0;
      
      double alpha = 57/2;
      double K = tau_inv*theta;

      parameter.push_back(K);
      parameter.push_back(beta);
      parameter.push_back(alpha);

      model[model_index].setParameter(parameter);
      model[model_index].setRef(reference);

      break;
    }

    case 6 :
    {
      /**
       * @brief
       * The model for fuel oxidation and stoichimetry deviation evolution is described with a mechanistic Langmuir-based approach
       * @ref Massih, A. R. "UO2 fuel oxidation and fission gas release." Swedish Radiation Safety Authority report, Report 2018 (2018): 25.
       * The oxidation rate follows: dx/dt = theta/tau (1 - sqrt(Po2(x)/Po2))
       * In SCIANTIX, the ODE is rewritten as: dx / dt = K (1 - beta * exp(alpha * x))
       * 
       * ### iStoichiometryDeviation = 5
       * 
       * Range of utilization:
       * - Pure steam
       * - Temperature range: 1073-1673 K
       * 
       * In this case, the surface-to-volume ratio of the sample is fixed.
       * This is the case for the simulation of oxidised rodlet.
       * 
       */
      const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;
      surface_to_volume = 225;
      
      double k_star = 1e4 * exp(-21253.0/history_variable[hv["Temperature"]].getFinalValue()-2.43); // (mol/m2 s)
      double tau_inv = k_star * (surface_to_volume)/8.0e4;
      double s = 0.023;
      double ka = 1.0e13 * exp(-21557.0/history_variable[hv["Temperature"]].getFinalValue());
      double B = s/sqrt(2*pi*8.314*history_variable[hv["Temperature"]].getFinalValue()*0.018);
      double A = 1.0135e5*B/(1.66e-6*ka); // (1/atm)
      double theta = A*history_variable[hv["Steam pressure"]].getFinalValue()*1.013e5 /(1+A*history_variable[hv["Steam pressure"]].getFinalValue()*1.013e5);
      double gamma = sqrt(exp(-32700.0/history_variable[hv["Temperature"]].getFinalValue()+9.92)*1.013e5);
      double rad_c = sqrt(0.0004);
      double beta;

      if(sciantix_variable[sv["Gap oxygen partial pressure"]].getFinalValue()>0)
        beta = rad_c*gamma/sqrt(sciantix_variable[sv["Gap oxygen partial pressure"]].getFinalValue()*1.013e5);
      else 
        beta = 0.0;

      double alpha = 57/2;
      double K = tau_inv*theta;

      parameter.push_back(K);
      parameter.push_back(beta);
      parameter.push_back(alpha);

      model[model_index].setParameter(parameter);
      model[model_index].setRef(reference);

      break;
    }


    
    default :
      ErrorMessages::Switch("StoichiometryDeviation.cpp", "iStoichiometryDeviation", int(input_variable[iv["iStoichiometryDeviation"]].getValue()));
      break;
  }
}
