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

#include "UO2Thermochemistry.h"

void UO2Thermochemistry()
{
  /**
   * @brief This routine defines the model to evaluate the oxygen partial pressure (in atm) in hyperstoichiometric UO2+x fuel
   * as a function of:
   * 
   * @param[in] stoichiometry_deviation
   * @param[in] temperature
   * 
   * @param[out] PO2_x oxygen partial pressure in UO2+x (in atm) 
   * from Blackburn’s relation, @ref *Blackburn (1973) J. Nucl. Mater., 46, 244–252*
   * 
   */

	if (!input_variable[iv["iStoichiometryDeviation"]].getValue()) return;

  model.emplace_back();
  int model_index = int(model.size()) - 1;

  model[model_index].setName("UO2 thermochemistry");
  
  std::string reference;
  reference = "Blackburn (1973) J. Nucl. Mater., 46, 244-252.";
  
  std::vector<double> parameter;

  parameter.push_back(sciantix_variable[sv["Stoichiometry deviation"]].getInitialValue());
  parameter.push_back(history_variable[hv["Temperature"]].getFinalValue()); 
  parameter.push_back(sciantix_variable[sv["Gap oxygen partial pressure"]].getFinalValue()); // (atm)

  model[model_index].setParameter(parameter);
  model[model_index].setRef(reference);
}

double BlackburnThermochemicalModel(double stoichiometry_deviation, double temperature)
{
  /**
   * @brief The oxygen partial pressure in UO2+x fuel as a function of x, i.e., PO2 (x) (in atm) is calculated from Blackburn’s relation
   * @ref Blackburn (1973) J. Nucl. Mater., 46, 244–252.
   * 
   * Validity range:
   * - T: 1000 K - 2670 K
   * - x: 0 - 0.25
   */

  double ln_p = 2.0 * log(stoichiometry_deviation*(2.0+stoichiometry_deviation)/(1.0-stoichiometry_deviation))
    +	108.0*pow(sciantix_variable[sv["Stoichiometry deviation"]].getFinalValue(),2.0)
    - 32700.0/temperature + 9.92;

  return exp(ln_p);
}