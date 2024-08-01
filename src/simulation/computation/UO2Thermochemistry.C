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

#include "Simulation.h"

void Simulation::UO2Thermochemistry()
{
  if (!input_variable["iStoichiometryDeviation"].getValue())
    return;

  Model uo2_thermochem_model;

  uo2_thermochem_model.setName("UO2 thermochemistry");

  std::string reference;
  reference = "Blackburn (1973) J. Nucl. Mater., 46, 244-252.";

  std::vector<double> parameter;

  parameter.push_back(sciantix_variable["Stoichiometry deviation"].getInitialValue());
  parameter.push_back(history_variable["Temperature"].getFinalValue());
  parameter.push_back(sciantix_variable["Gap oxygen partial pressure"].getFinalValue()); // (atm)

  uo2_thermochem_model.setParameter(parameter);
  uo2_thermochem_model.setRef(reference);

  model.push(uo2_thermochem_model);

  if (!input_variable["iStoichiometryDeviation"].getValue())
      return;

  if (history_variable["Temperature"].getFinalValue() < 1000.0 || sciantix_variable["Gap oxygen partial pressure"].getFinalValue() == 0)
      sciantix_variable["Equilibrium stoichiometry deviation"].setFinalValue(0.0);

  else
      sciantix_variable["Equilibrium stoichiometry deviation"].setFinalValue(
          solver.NewtonBlackburn(
              model["UO2 thermochemistry"].getParameter()));
}