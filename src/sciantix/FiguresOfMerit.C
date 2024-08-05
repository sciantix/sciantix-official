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

void Simulation::FiguresOfMerit()
{
  /// Fission gas release 
  if (sciantix_variable["Xe produced"].getFinalValue() + sciantix_variable["Kr produced"].getFinalValue() > 0.0)
    sciantix_variable["Fission gas release"].setFinalValue(
      (sciantix_variable["Xe released"].getFinalValue() + sciantix_variable["Kr released"].getFinalValue()) /
      (sciantix_variable["Xe produced"].getFinalValue() + sciantix_variable["Kr produced"].getFinalValue())
    );
  else
    sciantix_variable["Fission gas release"].setFinalValue(0.0);

  // Release-to-birth ratio: Xe133
  // Note that R/B is not defined with a null fission rate.
  if (sciantix_variable["Xe133 produced"].getFinalValue() - sciantix_variable["Xe133 decayed"].getFinalValue() > 0.0)
    sciantix_variable["Xe133 R/B"].setFinalValue(
      sciantix_variable["Xe133 released"].getFinalValue() /
      (sciantix_variable["Xe133 produced"].getFinalValue() - sciantix_variable["Xe133 decayed"].getFinalValue())
    );
  else
    sciantix_variable["Xe133 R/B"].setFinalValue(0.0);

  // Release-to-birth ratio: Kr85m
  // Note that R/B is not defined with a null fission rate.
  if (sciantix_variable["Kr85m produced"].getFinalValue() - sciantix_variable["Kr85m decayed"].getFinalValue() > 0.0)
    sciantix_variable["Kr85m R/B"].setFinalValue(
      sciantix_variable["Kr85m released"].getFinalValue() /
      (sciantix_variable["Kr85m produced"].getFinalValue() - sciantix_variable["Kr85m decayed"].getFinalValue())
    );
  else
    sciantix_variable["Kr85m R/B"].setFinalValue(0.0);

  // Helium fractional release
  if (sciantix_variable["He produced"].getFinalValue() > 0.0)
    sciantix_variable["He fractional release"].setFinalValue(
      sciantix_variable["He released"].getFinalValue() /
      sciantix_variable["He produced"].getFinalValue()
    );
  else
    sciantix_variable["He fractional release"].setFinalValue(0.0);

  // Helium release rate
  if (physics_variable["Time step"].getFinalValue() > 0.0)
    sciantix_variable["He release rate"].setFinalValue(
      sciantix_variable["He released"].getIncrement() /
      physics_variable["Time step"].getFinalValue()
    );
  else
    sciantix_variable["He release rate"].setFinalValue(0.0);

  // Fuel oxygen potential
  if(sciantix_variable["Fuel oxygen partial pressure"].getFinalValue() == 0.0)
    sciantix_variable["Fuel oxygen potential"].setFinalValue(0.0);
  else
    sciantix_variable["Fuel oxygen potential"].setFinalValue(8.314*1.0e-3*history_variable["Temperature"].getFinalValue()*log(sciantix_variable["Fuel oxygen partial pressure"].getFinalValue()/0.1013));

  // Intergranular bubble pressure p = kTng/Onv (MPa)
  if(sciantix_variable["Intergranular vacancies per bubble"].getFinalValue())
    sciantix_variable["Intergranular bubble pressure"].setFinalValue(1e-6 *
      boltzmann_constant * history_variable["Temperature"].getFinalValue() *
      sciantix_variable["Intergranular atoms per bubble"].getFinalValue() /
      (sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * matrices["UO2"].getSchottkyVolume())
    );
  else
    sciantix_variable["Intergranular bubble pressure"].setFinalValue(0.0);
}