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

#include "FiguresOfMerit.h"

void FiguresOfMerit()
{
  /**
   * @brief This routines computes the value of sciantix_variable associated to figures of merit
   * that are of interest in the output.txt file.
   * 
   */

  /// Fission gas release 
  if (sciantix_variable[sv["Xe produced"]].getFinalValue() + sciantix_variable[sv["Kr produced"]].getFinalValue() > 0.0)
    sciantix_variable[sv["Fission gas release"]].setFinalValue(
      (sciantix_variable[sv["Xe released"]].getFinalValue() + sciantix_variable[sv["Kr released"]].getFinalValue()) /
      (sciantix_variable[sv["Xe produced"]].getFinalValue() + sciantix_variable[sv["Kr produced"]].getFinalValue())
    );
  else
    sciantix_variable[sv["Fission gas release"]].setFinalValue(0.0);

  // Release-to-birth ratio: Xe133
  // Note that R/B is not defined with a null fission rate.
  if (sciantix_variable[sv["Xe133 produced"]].getFinalValue() - sciantix_variable[sv["Xe133 decayed"]].getFinalValue() > 0.0)
    sciantix_variable[sv["Xe133 R/B"]].setFinalValue(
      sciantix_variable[sv["Xe133 released"]].getFinalValue() /
      (sciantix_variable[sv["Xe133 produced"]].getFinalValue() - sciantix_variable[sv["Xe133 decayed"]].getFinalValue())
    );
  else
    sciantix_variable[sv["Xe133 R/B"]].setFinalValue(0.0);

  // Release-to-birth ratio: Kr85m
  // Note that R/B is not defined with a null fission rate.
  if (sciantix_variable[sv["Kr85m produced"]].getFinalValue() - sciantix_variable[sv["Kr85m decayed"]].getFinalValue() > 0.0)
    sciantix_variable[sv["Kr85m R/B"]].setFinalValue(
      sciantix_variable[sv["Kr85m released"]].getFinalValue() /
      (sciantix_variable[sv["Kr85m produced"]].getFinalValue() - sciantix_variable[sv["Kr85m decayed"]].getFinalValue())
    );
  else
    sciantix_variable[sv["Kr85m R/B"]].setFinalValue(0.0);

  // Helium fractional release
  if (sciantix_variable[sv["He produced"]].getFinalValue() > 0.0)
    sciantix_variable[sv["He fractional release"]].setFinalValue(
      sciantix_variable[sv["He released"]].getFinalValue() /
      sciantix_variable[sv["He produced"]].getFinalValue()
    );
  else
    sciantix_variable[sv["He fractional release"]].setFinalValue(0.0);

  // Helium release rate
  if (physics_variable[pv["Time step"]].getFinalValue() > 0.0)
    sciantix_variable[sv["He release rate"]].setFinalValue(
      sciantix_variable[sv["He released"]].getIncrement() /
      physics_variable[pv["Time step"]].getFinalValue()
    );
  else
    sciantix_variable[sv["He release rate"]].setFinalValue(0.0);

  // Fuel oxygen potential
  if(sciantix_variable[sv["Fuel oxygen partial pressure"]].getFinalValue() == 0.0)
    sciantix_variable[sv["Fuel oxygen potential"]].setFinalValue(0.0);
  else
    sciantix_variable[sv["Fuel oxygen potential"]].setFinalValue(8.314*1.0e-3*history_variable[hv["Temperature"]].getFinalValue()*log(sciantix_variable[sv["Fuel oxygen partial pressure"]].getFinalValue()/0.1013));
    
	const double boltzmann_constant = CONSTANT_NUMBERS_H::PhysicsConstants::boltzmann_constant;

  // Intergranular bubble pressure p = kTng/Onv
  if(sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue())
    sciantix_variable[sv["Intergranular bubble pressure"]].setFinalValue(1e-6 *
      boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue() *
      sciantix_variable[sv["Intergranular atoms per bubble"]].getFinalValue() /
      (sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[0].getSchottkyVolume())
    );
  else
    sciantix_variable[sv["Intergranular bubble pressure"]].setFinalValue(0.0);
}