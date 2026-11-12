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
//  Version: 2.2.1                                                                  //
//  Year: 2026                                                                      //
//  Authors: G. Nicodemo, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "DislocationDensity.h"
#include "Simulation.h"

void Simulation::DislocationDensity()
{
    // Defined only with the two-population intragranular bubble model (iIntraGranularBubbleBehavior = 5).
    if (!sciantix_variable.isElementPresent("Dislocation density"))
        return;

    sciantix_variable["Dislocation density"].setFinalValue(dislocation_density::correlation(
        sciantix_variable["Burnup"].getFinalValue() * dislocation_density::oxide_to_heavy_metal,
        history_variable["Temperature"].getFinalValue()));
}
