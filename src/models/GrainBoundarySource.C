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

#include "MainVariables.h"
#include "Simulation.h"
#include "SourceHandler.h"

// This routine is specifically used to simulate the resolution process from the grain boundary.
// Practically, it accounts for the concentrated source by removing gas from the grain boundary,
// as that gas has already been introduced back into the grain in the 'non_uniform_source.txt' file.

void Simulation::GrainBoundarySource()
{
    // Only meaningful with the non-uniform-source solver: the concentrated grain-boundary term is
    // read from non_uniform_source.txt, which is not loaded for the other solvers.
    GBresolve = 0.0;

    if (int(input_variable["iDiffusionSolver"].getValue()) != 4)
        return;

    if (input_variable["iGrainBoundaryResolution"].getValue() == 0)
        return;

    const size_t step = static_cast<size_t>(history_variable["Time step number"].getFinalValue());
    if (step >= sources_interp.size())
        return;

    const Source& FullSource = sources_interp[step];

    // The split below needs two regions: the inner one is the bulk fission-rate source, the outer
    // one is the re-solution layer adjacent to the grain boundary.
    if (FullSource.NormalizedDomain.size() < 3 || FullSource.Slopes.size() < 2 || FullSource.Intercepts.size() < 2)
        return;

    const double a = sciantix_variable["Grain radius"].getFinalValue();  // grain radius (m)
    if (a <= 0.0)
        return;

    const double d                     = a * FullSource.NormalizedDomain[1];  // inner radius of the re-solution layer
    const double ResolutionLayerVolume = (4.0 / 3.0) * M_PI * (pow(a, 3) - pow(d, 3));
    const double GrainVolume           = (4.0 / 3.0) * M_PI * pow(a, 3);
    const double R                     = ResolutionLayerVolume / GrainVolume;

    // Net gas to be removed from the grain boundary over the time step
    const double timestep              = physics_variable["Time step"].getFinalValue();  // (s)
    const double GBResolutionSourceNet = (FullSource.Intercepts[1] - FullSource.Intercepts[0]) * timestep;

    GBresolve = GBResolutionSourceNet * R;
}
