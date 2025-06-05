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

#include "Simulation.h"
#include "MainVariables.h"
#include "SourceHandler.h"

// This routine is specifically used to simulate the resolution process from the grain boundary.
// Practically, it accounts for the concentrated source by removing gas from the grain boundary,
// as that gas has already been introduced back into the grain in the 'non_uniform_source.txt' file.


void Simulation::GrainBoundarySource()
{
    // Model declaration
    for (auto &system : sciantix_system)
    {
        // Divide Iinto 2 sources:

        Source FullSource = sources_interp.at(history_variable["Time step number"].getFinalValue());
        Source fission_rate;
        Source GBSource;
        double a = Sciantix_variables[0]; // Grain Radius [m]

        // Assign shared properties (time)
        fission_rate.time = FullSource.time;
        GBSource.time = FullSource.time;

        // Resize Normalized Domain appropriately
        fission_rate.NormalizedDomain.resize(2);
        GBSource.NormalizedDomain.resize(2);

        fission_rate.NormalizedDomain[0] = FullSource.NormalizedDomain[0];
        fission_rate.NormalizedDomain[1] = FullSource.NormalizedDomain[1];
        GBSource.NormalizedDomain[0] = FullSource.NormalizedDomain[1];
        GBSource.NormalizedDomain[1] = FullSource.NormalizedDomain[2];

        // Resize Slopes and Intercepts appropriately
        fission_rate.Slopes.resize(1);     // Only A1
        fission_rate.Intercepts.resize(1); // Only B1

        GBSource.Slopes.resize(1);     // Only A2
        GBSource.Intercepts.resize(1); // Only B2

        // Assign values from FullSource
        fission_rate.Slopes[0] = FullSource.Slopes[0];         // A1
        fission_rate.Intercepts[0] = FullSource.Intercepts[0]; // B1

        GBSource.Slopes[0] = FullSource.Slopes[1];         // A2
        GBSource.Intercepts[0] = FullSource.Intercepts[1]; // B2

        double d = a * GBSource.NormalizedDomain[0];
        double x = a-d;
        double ResolutionLayerVolume = (4.0 / 3.0) * M_PI * (pow(a,3) - pow(d,3));
        double GrainVolume = (4.0 / 3.0) * M_PI * pow(a,3);
        
        // This gives how much we should remove from the grain boundaries
        double timestep = physics_variable["Time step"].getFinalValue(); //already in seconds
        double GBResolutionSourceNet = (GBSource.Intercepts[0] - fission_rate.Intercepts[0]) * timestep;

        GBresolve = GBResolutionSourceNet;

    }
}