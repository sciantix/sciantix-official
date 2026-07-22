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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

Simulation* Simulation::instance = nullptr;

Simulation* Simulation::getInstance()
{
    if (instance == nullptr)
    {
        instance = new Simulation;
    }
    return instance;
}

void Simulation::initialize(int                            Sciantix_options[],
                            double                         Sciantix_history[],
                            double                         Sciantix_variables[],
                            double                         Sciantix_scaling_factors[],
                            double                         Sciantix_diffusion_modes[],
                            double                         Sciantix_thermochemistry[],
                            const ThermochemistrySettings* Sciantix_thermochemistry_settings)
{
    setVariables(Sciantix_options,
                 Sciantix_history,
                 Sciantix_variables,
                 Sciantix_scaling_factors,
                 Sciantix_diffusion_modes,
                 Sciantix_thermochemistry,
                 Sciantix_thermochemistry_settings);
    setFissionProducts();
    setMatrix();
    setSystem();
    setGPVariables(Sciantix_options, Sciantix_history, Sciantix_variables);
}

void Simulation::execute()
{
#if !defined(COUPLING_TU)
    Burnup();

    EffectiveBurnup();

    Densification();
#endif

    // FUEL MICROSTRUCTURE

    HighBurnupStructureFormation();

    HighBurnupStructurePorosity();

    GrainGrowth();

    // FUEL CHEMISTRY

    ChromiumSolubility();

#if !defined(COUPLING_TU)
    GapPartialPressure();

    UO2Thermochemistry();
#endif
    StoichiometryDeviation();

    // FISSION PRODUCT BEHAVIOR

    GrainBoundarySweeping();

    FissionProductProduction();

    FissionProductDecay();

    IntraGranularBubbleBehavior();

    IntragranularDiffusion();

    SetPhaseDiagram();

    GrainBoundaryMicroCracking();

    GrainBoundaryVenting();

    InterGranularBubbleBehavior();

    FissionProductRelease();

    // GAP BEHAVIOUR

    JOGFormation();
}
