#include "Sciantix.h"

void Sciantix(int Sciantix_options[], double Sciantix_history[], double Sciantix_variables[], double Sciantix_scaling_factors[], double Sciantix_diffusion_modes[])
{
    Simulation* simulation = Simulation::getInstance();

    simulation->initialize(Sciantix_options, Sciantix_history, Sciantix_variables, Sciantix_scaling_factors, Sciantix_diffusion_modes);

    simulation->execute();

    simulation->UpdateVariables(Sciantix_variables, Sciantix_diffusion_modes);

    simulation->FiguresOfMerit();

    simulation->Output();
}