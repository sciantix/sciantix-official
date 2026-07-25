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

void Simulation::Densification()
{
    if (!int(input_variable["iDensification"].getValue()))
        return;

    // Model declaration
    Model model_;
    model_.setName("Densification");

    std::vector<double> parameter;
    std::string         reference;

    switch (int(input_variable["iDensification"].getValue()))
    {
        case 0:
        {
            parameter.push_back(0);
            parameter.push_back(0);

            reference = ": not considered.";

            break;
        }
        case 1:
        {
            // Pagani et al. (2026), Eqs. (8)-(9): df_dens/dbeta = -k_beta * f_dens + S(T),
            // S(T) = k_T1 * exp(k_T2 * T). f_dens saturates at S(T)/k_beta, so temperature
            // sets how much of the densifiable porosity anneals out. Two divergences from the
            // published text: Eq. (8) is printed with a positive k_beta (growth form), and it
            // calls f_dens the fraction of the original fabrication porosity, whereas it is
            // the fraction of the densifiable part - a factor four smaller.
            const double k_beta = 2.0;
            const double k_T1   = 0.006;
            const double k_T2   = 0.002;

            parameter.push_back(k_beta);
            parameter.push_back(k_T1 * exp(k_T2 * history_variable["Temperature"].getFinalValue()));

            reference = ": Pagani et al., Journal of Nuclear Materials (2026), Eqs. (8)-(9)";

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iDensification", int(input_variable["iDensification"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);
    model.push(model_);

    double dens_factor = solver.Decay(sciantix_variable["Densification factor"].getInitialValue(),
                                      model["Densification"].getParameter().at(0),
                                      model["Densification"].getParameter().at(1),
                                      sciantix_variable["Burnup"].getIncrement());

    if (dens_factor > 1.0)
        dens_factor = 1.0;
    else if (dens_factor < 0.0)
        dens_factor = 0.0;

    sciantix_variable["Densification factor"].setFinalValue(dens_factor);

    // f_dens is cumulative: the porosity is a closed-form function of it, evaluated from the
    // as-fabricated value. Applying (1 - f) to the running value instead compounds the
    // reduction once per step, making the result depend on the step count, not on burnup.
    // The densifiable part follows from the residual porosity alone (Initialization.C fixes
    // the residual at a constant fraction r of the as-fabricated one), which keeps this
    // independent of an initialisation that does not run in coupled builds.
    const double residual_porosity_fraction = 0.75;  // as in Initialization.C
    const double residual_porosity          = sciantix_variable["Residual porosity"].getFinalValue();
    const double densifiable_porosity =
        residual_porosity * (1.0 - residual_porosity_fraction) / residual_porosity_fraction;

    sciantix_variable["Fabrication porosity"].setFinalValue(
        residual_porosity + densifiable_porosity * (1.0 - sciantix_variable["Densification factor"].getFinalValue()));

    sciantix_variable["Porosity"].addValue(sciantix_variable["Fabrication porosity"].getIncrement());
}