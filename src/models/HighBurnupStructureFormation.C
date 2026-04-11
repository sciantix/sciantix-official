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

void Simulation::HighBurnupStructureFormation()
{
    if (!int(input_variable["iHighBurnupStructureFormation"].getValue())) return;

    // Model declaration
    Model model_;

    model_.setName("High-burnup structure formation");

    std::string reference;
    std::vector<double> parameter;

    switch (int(input_variable["iHighBurnupStructureFormation"].getValue()))
    {
        case 0:
        {
            reference += ": not considered.";
            parameter.push_back(0.0);
            parameter.push_back(0.0);
            parameter.push_back(0.0);
            parameter.push_back(0.0);

            break;
        }

        case 1:
        {
            reference += ": Barani et al. Journal of Nuclear Materials 539 (2020) 152296";

            double avrami_constant(3.54);
            double transformation_rate(2.77e-7);
            double resolution_layer_thickness = 1.0e-9; // (m)
            double resolution_critical_distance = 1.0e-9; // (m)
            // HBS-formation incubation burnup (MWd/kgU). Below this value
            // neither grain sub-division (alpha_r) nor pore nucleation (nu_P)
            // are active, following the modified JMAK formulation of Biswas &
            // Aagesen 2025 (Comput. Mater. Sci. 258, 114052, Eq. 45) derived
            // from the dislocation-energy vs subgrain-formation-energy balance.
            double hbs_incubation_burnup = 20.0; // MWd/kgU

            parameter.push_back(avrami_constant);
            parameter.push_back(transformation_rate);
            parameter.push_back(resolution_layer_thickness);
            parameter.push_back(resolution_critical_distance);
            parameter.push_back(hbs_incubation_burnup);

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iHighBurnupStructureFormation", int(input_variable["iHighBurnupStructureFormation"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    // Analytic integral of the modified KJMA with incubation burnup:
    //   alpha_r = 1 - exp[-K * (bu_eff_U - bu_inc)^n]    for bu_eff_U > bu_inc
    //   alpha_r = 0                                        otherwise
    // Unlike the Decay ODE solver, the analytic form is robust across the
    // bu_inc crossing, where dalpha_r/dbu is formally discontinuous.
    double n_avrami = model["High-burnup structure formation"].getParameter().at(0);
    double K_transformation = model["High-burnup structure formation"].getParameter().at(1);
    double bu_inc = model["High-burnup structure formation"].getParameter().at(4);
    double bu_eff_U = sciantix_variable["Effective burnup"].getFinalValue() / 0.8814;

    double alpha_r_new = 0.0;
    if (bu_eff_U > bu_inc)
    {
        double bu_delta = bu_eff_U - bu_inc;
        alpha_r_new = 1.0 - exp(- K_transformation * pow(bu_delta, n_avrami));
    }
    sciantix_variable["Restructured volume fraction"].setFinalValue(alpha_r_new);
}