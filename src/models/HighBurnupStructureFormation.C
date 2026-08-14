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
    if (!int(input_variable["iHighBurnupStructureFormation"].getValue()))
        return;

    // Model declaration
    Model model_;

    model_.setName("High-burnup structure formation");

    std::string         reference;
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
            reference +=
                ": Barani et al. Journal of Nuclear Materials 539 (2020) 152296 (original KJMA, no incubation burnup)";

            double avrami_constant(3.54);
            double transformation_rate(2.77e-7);
            double resolution_layer_thickness   = 1.0e-9;  // (m)
            double resolution_critical_distance = 1.0e-9;  // (m)
            double hbs_incubation_burnup        = 0.0;     // MWd/kgHM

            parameter.push_back(avrami_constant);
            parameter.push_back(transformation_rate);
            parameter.push_back(resolution_layer_thickness);
            parameter.push_back(resolution_critical_distance);
            parameter.push_back(hbs_incubation_burnup);

            break;
        }

        case 2:
        {
            reference += ": Barani et al. Journal of Nuclear Materials 539 (2020) 152296; incubation burnup bu_inc = "
                         "15 MWd/kgHM from Biswas & Aagesen Comput. Mater. Sci. 258 (2025) 114052, Eq. 45; parameter "
                         "selection Zullo (2026)";

            double avrami_constant(3.54);
            double transformation_rate(2.77e-7);
            double resolution_layer_thickness   = 1.0e-9;  // (m)
            double resolution_critical_distance = 1.0e-9;  // (m)
            // HBS-formation incubation burnup (MWd/kgHM). Below this value
            // neither grain sub-division (alpha_r) nor pore nucleation (nu_P)
            // are active, following the modified KJMA formulation of Biswas &
            // Aagesen 2025 (Comput. Mater. Sci. 258, 114052, Eq. 45) derived
            // from the dislocation-energy vs subgrain-formation-energy balance.
            double hbs_incubation_burnup = 15.0;  // MWd/kgHM

            parameter.push_back(avrami_constant);
            parameter.push_back(transformation_rate);
            parameter.push_back(resolution_layer_thickness);
            parameter.push_back(resolution_critical_distance);
            parameter.push_back(hbs_incubation_burnup);

            break;
        }

        case 3:
        {
            // Veshchunov & Shestak, J. Nucl. Mater. 384 (2009) 12-18, Fig. 4
            //   -> dislocation-density correlation rho_d(bu, T) and HBS
            //      nucleation threshold rho_crit
            // Zullo (2026), KJMA(rho_d) calibration on PIE data from
            // Gerczak (2018) and Noirot (2015) -> K_rho, gamma_rho
            reference +=
                ": dislocation-density KJMA, Veshchunov & Shestak J. Nucl. Mater. 384 (2009) 12-18; fit Zullo (2026)";

            double A_fit     = 6.545e12;  // (m^-2) / (MWd/kgHM)^n prefactor
            double n_fit     = 1.151;     // burnup exponent
            double A_inf     = 0.608;     // high-T plateau of temperature factor
            double Tc        = 1109.0;    // (K) sigmoid centre
            double dT        = 25.8;      // (K) sigmoid width
            double rho_crit  = 6.0e14;    // (m^-2) HBS nucleation threshold (Veshchunov 2009)
            double rho_scale = 1.0e15;    // (m^-2) normalization so that xi is dimensionless and O(1)
            double K_rho     = 2.597;     // (-) KJMA(rho) prefactor, fit on PIE (Zullo 2026)
            double gamma_rho = 1.104;     // (-) KJMA(rho) exponent, fit on PIE (Zullo 2026)

            parameter.push_back(A_fit);
            parameter.push_back(n_fit);
            parameter.push_back(A_inf);
            parameter.push_back(Tc);
            parameter.push_back(dT);
            parameter.push_back(rho_crit);
            parameter.push_back(rho_scale);
            parameter.push_back(K_rho);
            parameter.push_back(gamma_rho);

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__,
                                  "iHighBurnupStructureFormation",
                                  int(input_variable["iHighBurnupStructureFormation"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    const int option = int(input_variable["iHighBurnupStructureFormation"].getValue());

    if (option == 1 || option == 2)
    {
        // Model resolution
        // Analytic integral of the modified KJMA with incubation burnup:
        //   alpha_r = 1 - exp[-K * (bu_eff_U - bu_inc)^n]    for bu_eff_U > bu_inc
        //   alpha_r = 0                                        otherwise
        // Unlike the Decay ODE solver, the analytic form is robust across the
        // bu_inc crossing, where dalpha_r/dbu is formally discontinuous.
        double n_avrami         = model["High-burnup structure formation"].getParameter().at(0);
        double K_transformation = model["High-burnup structure formation"].getParameter().at(1);
        double bu_inc           = model["High-burnup structure formation"].getParameter().at(4);
        double bu_eff_U         = sciantix_variable["Effective burnup"].getFinalValue() / 0.8814;

        double alpha_r_new = 0.0;
        if (bu_eff_U > bu_inc)
        {
            double bu_delta = bu_eff_U - bu_inc;
            alpha_r_new     = 1.0 - exp(-K_transformation * pow(bu_delta, n_avrami));
        }
        sciantix_variable["Restructured volume fraction"].setFinalValue(alpha_r_new);
    }
    else if (option == 3)
    {
        // Dislocation density as a function of LOCAL burnup (MWd/kgHM) and
        // local temperature (K):
        //   rho_d(bu, T) = A * bu^n * [A_inf + (1 - A_inf) / (1 + exp((T - Tc)/dT))]
        //
        // Burnup input: we deliberately use "Burnup" (local/total) rather than
        // "Effective burnup". EffectiveBurnup.C already applies a Holt-style
        // Heaviside that zeroes the burnup accumulation above T = 1273.15 K.
        // The Veshchunov-Shestak fit in Fig. 4 was calibrated against total
        // burnup, and the thermal suppression of HBS is already carried by
        // the sigmoid f(T) = A_inf + (1 - A_inf)/(1 + exp((T - Tc)/dT)) in the
        // correlation itself. Feeding bu_eff here would apply the thermal
        // cutoff twice, which is physically and numerically inconsistent with
        // how the fit was built.
        //
        // The restructured volume fraction is obtained by a KJMA-like
        // expression in which the progress variable is the (excess)
        // dislocation density, normalized against rho_scale to keep the fit
        // parameters dimensionless and O(1). K_rho and gamma_rho come from
        // a direct fit against PIE data (Gerczak 2018 / Noirot 2015)
        // assuming T = 900 K at the rim positions of the reported samples.
        // See Zullo (2026). The monotonic lock below preserves the
        // irreversibility of HBS across timesteps.
        double A_fit     = model["High-burnup structure formation"].getParameter().at(0);
        double n_fit     = model["High-burnup structure formation"].getParameter().at(1);
        double A_inf     = model["High-burnup structure formation"].getParameter().at(2);
        double Tc        = model["High-burnup structure formation"].getParameter().at(3);
        double dT        = model["High-burnup structure formation"].getParameter().at(4);
        double rho_crit  = model["High-burnup structure formation"].getParameter().at(5);
        double rho_scale = model["High-burnup structure formation"].getParameter().at(6);
        double K_rho     = model["High-burnup structure formation"].getParameter().at(7);
        double gamma_rho = model["High-burnup structure formation"].getParameter().at(8);

        double bu_local_HM = sciantix_variable["Burnup"].getFinalValue() / 0.8814;
        double T           = history_variable["Temperature"].getFinalValue();

        double rho_d = 0.0;
        if (bu_local_HM > 0.0)
        {
            double temp_factor = A_inf + (1.0 - A_inf) / (1.0 + exp((T - Tc) / dT));
            rho_d              = A_fit * pow(bu_local_HM, n_fit) * temp_factor;
        }

        double xi        = std::max((rho_d - rho_crit) / rho_scale, 0.0);
        double f_instant = 1.0 - std::exp(-K_rho * std::pow(xi, gamma_rho));

        // Cap strictly below 1 to preserve KJMA asymptotic behaviour and
        // protect the downstream porosity sweeping term, which computes
        // 1/(1 - alpha) and would produce inf/NaN if alpha reached exactly 1.
        const double f_max = 1.0 - 1.0e-9;
        f_instant          = std::min(f_max, f_instant);

        double alpha_r_old = sciantix_variable["Restructured volume fraction"].getInitialValue();
        double alpha_r_new = std::min(f_max, std::max(alpha_r_old, f_instant));

        sciantix_variable["Restructured volume fraction"].setFinalValue(alpha_r_new);
        sciantix_variable["Dislocation density"].setFinalValue(rho_d);
    }
}