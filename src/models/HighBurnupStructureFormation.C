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

        case 4:  // author: E.Cappellari, POLIMI-CEA 2026
        {
            // HBS formation as a continuous transition, order parameter the mean
            // misorientation, equilibrium of the dislocation energy.
            // Reference implementation and calibration:
            //   utilities/HBSformation/hbs_formation_landau.py  (the model)
            //   utilities/HBSformation/calibrate.py             (beta, k, rho_crit)
            //   utilities/HBSformation/README.md                (the derivation)
            reference +=
                ": Landau functional, HBS as a continuous transition, Cappellari (2026); "
                "dislocation source Nogita & Une Nucl. Instrum. Methods B 91 (1994) 301-306, above a critical density "
                "(cf. Veshchunov & Shestak J. Nucl. Mater. 384 (2009) 12-18); "
                "Read-Shockley line-energy cut-offs, Humphreys, Rohrer & Rollett (2017) Eqs. 2.6, 4.4-4.5; "
                "dislocation balance after Gourdet & Montheillet Acta Mater. 51 (2003) 2685-2699";

            // --- fixed, offsets 4-7 -------------------------------------------
            double theta_hagb = 10.0;                         // (deg)   LAGB/HAGB boundary
            double theta_max  = theta_hagb * (M_PI / 180.0);  // (rad) = 0.174533, as Python's math.radians
            // (deg) lower end member of the mixture Eq. (10) inverts. Set by the EBSD
            // binning, not fitted: the dataset reports a restructured fraction at 1 deg
            // and one at 10 deg, so 1 deg is the threshold below which a boundary is
            // not counted at all.
            double theta_u = 1.0;
            double burgers = 3.889087296526011e-10;  // (m)     Djonovic thesis

            // --- calibrated, offsets 0-3, printed ready to paste by calibrate.py ---
            parameter.push_back(2.0);                 // n, dislocation families in a wall
            parameter.push_back(21.36831476383452);   // beta, wall geometry
            parameter.push_back(0.6787994413909928);  // k, sweeping
            parameter.push_back(685421967748407.1);   // rho_crit, critical dislocation density (m^-2)
            parameter.push_back(theta_max);
            parameter.push_back(theta_hagb);
            parameter.push_back(theta_u);
            parameter.push_back(burgers);

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
    else if (option == 4)  // author: E.Cappellari, POLIMI-CEA 2026
    {
        // This block mirrors hbs_state() of utilities/HBSformation/hbs_formation_landau.py
        // statement by statement, with the same arithmetic, so that
        // compare_with_sciantix.py can check the two against each other. The
        // equilibrium is a numerical minimum: any change to the order of the
        // operations below must be made in the Python as well.
        double n_families = model["High-burnup structure formation"].getParameter().at(0);
        double beta       = model["High-burnup structure formation"].getParameter().at(1);
        double k_sweep    = model["High-burnup structure formation"].getParameter().at(2);
        double rho_crit   = model["High-burnup structure formation"].getParameter().at(3);
        double theta_max  = model["High-burnup structure formation"].getParameter().at(4);
        double theta_hagb = model["High-burnup structure formation"].getParameter().at(5);
        double theta_u    = model["High-burnup structure formation"].getParameter().at(6);
        double burgers    = model["High-burnup structure formation"].getParameter().at(7);

        double bu_local_HM = sciantix_variable["Burnup"].getFinalValue() / 0.8814;
        // GrainGrowth() runs after this model, so this is the grain radius at the
        // start of the step. It is used only as the ceiling of Eq. (9).
        double R_grain = sciantix_variable["Grain radius"].getFinalValue();
        // Temperature, porosity and stoichiometry enter F only through the common
        // factor f(nu) G b^2 / (4 pi), which does not move its minimum: none of the
        // three outputs depends on them, so they are not read here.

        // (1) dislocations available to polygonize -- Nogita & Une (1994), read as a
        //     pure source above the critical density: max(rho(bu) - rho_crit, 0),
        //     bu in MWd/kgU = GWd/tU. rho_crit is the fixed density scale that sets the
        //     continuous threshold; F itself has none (every length in it scales as rho^-1/2).
        double rho_tot = std::max(std::pow(10.0, 2.2e-2 * bu_local_HM + 13.8) - rho_crit, 0.0);

        double eta          = 0.0;
        double rho_lagb_max = 0.0, s_over_v_max = 0.0, swept_max = 0.0;
        if (rho_tot > 0.0)
        {
            // (3) wall geometry. Dislocations at spacing d give theta = b/d, so a wall
            //     carrying n families has line length n*theta/b per unit area, and the
            //     low-angle boundary area per unit volume is (S/V) = 3*sqrt(rho_LAGB)/beta.
            //     x = k rho_ord / rho_tot is the extended volume swept by the boundaries.
            rho_lagb_max = std::pow(3.0 * n_families * theta_max / (beta * burgers), 2.0);
            s_over_v_max = 9.0 * n_families * theta_max / (beta * beta * burgers);
            swept_max    = k_sweep * rho_lagb_max / rho_tot;

            // (7b) admissibility, and the LAGB/HAGB cap of Eq. (8)
            double eta_balance = std::sqrt(std::min(rho_tot / rho_lagb_max, 1.0));
            double eta_hagb    = (theta_hagb * (M_PI / 180.0)) / theta_max;
            double eta_upper   = std::min(eta_balance, eta_hagb);

            // (4)-(6) (F - C0) / (f(nu) G b^2 / 4 pi) = -rho_swept L1 + rho_ord (L2 - L1),
            //     L = ln(R/b), R = spacing of the dislocations: rho_tot^-1/2 in the random
            //     array, b/theta in a Read-Shockley wall (capped at rho_tot^-1/2).
            //     rho_free = (rho_tot - rho_ord) exp(-x): Gourdet & Montheillet's
            //     d rho_i = -rho_i dV integrated over the swept volume.
            auto reduced_energy = [&](double e)
            {
                double spacing  = std::pow(rho_tot, -0.5);
                double log_free = std::log(spacing / burgers);
                double theta_e  = e * theta_max;
                double log_wall = (theta_e * spacing <= burgers) ? log_free : std::log(burgers / theta_e / burgers);
                double rho_ord  = std::min(rho_lagb_max * e * e, rho_tot);
                double rho_free = (rho_tot - rho_ord) * std::exp(-swept_max * e * e);
                double rho_swep = rho_tot - rho_ord - rho_free;
                return -rho_swep * log_free + rho_ord * (log_wall - log_free);
            };

            // (7) minimum of F on [0, eta_upper]: coarse scan, then golden section
            const int    nodes     = 400;
            const double tolerance = 1e-13;
            int          best      = 0;
            double       best_e    = reduced_energy(0.0);
            for (int i = 1; i <= nodes; ++i)
            {
                double e_i = reduced_energy(eta_upper * i / nodes);
                if (e_i < best_e)
                {
                    best_e = e_i;
                    best   = i;
                }
            }
            double low   = eta_upper * std::max(best - 1, 0) / nodes;
            double high  = eta_upper * std::min(best + 1, nodes) / nodes;
            double ratio = (std::sqrt(5.0) - 1.0) / 2.0;
            while (high - low > tolerance)
            {
                double left  = high - ratio * (high - low);
                double right = low + ratio * (high - low);
                if (reduced_energy(left) < reduced_energy(right))
                    high = right;
                else
                    low = left;
            }
            eta = 0.5 * (low + high);
            // on the bound (7b) or the HAGB cap, exactly: F is flat there to rounding,
            // so a bracket closing on eta_upper counts as reaching it
            if (eta_upper - eta <= tolerance || reduced_energy(eta_upper) <= reduced_energy(eta))
                eta = eta_upper;
            if (!(reduced_energy(eta) < 0.0))
                eta = 0.0;
        }

        // (8) mean misorientation
        double theta = eta * theta_max * (180.0 / M_PI);
        eta          = (theta * (M_PI / 180.0)) / theta_max;  // re-derived after the cap

        // (9) subgrain radius, capped at the host grain; the walls in the swept volume
        //     go too (Gourdet & Montheillet Eq. 8), so the wall area is S/V exp(-x)
        double s_over_v = s_over_v_max * eta * std::exp(-swept_max * eta * eta);
        double r_n      = 0.0;
        if (s_over_v > 0.0)
            r_n = std::min(1.5 / s_over_v, R_grain);

        // (10) restructured fraction, lever rule
        const double f_max     = 1.0 - 1.0e-9;
        double       f_instant = (theta - theta_u) / (theta_hagb - theta_u);
        f_instant              = std::min(f_max, std::max(f_instant, 0.0));

        // Monotonic lock, as in option 3: HBS formation is irreversible.
        double alpha_r_old = sciantix_variable["Restructured volume fraction"].getInitialValue();
        double alpha_r_new = std::min(f_max, std::max(alpha_r_old, f_instant));

        sciantix_variable["Restructured volume fraction"].setFinalValue(alpha_r_new);
        sciantix_variable["Dislocation density"].setFinalValue(rho_tot);
        sciantix_variable["Mean misorientation"].setFinalValue(theta);
        sciantix_variable["Subgrain radius"].setFinalValue(r_n);
    }
}