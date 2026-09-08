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

        case 4:
        {
            // HBS formation as a second-order phase transition (Landau functional).
            // Reference implementation and calibration:
            //   utilities/HBSformation/hbs_formation_landau.py  (the model)
            //   utilities/HBSformation/calibrate.py             (beta, k, rho_c)
            //   utilities/HBSformation/README.md                (the derivation)
            //
            // The order parameter is the mean misorientation of the subgrains
            // normalized to its maximum, eta = theta/theta_max. The functional is
            // built by splitting the dislocations into three populations that must
            // add up to rho_tot -- free, condensed into low-angle walls, annihilated
            // by the sweeping boundaries -- and giving each the line energy of the
            // state it is in. Only the FREE dislocations are swept: the ones already
            // in a wall belong to the boundary, not to the volume the boundary
            // passes through (Gourdet & Montheillet, Acta Mater. 51 (2003) 2685).
            reference += ": Landau functional, HBS as a second-order phase transition, Cappellari (2026); "
                         "dislocation density Nogita & Une Nucl. Instrum. Methods B 91 (1994) 301-306; "
                         "elastic constants NEA/NSC/R(2024)1 (2025) p. 124; "
                         "dislocation balance after Gourdet & Montheillet Acta Mater. 51 (2003) 2685-2699";

            // --- calibrated, offsets 0-3, printed ready to paste by calibrate.py ---
            double n_families = 2.0;                  // (-)     dislocation families in a wall
            double beta       = 33.54724855333423;    // (-)     wall geometry
            double k_sweep    = 0.04696637283583627;  // (-)     sweeping
            double rho_c      = 1165846255229680.0;   // (m^-2)  strain-field cut-off

            // --- fixed, offsets 4-7 -------------------------------------------
            // theta_max is a pure NORMALIZATION: every physical quantity depends on
            // theta = eta*theta_max alone, so it cancels out of Theta, r_n and X at
            // fixed beta, k and rho_c (the reference implementation's --selftest
            // checks this). It is therefore set to the LAGB/HAGB boundary itself, so
            // that eta runs over the full [0, 1] and
            //     eta = 1  <=>  Theta = theta_HAGB  <=>  rho_ord = rho_tot
            // all coincide, at 91.32 GWd/tU.
            double theta_hagb = 10.0;                   // (deg)   LAGB/HAGB boundary
            double theta_max  = theta_hagb * M_PI / 180.0;  // (rad) = 0.174533
            double theta_u    = 2.20;                   // (deg)   measured median AMis2Mean
            double burgers    = 3.889087296526011e-10;  // (m)     Djonovic thesis

            parameter.push_back(n_families);
            parameter.push_back(beta);
            parameter.push_back(k_sweep);
            parameter.push_back(rho_c);
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
    else if (option == 4)
    {
        // Landau functional. This block mirrors hbs_state() of
        // utilities/HBSformation/hbs_formation_landau.py statement by statement, in
        // the same order, so that the two can be read side by side;
        // compare_with_sciantix.py checks them against each other with no tolerance.
        //
        // Burnup input: "Burnup" (local), not "Effective burnup", for the same
        // reason as option 3 -- rho_tot(bu) of Nogita & Une was correlated against
        // the total burnup, and applying the Holt-style thermal cutoff of
        // EffectiveBurnup.C on top of it would suppress the accumulation twice.
        double n_families = model["High-burnup structure formation"].getParameter().at(0);
        double beta       = model["High-burnup structure formation"].getParameter().at(1);
        double k_sweep    = model["High-burnup structure formation"].getParameter().at(2);
        double rho_c      = model["High-burnup structure formation"].getParameter().at(3);
        double theta_max  = model["High-burnup structure formation"].getParameter().at(4);
        double theta_hagb = model["High-burnup structure formation"].getParameter().at(5);
        double theta_u    = model["High-burnup structure formation"].getParameter().at(6);
        double burgers    = model["High-burnup structure formation"].getParameter().at(7);

        double bu_local_HM = sciantix_variable["Burnup"].getFinalValue() / 0.8814;
        double T           = history_variable["Temperature"].getFinalValue();
        double P           = sciantix_variable["Porosity"].getFinalValue();
        // StoichiometryDeviation() runs after this model, so this is the value of
        // the previous time step. It is immaterial: as shown below, it cancels out
        // of the three outputs entirely and survives only in C0.
        double x_dev       = sciantix_variable["Stoichiometry deviation"].getFinalValue();
        // GrainGrowth() also runs after this model, so this is the grain radius at
        // the start of the step. It is used only as the ceiling of Eq. (9).
        double R_grain     = sciantix_variable["Grain radius"].getFinalValue();

        // (1) dislocation density -- Nogita & Une (1994)
        //     log10(rho_tot) = 2.2e-2 * bu + 13.8, bu in MWd/kgU = GWd/tU
        double rho_tot = std::pow(10.0, 2.2e-2 * bu_local_HM + 13.8);

        // (2) elastic constants -- NEA/NSC/R(2024)1 p. 124. Both correlations have
        //     the same four-factor shape: composition, porosity, deviation from
        //     stoichiometry, temperature. The plutonium fraction q is 0 for UO2.
        double G_matrix = 1.0e9 * 82.52 * std::pow(1.0 - P, 2.0) / (1.0 + 0.95275 * P) *
                          (1.0 - 2.88078 * x_dev + 15.49419 * x_dev * x_dev) *
                          (1.009549 - 1.182e-5 * T - 6.671e-8 * T * T);
        double nu = 0.32051 * (1.0 - 1.03223 * P) * (1.0 + 0.69962 * x_dev - 7.52905 * x_dev * x_dev) *
                    (1.017906 - 6.420e-5 * T + 1.506e-8 * T * T);
        // f(nu) = (1 - nu/2)/(1 - nu), the edge/screw average of the dislocation
        // line energy prefactor (Hansen, Mater. Sci. Eng. 81 (1986) 141).
        double f_nu = (1.0 - 0.5 * nu) / (1.0 - nu);
        double gb2  = G_matrix * burgers * burgers;

        // (3) wall geometry. Dislocations at spacing d give theta = b/d, so a wall
        //     carrying n families has line length n*theta/b per unit area, and the
        //     low-angle boundary area per unit volume is (S/V) = 3*sqrt(rho_LAGB)/beta.
        double rho_lagb_max  = std::pow(3.0 * n_families * theta_max / (beta * burgers), 2.0);
        double s_over_v_max  = 9.0 * n_families * theta_max / (beta * beta * burgers);
        double dr_over_r_max = k_sweep * rho_lagb_max / rho_tot;

        // (4) the two logarithmic cut-offs of the dislocation line energy,
        //     E_D = G b^2 f(nu)/(4 pi) * ln(R/b)
        double a1 = f_nu / (4.0 * M_PI) * std::log(std::pow(rho_c, -0.5) / burgers);    // random array
        double a2 = f_nu / (4.0 * M_PI) * std::log(std::pow(rho_tot, -0.5) / burgers);  // screened in the wall

        // (5)-(6) the partition, collected into F = C0 + C2 eta^2 + C4 eta^4
        double c0 = rho_tot * a1 * gb2;                          // free dislocations
        double c2 = rho_lagb_max * (a2 - a1) * gb2               // condensed into walls
                    - rho_tot * dr_over_r_max * a1 * gb2;        // sweep, second order
        double c4 = rho_lagb_max * dr_over_r_max * a1 * gb2;     // sweep, fourth order

        // (7) stationary point: eta^2 = -C2/(2 C4), zero where C2 >= 0. No guard is
        //     needed on virgin fuel: the functional gives C2 > 0 at bu = 0 on its own.
        double eta_stationary = std::sqrt(std::max(-c2 / (2.0 * c4), 0.0));

        // (7b) admissibility. The walls cannot hold more dislocations than exist, so
        //      rho_ord = rho_LAGB_max*eta^2 <= rho_tot. The equilibrium is the minimum
        //      of F on 0 <= eta <= eta_balance, not the free stationary point. On the
        //      bound theta = beta*b*sqrt(rho_tot)/(3n), the classical theta ~ sqrt(rho):
        //      every dislocation is in a wall, so the misorientation can only grow as
        //      fast as the dislocations that feed it, and the sweep stops because there
        //      is nothing free left to sweep. Without this bound the walls hold up to
        //      twice the dislocations that exist over 66-98 GWd/tU.
        double eta_balance = std::sqrt(std::min(rho_tot / rho_lagb_max, 1.0));

        // (8) mean misorientation, capped at the LAGB/HAGB boundary   <-- output 1
        //     With theta_max = theta_HAGB the cap is eta <= 1, which coincides with
        //     eta_balance at saturation; the min over all three is what matters.
        double eta_hagb = (theta_hagb * M_PI / 180.0) / theta_max;
        double eta      = std::min(std::min(eta_stationary, eta_balance), eta_hagb);
        double theta    = eta * theta_max * 180.0 / M_PI;
        eta             = (theta * M_PI / 180.0) / theta_max;  // re-derived after the cap

        // (9) subgrain radius, capped at the host grain               <-- output 2
        //     SCIANTIX writes 0.0 below the threshold, where there is no substructure
        //     and the radius is not a length; the reference implementation writes nan.
        double s_over_v  = s_over_v_max * eta;
        double dr_over_r = dr_over_r_max * eta * eta;
        double r_n       = 0.0;
        if (s_over_v > 0.0)
            r_n = std::min(1.5 / s_over_v * (1.0 + dr_over_r), R_grain);

        // (10) restructured fraction, lever rule                      <-- output 3
        //      The measured Theta is the mean over the EBSD map, i.e. the weighted
        //      mean of a two-phase mixture; the fraction is recovered by inverting it.
        const double f_max = 1.0 - 1.0e-9;
        double f_instant   = (theta - theta_u) / (theta_hagb - theta_u);
        f_instant          = std::min(f_max, std::max(f_instant, 0.0));

        // Monotonic lock, as in option 3: HBS formation is irreversible. With the
        // burnup non-decreasing and the three outputs functions of the burnup alone
        // this is already satisfied, so the lock only guards against a history that
        // steps the burnup backwards.
        double alpha_r_old = sciantix_variable["Restructured volume fraction"].getInitialValue();
        double alpha_r_new = std::min(f_max, std::max(alpha_r_old, f_instant));

        sciantix_variable["Restructured volume fraction"].setFinalValue(alpha_r_new);
        sciantix_variable["Dislocation density"].setFinalValue(rho_tot);
        sciantix_variable["Mean misorientation"].setFinalValue(theta);
        sciantix_variable["Subgrain radius"].setFinalValue(r_n);

        // c0 is the stored energy available for the nucleation criterion. It is not
        // an output; referenced here so the compiler does not warn it away, and so
        // that the bridge to Muramatsu et al. (2014) Eq. 8 stays visible in the port:
        //   C0 / (rho_tot G b^2 / 2) = f(nu) ln(rho_c^-1/2 / b) / (2 pi).
        (void)c0;
    }
}