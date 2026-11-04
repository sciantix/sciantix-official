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

#include "Solver.h"
#include <algorithm>
#include <cmath>
#include <iostream>

// Vacancies per mean HBS pore after one time step, by the Speight-Beere flow
// (Barani 2022, Eq. 14) solved fully implicitly (backward Euler):
//   n_vp = n_vp0 + dt * 2*pi*rho_P*D/(zeta(psi) kT) * (p_P(n_vp) - p_eq(n_vp)),
//   p_P  = Z(y) n kT / V_p,  V_p = n*Omega_Xe + n_vp*Omega_vac  (Barani 2022, Eq. 16),
//   p_eq = 2 gamma / R_p - sigma_h,  y = n*Omega_Xe/V_p (capped at 0.65).
// For frozen Z, zeta and p_eq the step has the closed form of LimitedGrowth in
// (n_vp + u), u = n*Omega_Xe/Omega_vac. Z, zeta and p_eq are then iterated to
// convergence (under-relaxed fixed point), so that they are evaluated with the
// end-of-step volume. Evaluating them with the start-of-step volume produced a
// step-to-step oscillation of n_vp for nanometric pores (capillary pressure of GPa).
// n_vp is kept >= 0. a, b return the last growth and equilibrium coefficients.
static double ImplicitVacanciesPerPore(double n_vp0, double n_xe, double N_p, double omega_xe,
                                       double omega_vac, double D_v, double gamma, double sigma_h,
                                       double kT, double dt, double& a, double& b)
{
    a = 0.0;
    b = 0.0;
    if (N_p <= 0.0 || n_xe <= 0.0 || dt <= 0.0)
        return n_vp0;

    const double rho = pow(3.0 / (4.0 * M_PI * N_p), 1.0 / 3.0);
    const double u   = n_xe * omega_xe / omega_vac;

    auto step = [&](double n_vp) {
        double V   = n_xe * omega_xe + n_vp * omega_vac;
        double y   = std::min(0.65, n_xe * omega_xe / V);
        double Z   = (1.0 + y + y * y - y * y * y) / pow(1.0 - y, 3.0);
        double R   = 0.620350491 * pow(V, 1.0 / 3.0);
        double psi = std::min(0.7, R / rho);
        double zeta = 10.0 * psi * (1.0 + pow(psi, 3.0)) / (-pow(psi, 6.0) + 5.0 * psi * psi - 9.0 * psi + 5.0);
        double vfr = 2.0 * M_PI * rho * D_v / zeta;
        a = vfr * n_xe * Z / omega_vac;
        b = -vfr * (2.0 * gamma / R - sigma_h) / kT;
        double x0 = n_vp0 + u + b * dt;
        return std::max(0.0, 0.5 * (x0 + sqrt(x0 * x0 + 4.0 * a * dt)) - u);
    };

    double n_vp = step(n_vp0);
    for (int it = 0; it < 50; ++it)
    {
        double next = 0.5 * (n_vp + step(n_vp));
        if (fabs(next - n_vp) <= 1e-8 * (n_vp + u))
        {
            n_vp = next;
            break;
        }
        n_vp = next;
    }
    return n_vp;
}

void Simulation::HighBurnupStructurePorosity()
{
    if (!int(input_variable["iHighBurnupStructurePorosity"].getValue()))
        return;

    // Model declaration
    Model model_;

    model_.setName("High-burnup structure porosity");

    double porosity_increment = 0.0;

    std::string         reference;
    std::vector<double> parameter;

    Matrix fuel_(matrices["UO2HBS"]);

    switch (int(input_variable["iHighBurnupStructurePorosity"].getValue()))
    {
        case 0:
        {
            reference += "not considered";
            parameter.push_back(0.0);
            sciantix_variable["HBS porosity"].setInitialValue(0.0);
            sciantix_variable["HBS porosity"].setFinalValue(0.0);
            break;
        }

        case 1:
        {
            double rate_coefficient         = 1.3e-3;
            double porosity_upper_threshold = 0.15;
            double burnup_threshold         = 50.0;

            if (sciantix_variable["HBS porosity"].getInitialValue() < porosity_upper_threshold)
            {
                if (sciantix_variable["Burnup"].getFinalValue() < burnup_threshold)
                    porosity_increment = 0.0;
                else
                    porosity_increment = rate_coefficient;
            }

            else
            {
                sciantix_variable["HBS porosity"].setInitialValue(0.15);
                porosity_increment = 0.0;
            }

            reference = ": semi-empirical porosity model based on Spino et al. JNM 354 (2006) 66";

            parameter.push_back(porosity_increment);

            break;
        }

        case 2:
        {
            double alpha     = sciantix_variable["Restructured volume fraction"].getFinalValue();
            double angle_deg = 4.0 * (1.0 - alpha) + 40.0 * alpha;
            double D_sa      = fuel_.getGrainBoundarySingleAtomDiffusivity();
            double D_gb      = D_sa * sin(angle_deg * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);
            double R_pore    = sciantix_variable["HBS pore radius"].getFinalValue();

            // Phenomenological saturation factor F_sat = (1 - xi/xi_sat)^2,
            // applied once, to D_gb^v in the Speight-Beere vacancy flow.
            // xi_sat is a calibrated parameter. F_sat is NOT applied to
            // D_gb^SA / beta_n, so gas trapping continues at high porosity.
            double       xi_old            = sciantix_variable["HBS porosity"].getInitialValue();
            const double xi_sat            = 0.18;
            double       linear            = std::max(0.0, 1.0 - xi_old / xi_sat);
            double       saturation_factor = linear * linear;

            // Nucleation rate with incubation burnup.
            // Following Biswas & Aagesen 2025 (Comput. Mater. Sci. 258, 114052),
            // the modified KJMA is alpha_r = 1 - exp[-K*(bu - bu_inc)^n]
            // for bu > bu_inc, 0 otherwise. The threshold bu_inc is derived from
            // the dislocation-energy vs subgrain-formation-energy balance
            // (f_d > E_sub).
            // This avoids the "gas-reservoir burst" that occurs when the
            // threshold is applied to nu_P alone: grain sub-division produces
            // c_gb^HBS with no pore sink, then the first pores explode when
            // nu_P reactivates.
            double avrami_constant      = model["High-burnup structure formation"].getParameter().at(0);
            double transformation_rate  = model["High-burnup structure formation"].getParameter().at(1);
            double bu_inc               = model["High-burnup structure formation"].getParameter().at(4);
            double bu_for_nucl          = sciantix_variable["Effective burnup"].getFinalValue() / 0.8814 - bu_inc;
            double pore_nucleation_rate = 0.0;
            if (bu_for_nucl > 0.0)
            {
                // nu_P = prefactor * d(alpha_r)/d(bu_U), with bu_U in MWd/kgU.
                // The analytic d(alpha_r)/d(bu_U) below is formed in MWd/kgU (note
                // bu_for_nucl = bu_U - bu_inc). To convert to a time rate the chain
                // rule gives d(bu_U)/dt = (1/0.8814) * d(bu_eff_UO2)/dt, where
                // "Effective burnup" is the sciantix variable stored in MWd/kgUO2
                // (see EffectiveBurnup.C). The /0.8814 factor in the time-rate
                // multiplier below enforces that conversion.
                pore_nucleation_rate = 8.8e17 * transformation_rate * avrami_constant *
                                       (1.0 - sciantix_variable["Restructured volume fraction"].getFinalValue()) *
                                       pow(bu_for_nucl, avrami_constant - 1.);
                if (physics_variable["Time step"].getFinalValue())
                    pore_nucleation_rate *= sciantix_variable["Effective burnup"].getIncrement() /
                                            (0.8814 * physics_variable["Time step"].getFinalValue());
                else
                    pore_nucleation_rate = 0.0;
            }

            // Resolution rate
            double resolution_layer_thickness   = model["High-burnup structure formation"].getParameter().at(2);
            double resolution_critical_distance = model["High-burnup structure formation"].getParameter().at(3);

            double b0(2.0e-23 * history_variable["Fission rate"].getFinalValue());
            double pore_resolution_rate =
                b0 *
                (3.0 * resolution_critical_distance /
                 (3.0 * resolution_critical_distance + sciantix_variable["HBS pore radius"].getFinalValue())) *
                (resolution_layer_thickness /
                 (resolution_layer_thickness + sciantix_variable["HBS pore radius"].getFinalValue()));

            // Sweeping term
            double sweeping_term(0.0);
            if (physics_variable["Time step"].getFinalValue())
                sweeping_term = 1. / (1. - sciantix_variable["Restructured volume fraction"].getFinalValue()) *
                                sciantix_variable["Restructured volume fraction"].getIncrement() /
                                physics_variable["Time step"].getFinalValue();
            if (std::isinf(sweeping_term) || std::isnan(sweeping_term))
                sweeping_term = 0.0;

            // from non-restructured grains to non-restructured grain-boundary (bubbles)
            // = - D nabla^2 G
            double gas_in_gb_from_grain(0.0);
            double swept = sweeping_term * physics_variable["Time step"].getFinalValue() *
                           sciantix_variable["Xe in grain"].getInitialValue();
            gas_in_gb_from_grain = sciantix_variable["Xe produced"].getIncrement() -
                                   sciantix_variable["Xe in grain"].getIncrement() - swept;

            // from HBS grains to HBS grain-boundary (gas in solution in HBS grain boundary)
            // = - D nabla^2 G_HBS
            double gas_in_pores_from_hbs_grain(0.0);
            gas_in_pores_from_hbs_grain = sciantix_variable["Xe produced in HBS"].getIncrement() -
                                          sciantix_variable["Xe in grain HBS"].getIncrement() + swept;

            double N_pore = sciantix_variable["HBS pore density"].getFinalValue();

            reference = ": mechanistic cluster dynamics, Barani et al. JNM 563 (2022) 153627; saturation of the "
                        "vacancy supply and implicit "
                        "Euler corrections from Zullo (2026)";

            // Single-atom trapping rate beta_n: NOT modulated by F_sat.
            // The saturation factor enters only the vacancy channel
            // (D_gb^v in Speight-Beere).
            double trapping_coeff_HBS =
                4.0 * M_PI * D_gb * R_pore * (1.0 + 1.8 * pow(sciantix_variable["HBS porosity"].getFinalValue(), 1.3));
            double total_trapping_rate_HBS = trapping_coeff_HBS * N_pore;
            double dt                      = physics_variable["Time step"].getFinalValue();

            double coeff_matrix[25];
            double initial_conditions[5];

            for (int i = 0; i < 25; ++i)
                coeff_matrix[i] = 0.0;

            coeff_matrix[0] = 1.0 + pore_resolution_rate * dt;

            // Row 1: Xe in HBS pores (A)
            coeff_matrix[6] = 1.0 + pore_resolution_rate * dt;
            coeff_matrix[9] = -total_trapping_rate_HBS * dt;

            // Row 2: Xe in HBS pores - variance (B)
            // The -total_trapping_rate_HBS*dt coupling to c_gb^HBS gives +beta_n*N_p
            // on dB/dt: kinematic broadening from trapping, each capture shifts a
            // pore n -> n+1 and adds 2(n-n_bar)+1 to (n-n_bar)^2 summed over the
            // population. Exact in the mean-field/size-independent limit.
            coeff_matrix[12] = 1.0 + pore_resolution_rate * dt;
            coeff_matrix[14] = -total_trapping_rate_HBS * dt;

            // Row 3: Xe at grain boundary (NR)
            coeff_matrix[18] = 1.0 + sweeping_term * dt;

            // Row 4: Xe at grain boundary HBS
            coeff_matrix[21] = -pore_resolution_rate * dt;
            coeff_matrix[23] = -sweeping_term * dt;
            coeff_matrix[24] = 1.0 + total_trapping_rate_HBS * dt;

            initial_conditions[0] = sciantix_variable["HBS pore density"].getInitialValue() + pore_nucleation_rate * dt;

            initial_conditions[1] =
                sciantix_variable["Xe in HBS pores"].getInitialValue() + 2.0 * pore_nucleation_rate * dt;

            initial_conditions[2] = sciantix_variable["Xe in HBS pores - variance"].getInitialValue() +
                                    pow((sciantix_variable["Xe atoms per HBS pore"].getFinalValue() - 2.0), 2.0) *
                                        pore_nucleation_rate * dt;

            initial_conditions[3] = sciantix_variable["Xe at grain boundary"].getInitialValue() + gas_in_gb_from_grain;
            initial_conditions[4] = sciantix_variable["Xe at grain boundary HBS"].getInitialValue() +
                                    gas_in_pores_from_hbs_grain - 2.0 * pore_nucleation_rate * dt;

            solver.Laplace(5, coeff_matrix, initial_conditions);

            sciantix_variable["HBS pore density"].setFinalValue(initial_conditions[0]);            // Np
            sciantix_variable["Xe in HBS pores"].setFinalValue(initial_conditions[1]);             // A
            sciantix_variable["Xe in HBS pores - variance"].setFinalValue(initial_conditions[2]);  // B
            sciantix_variable["Xe at grain boundary"].setFinalValue(initial_conditions[3]);        // CGB
            sciantix_variable["Xe at grain boundary HBS"].setFinalValue(initial_conditions[4]);    // CGBHBS

            if (sciantix_variable["Xe at grain boundary"].getFinalValue() < 0.0)
                sciantix_variable["Xe at grain boundary"].setFinalValue(0.0);
            if (sciantix_variable["Xe at grain boundary HBS"].getFinalValue() < 0.0)
                sciantix_variable["Xe at grain boundary HBS"].setFinalValue(0.0);
            if (sciantix_variable["Xe in HBS pores"].getFinalValue() < 0.0)
                sciantix_variable["Xe in HBS pores"].setFinalValue(0.0);

            // Xe atoms per HBS pore: n = A / Np
            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore"].setFinalValue(
                    sciantix_variable["Xe in HBS pores"].getFinalValue() /
                    sciantix_variable["HBS pore density"].getFinalValue());

            // HBS pore volume
            double XeHSDiameter =
                4.45e-10 * (0.8542 - 0.03996 * log(history_variable["Temperature"].getFinalValue() / 231.2));
            double gasVolumeInPore = M_PI / 6.0 * pow(XeHSDiameter, 3.0);
            double V_pore_old_step = sciantix_variable["HBS pore volume"].getInitialValue();

            sciantix_variable["HBS pore volume"].setInitialValue(
                sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * gasVolumeInPore +
                sciantix_variable["Vacancies per HBS pore"].getInitialValue() * fuel_.getSchottkyVolume());
            sciantix_variable["HBS pore radius"].setInitialValue(
                0.620350491 * pow(sciantix_variable["HBS pore volume"].getInitialValue(), (1.0 / 3.0)));

            // Vacancy absorption (Speight-Beere), implicit in n_vp, Z, zeta and p_eq.
            // Alpha-weighted tilt correction (Barani 2022, Eq. 7) and saturation
            // factor (phenomenological closure, exponent 2 not calibrated) on D_gb^v
            // only. D_gb^SA in the trapping term is not damped.
            {
                double tilt_factor = sin(angle_deg * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);
                double D_gb_v_eff  = fuel_.getGrainBoundaryVacancyDiffusivity() * tilt_factor * saturation_factor;
                double growth_rate(0.0), equilibrium_term(0.0);
                sciantix_variable["Vacancies per HBS pore"].setFinalValue(ImplicitVacanciesPerPore(
                    sciantix_variable["Vacancies per HBS pore"].getInitialValue(),
                    sciantix_variable["Xe atoms per HBS pore"].getFinalValue(),
                    sciantix_variable["HBS pore density"].getFinalValue(), gasVolumeInPore,
                    fuel_.getSchottkyVolume(), D_gb_v_eff, fuel_.getSurfaceTension(),
                    history_variable["Hydrostatic stress"].getFinalValue() * 1e6,
                    boltzmann_constant * history_variable["Temperature"].getFinalValue(),
                    physics_variable["Time step"].getFinalValue(), growth_rate, equilibrium_term));
                parameter.push_back(growth_rate);
                parameter.push_back(equilibrium_term);
            }

            // HBS pore volume - update with vacancy contribution:
            // V_p = n_Xe*Omega_Xe + n_vac*Omega_Sch. F_sat acts only on D_gb^v
            // above, the pore-volume increment is not capped.
            sciantix_variable["HBS pore volume"].setFinalValue(
                sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * gasVolumeInPore +
                sciantix_variable["Vacancies per HBS pore"].getFinalValue() * fuel_.getSchottkyVolume());

            double V_pore_increment = sciantix_variable["HBS pore volume"].getFinalValue() - V_pore_old_step;

            sciantix_variable["HBS pore radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), 1. / 3.));

            sciantix_variable["HBS porosity"].setFinalValue(sciantix_variable["HBS pore volume"].getFinalValue() *
                                                            sciantix_variable["HBS pore density"].getFinalValue());

            // HBS pore interconnection by impingement.
            double limiting_factor = (2.0 - sciantix_variable["HBS porosity"].getFinalValue()) /
                                     (2.0 * pow(1.0 - sciantix_variable["HBS porosity"].getFinalValue(), 3.0));

            double pore_interconnection_rate = 4.0 * limiting_factor;

            sciantix_variable["HBS pore density"].resetValue();
            sciantix_variable["HBS pore density"].setFinalValue(solver.BinaryInteraction(
                sciantix_variable["HBS pore density"].getInitialValue(), pore_interconnection_rate,
                std::max(0.0, V_pore_increment)));  // impingement only on pore growth

            // Conservation through coalescence: Xe, vacancies and volume per
            // pore are rescaled by N_before/N_after, conserving total atoms,
            // vacancies and pore volume.
            if (sciantix_variable["HBS pore density"].getFinalValue())
            {
                double rescale_factor = sciantix_variable["HBS pore density"].getInitialValue() /
                                        sciantix_variable["HBS pore density"].getFinalValue();

                sciantix_variable["Xe atoms per HBS pore"].rescaleFinalValue(rescale_factor);
                sciantix_variable["Vacancies per HBS pore"].rescaleFinalValue(rescale_factor);

                sciantix_variable["HBS pore volume"].setFinalValue(
                    sciantix_variable["HBS pore volume"].getFinalValue() * rescale_factor);

                sciantix_variable["HBS pore radius"].setFinalValue(
                    0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), 1. / 3.));
            }

            sciantix_variable["HBS porosity"].setFinalValue(sciantix_variable["HBS pore volume"].getFinalValue() *
                                                            sciantix_variable["HBS pore density"].getFinalValue());

            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore - variance"].setFinalValue(
                    sciantix_variable["Xe in HBS pores - variance"].getFinalValue() /
                    sciantix_variable["HBS pore density"].getFinalValue());

            break;
        }

        case 3:
        {
            // Formation-agnostic variant of case 2. Identical cluster-dynamics,
            // saturation, vacancy growth, and interconnection; the only two
            // differences are:
            //
            //   (a) Pore nucleation rate is computed directly from the time
            //       derivative of "Restructured volume fraction", not from
            //       positional reads of formation-model parameters. This makes
            //       the porosity evolution consistent with any formation
            //       option that populates alpha_r monotonically - including
            //       iHighBurnupStructureFormation = 3 (dislocation-density).
            //
            //       For the KJMA paths (options 1 and 2), alpha_r = 1 - exp[-K(bu - bu_inc)^g]
            //       so d(alpha_r)/dbu = K g (1 - alpha_r) (bu - bu_inc)^(g-1)
            //       and case-2's nucleation expression equals 1e18 * d(alpha_r)/dt
            //       by chain rule. Case 3 writes that same identity directly, so
            //       paired with options 1 and 2 it gives numerically very close (but
            //       not bit-identical, finite-difference vs analytical
            //       derivative) results. For option 3 it gives a physically
            //       sensible trajectory where case 2 overflows pow(bu, 6.5e12).
            //
            //   (b) Veshchunov-Tarasov re-solution thicknesses (d_V, delta_V)
            //       are hardcoded at 1 nm (the KJMA-branch values, Barani 2022
            //       Tab 1) instead of being read from the formation-model
            //       parameter vector at positions 2 and 3. Those positions
            //       belong to A_inf (0.608) and Tc (1109 K) under option 3 -
            //       which have nothing to do with re-solution physics.
            double alpha     = sciantix_variable["Restructured volume fraction"].getFinalValue();
            double angle_deg = 4.0 * (1.0 - alpha) + 40.0 * alpha;
            double D_sa      = fuel_.getGrainBoundarySingleAtomDiffusivity();
            double D_gb      = D_sa * sin(angle_deg * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);
            double R_pore    = sciantix_variable["HBS pore radius"].getFinalValue();

            // Phenomenological saturation factor F_sat = (1 - xi/xi_sat)^2, as in
            // case 2: applied once, to D_gb^v. Not applied to D_gb^SA / beta_n.
            double       xi_old            = sciantix_variable["HBS porosity"].getInitialValue();
            const double xi_sat            = 0.18;
            double       linear            = std::max(0.0, 1.0 - xi_old / xi_sat);
            double       saturation_factor = linear * linear;

            // Nucleation rate: nu_P = 1e18 * d(alpha_r)/dt. See (a) above.
            const double pore_nucleation_prefactor = 1.0e18;  // pores/m3 per unit alpha
            double       dalpha                    = sciantix_variable["Restructured volume fraction"].getIncrement();
            double       dt                        = physics_variable["Time step"].getFinalValue();
            double       pore_nucleation_rate      = 0.0;
            if (dt > 0.0 && dalpha > 0.0)
                pore_nucleation_rate = pore_nucleation_prefactor * dalpha / dt;

            // Resolution rate with Veshchunov-Tarasov thin-layer geometry.
            // See (b) above for why these are hardcoded rather than read from
            // the formation-model parameter vector.
            const double resolution_layer_thickness   = 1.0e-9;  // (m)
            const double resolution_critical_distance = 1.0e-9;  // (m)
            double       b0                           = 2.0e-23 * history_variable["Fission rate"].getFinalValue();
            double       pore_resolution_rate =
                b0 *
                (3.0 * resolution_critical_distance /
                 (3.0 * resolution_critical_distance + sciantix_variable["HBS pore radius"].getFinalValue())) *
                (resolution_layer_thickness /
                 (resolution_layer_thickness + sciantix_variable["HBS pore radius"].getFinalValue()));

            // Sweeping term
            double sweeping_term(0.0);
            if (dt > 0.0)
                sweeping_term = 1. / (1. - sciantix_variable["Restructured volume fraction"].getFinalValue()) *
                                sciantix_variable["Restructured volume fraction"].getIncrement() / dt;
            if (std::isinf(sweeping_term) || std::isnan(sweeping_term))
                sweeping_term = 0.0;

            // from non-restructured grains to non-restructured grain-boundary (bubbles)
            double gas_in_gb_from_grain(0.0);
            double swept         = sweeping_term * dt * sciantix_variable["Xe in grain"].getInitialValue();
            gas_in_gb_from_grain = sciantix_variable["Xe produced"].getIncrement() -
                                   sciantix_variable["Xe in grain"].getIncrement() - swept;

            // from HBS grains to HBS grain-boundary
            double gas_in_pores_from_hbs_grain(0.0);
            gas_in_pores_from_hbs_grain = sciantix_variable["Xe produced in HBS"].getIncrement() -
                                          sciantix_variable["Xe in grain HBS"].getIncrement() + swept;

            double N_pore = sciantix_variable["HBS pore density"].getFinalValue();

            reference = ": Barani et al. (2022) JNM 563, 153627 (linear cluster dynamics); nucleation rate coupled to "
                        "d(alpha_r)/dt from the active formation model (Zullo 2026).";

            double trapping_coeff_HBS =
                4.0 * M_PI * D_gb * R_pore * (1.0 + 1.8 * pow(sciantix_variable["HBS porosity"].getFinalValue(), 1.3));
            double total_trapping_rate_HBS = trapping_coeff_HBS * N_pore;

            double coeff_matrix[25];
            double initial_conditions[5];

            for (int i = 0; i < 25; ++i)
                coeff_matrix[i] = 0.0;

            coeff_matrix[0] = 1.0 + pore_resolution_rate * dt;

            // Row 1: Xe in HBS pores (A)
            coeff_matrix[6] = 1.0 + pore_resolution_rate * dt;
            coeff_matrix[9] = -total_trapping_rate_HBS * dt;

            // Row 2: Xe in HBS pores - variance (B)
            coeff_matrix[12] = 1.0 + pore_resolution_rate * dt;

            // Row 3: Xe at grain boundary (NR)
            coeff_matrix[18] = 1.0 + sweeping_term * dt;

            // Row 4: Xe at grain boundary HBS
            coeff_matrix[21] = -pore_resolution_rate * dt;
            coeff_matrix[23] = -sweeping_term * dt;
            coeff_matrix[24] = 1.0 + total_trapping_rate_HBS * dt;

            initial_conditions[0] = sciantix_variable["HBS pore density"].getInitialValue() + pore_nucleation_rate * dt;

            initial_conditions[1] =
                sciantix_variable["Xe in HBS pores"].getInitialValue() + 2.0 * pore_nucleation_rate * dt;

            initial_conditions[2] = sciantix_variable["Xe in HBS pores - variance"].getInitialValue() +
                                    pow((sciantix_variable["Xe atoms per HBS pore"].getFinalValue() - 2.0), 2.0) *
                                        pore_nucleation_rate * dt;

            initial_conditions[3] = sciantix_variable["Xe at grain boundary"].getInitialValue() + gas_in_gb_from_grain;
            initial_conditions[4] = sciantix_variable["Xe at grain boundary HBS"].getInitialValue() +
                                    gas_in_pores_from_hbs_grain - 2.0 * pore_nucleation_rate * dt;

            solver.Laplace(5, coeff_matrix, initial_conditions);

            sciantix_variable["HBS pore density"].setFinalValue(initial_conditions[0]);
            sciantix_variable["Xe in HBS pores"].setFinalValue(initial_conditions[1]);
            sciantix_variable["Xe in HBS pores - variance"].setFinalValue(initial_conditions[2]);
            sciantix_variable["Xe at grain boundary"].setFinalValue(initial_conditions[3]);
            sciantix_variable["Xe at grain boundary HBS"].setFinalValue(initial_conditions[4]);

            if (sciantix_variable["Xe at grain boundary"].getFinalValue() < 0.0)
                sciantix_variable["Xe at grain boundary"].setFinalValue(0.0);
            if (sciantix_variable["Xe at grain boundary HBS"].getFinalValue() < 0.0)
                sciantix_variable["Xe at grain boundary HBS"].setFinalValue(0.0);
            if (sciantix_variable["Xe in HBS pores"].getFinalValue() < 0.0)
                sciantix_variable["Xe in HBS pores"].setFinalValue(0.0);

            // Xe atoms per HBS pore: n = A / Np
            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore"].setFinalValue(
                    sciantix_variable["Xe in HBS pores"].getFinalValue() /
                    sciantix_variable["HBS pore density"].getFinalValue());

            // HBS pore volume
            double XeHSDiameter =
                4.45e-10 * (0.8542 - 0.03996 * log(history_variable["Temperature"].getFinalValue() / 231.2));
            double gasVolumeInPore = M_PI / 6.0 * pow(XeHSDiameter, 3.0);
            double V_pore_old_step = sciantix_variable["HBS pore volume"].getInitialValue();

            sciantix_variable["HBS pore volume"].setInitialValue(
                sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * gasVolumeInPore +
                sciantix_variable["Vacancies per HBS pore"].getInitialValue() * fuel_.getSchottkyVolume());
            sciantix_variable["HBS pore radius"].setInitialValue(
                0.620350491 * pow(sciantix_variable["HBS pore volume"].getInitialValue(), (1.0 / 3.0)));

            // Vacancy absorption (Speight-Beere), implicit in n_vp, Z, zeta and p_eq.
            // Alpha-weighted tilt correction (Barani 2022, Eq. 7) and saturation
            // factor (phenomenological closure, exponent 2 not calibrated) on D_gb^v
            // only. D_gb^SA in the trapping term is not damped.
            {
                double tilt_factor = sin(angle_deg * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);
                double D_gb_v_eff  = fuel_.getGrainBoundaryVacancyDiffusivity() * tilt_factor * saturation_factor;
                double growth_rate(0.0), equilibrium_term(0.0);
                sciantix_variable["Vacancies per HBS pore"].setFinalValue(ImplicitVacanciesPerPore(
                    sciantix_variable["Vacancies per HBS pore"].getInitialValue(),
                    sciantix_variable["Xe atoms per HBS pore"].getFinalValue(),
                    sciantix_variable["HBS pore density"].getFinalValue(), gasVolumeInPore,
                    fuel_.getSchottkyVolume(), D_gb_v_eff, fuel_.getSurfaceTension(),
                    history_variable["Hydrostatic stress"].getFinalValue() * 1e6,
                    boltzmann_constant * history_variable["Temperature"].getFinalValue(),
                    physics_variable["Time step"].getFinalValue(), growth_rate, equilibrium_term));
                parameter.push_back(growth_rate);
                parameter.push_back(equilibrium_term);
            }

            sciantix_variable["HBS pore volume"].setFinalValue(
                sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * gasVolumeInPore +
                sciantix_variable["Vacancies per HBS pore"].getFinalValue() * fuel_.getSchottkyVolume());

            double V_pore_increment = sciantix_variable["HBS pore volume"].getFinalValue() - V_pore_old_step;

            sciantix_variable["HBS pore radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), 1. / 3.));

            sciantix_variable["HBS porosity"].setFinalValue(sciantix_variable["HBS pore volume"].getFinalValue() *
                                                            sciantix_variable["HBS pore density"].getFinalValue());

            // HBS pore interconnection by impingement
            double limiting_factor = (2.0 - sciantix_variable["HBS porosity"].getFinalValue()) /
                                     (2.0 * pow(1.0 - sciantix_variable["HBS porosity"].getFinalValue(), 3.0));

            double pore_interconnection_rate = 4.0 * limiting_factor;

            sciantix_variable["HBS pore density"].resetValue();
            sciantix_variable["HBS pore density"].setFinalValue(solver.BinaryInteraction(
                sciantix_variable["HBS pore density"].getInitialValue(), pore_interconnection_rate,
                std::max(0.0, V_pore_increment)));  // impingement only on pore growth

            // Conservation (atoms and vacancies)
            if (sciantix_variable["HBS pore density"].getFinalValue())
            {
                sciantix_variable["Xe atoms per HBS pore"].rescaleFinalValue(
                    sciantix_variable["HBS pore density"].getInitialValue() /
                    sciantix_variable["HBS pore density"].getFinalValue());

                sciantix_variable["Vacancies per HBS pore"].rescaleFinalValue(
                    sciantix_variable["HBS pore density"].getInitialValue() /
                    sciantix_variable["HBS pore density"].getFinalValue());

                sciantix_variable["HBS pore volume"].setFinalValue(
                    sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * gasVolumeInPore +
                    sciantix_variable["Vacancies per HBS pore"].getFinalValue() * fuel_.getSchottkyVolume());

                sciantix_variable["HBS pore radius"].setFinalValue(
                    0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), 1. / 3.));
            }

            sciantix_variable["HBS porosity"].setFinalValue(sciantix_variable["HBS pore volume"].getFinalValue() *
                                                            sciantix_variable["HBS pore density"].getFinalValue());

            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore - variance"].setFinalValue(
                    sciantix_variable["Xe in HBS pores - variance"].getFinalValue() /
                    sciantix_variable["HBS pore density"].getFinalValue());

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__,
                                  "iHighBurnupStructurePorosity",
                                  int(input_variable["iHighBurnupStructurePorosity"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    switch (int(input_variable["iHighBurnupStructurePorosity"].getValue()))
    {
        case 0:
            break;

        case 1:
        {
            // empirical porosity evolution
            sciantix_variable["HBS porosity"].setFinalValue(
                solver.Integrator(sciantix_variable["HBS porosity"].getInitialValue(),
                                  model["High-burnup structure porosity"].getParameter().at(0),
                                  sciantix_variable["Burnup"].getIncrement()));

            if (sciantix_variable["HBS porosity"].getFinalValue() > 0.15)
                sciantix_variable["HBS porosity"].setFinalValue(0.15);

            // evolution of pore number density via pore nucleation and re-solution
            if (sciantix_variable["HBS porosity"].getFinalValue())
                sciantix_variable["HBS pore density"].setFinalValue(
                    solver.Decay(sciantix_variable["HBS pore density"].getInitialValue(),
                                 fuel_.getPoreResolutionRate(),
                                 fuel_.getPoreNucleationRate(),
                                 physics_variable["Time step"].getFinalValue()));
            else
                sciantix_variable["HBS pore density"].setFinalValue(0.0);

            // calculation of pore volume based on porosity and pore number density
            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["HBS pore volume"].setFinalValue(
                    sciantix_variable["HBS porosity"].getFinalValue() /
                    sciantix_variable["HBS pore density"].getFinalValue());

            sciantix_variable["HBS pore radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), (1.0 / 3.0)));

            // update of number density of HBS pores: interconnection by impingement
            double limiting_factor = (2.0 - sciantix_variable["HBS porosity"].getFinalValue()) /
                                     (2.0 * pow(1.0 - sciantix_variable["HBS porosity"].getFinalValue(), 3.0));

            double pore_interconnection_rate = 4.0 * limiting_factor;
            sciantix_variable["HBS pore density"].setFinalValue(
                solver.BinaryInteraction(sciantix_variable["HBS pore density"].getFinalValue(),
                                         pore_interconnection_rate,
                                         sciantix_variable["HBS pore volume"].getIncrement()));

            // update of pore volume and pore radius after interconnection by impingement
            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["HBS pore volume"].setFinalValue(
                    sciantix_variable["HBS porosity"].getFinalValue() /
                    sciantix_variable["HBS pore density"].getFinalValue());

            sciantix_variable["HBS pore radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), (1.0 / 3.0)));

            // average (at/m^3) of gas atoms in HBS pores
            sciantix_variable["Xe in HBS pores"].setFinalValue(solver.Integrator(
                sciantix_variable["Xe in HBS pores"].getInitialValue(),
                2.0 * fuel_.getPoreNucleationRate() + sciantix_variable["HBS pore density"].getFinalValue() *
                                                          (fuel_.getPoreTrappingRate() - fuel_.getPoreResolutionRate()),
                physics_variable["Time step"].getFinalValue()));

            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore"].setFinalValue(
                    sciantix_variable["Xe in HBS pores"].getFinalValue() /
                    sciantix_variable["HBS pore density"].getFinalValue());

            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore - variance"].setFinalValue(
                    sciantix_variable["Xe in HBS pores - variance"].getFinalValue() /
                    sciantix_variable["HBS pore density"].getFinalValue());
            break;
        }

        case 2:
            break;

        case 3:
            break;

        default:
            ErrorMessages::Switch(__FILE__,
                                  "iHighBurnupStructurePorosity",
                                  int(input_variable["iHighBurnupStructurePorosity"].getValue()));
            break;
    }
}