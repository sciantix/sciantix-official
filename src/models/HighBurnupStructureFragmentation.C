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
//  Version: 2.2.1                                                                  //
//  Year: 2026                                                                      //
//  Authors: E. Cappellari                                                          //
//  author: E.Cappellari, POLIMI-CEA 2026                                           //                                  //
//////////////////////////////////////////////////////////////////////////////////////

// Fine fragmentation of the high-burnup structure (HBS), iHighBurnupStructureFragmentation = 1, 2, 3, 4
// (1 Jernkvist, 2 Kulacsy, 3 LEFM, 4 proposed model: see utilities/HBFF/README.md).
//
// The model reads the HBS pore state of HighBurnupStructurePorosity (option 2 or 3), T and the hydrostatic pressure
// of input_history.txt (P_h = -Hydrostatic stress), and returns per step
//   D  fraction of the HBS phase whose pores are broken (pore-number weighted, drives the fragment size),
//   G  fraction of the pore gas released by the broken pores (gas weighted, drives the release),
//   d  mean fragment size.
//
// Tags: [P] paper, [R] reduction into the SCIANTIX state, [U] application to the HBS material,
//       [N] numerics, [?] unknown parameter, [E] deviation from or error in a source.
//
// References
//   J19  L.O. Jernkvist, Modelling of fine fragmentation and fission gas release of UO2 fuel in accident conditions,
//        EPJ Nucl. Sci. Technol. 5 (2019) 11, doi:10.1051/epjn/2019030
//   J20  L.O. Jernkvist, A review of analytical criteria for fission gas induced fragmentation of oxide fuel in accident
//        conditions, Prog. Nucl. Energy 119 (2020) 103188, doi:10.1016/j.pnucene.2019.103188
//   K15  K. Kulacsy, Mechanistic model for the fragmentation of the high-burnup structure during LOCA,
//        J. Nucl. Mater. 466 (2015) 409, doi:10.1016/j.jnucmat.2015.08.015
//   OHP  OperaHPC Deliverable D5.2 (2026), section 2.2 (NFIR annealing test, RVE with HBS pores)
//   KH20 G. Khvostov, Analytical criteria for fuel fragmentation and burst FGR during a LOCA, Nucl. Eng. Technol. 52
//        (2020) 2402 (fragment size from the specific surface of the cracks)
//   IR57 G.R. Irwin, J. Appl. Mech. 24 (1957) 361 (penny-shaped crack under internal pressure), as in J20 Eq. 8
//
// --------------------------------------------------------------------------------------------------
// Limitations
//   L1 [R]  the pressure is uniform over the pore classes (option 3); K15, J19 and option 4 use dP ~ 1/R.
//   L2 [R]  the venting scales the gas by (1 - dG) and its variance by the square, as if every pore lost the same
//           fraction of its gas.
//   L3 [E]  options 1, 3 use the hard-sphere EoS of the porosity model; option 2 uses van der Waals in place of Ronchi.
//           Ronchi (1981) gives the EoS as a table (T, V) -> P to be interpolated (Kulacsy 3.1.2), not a closed
//           form: pending digitisation of that table, van der Waals stays as the documented deviation.
//   L4 [?]  the fragments are equiaxed; HBS fragments are acicular (NEA/CSNI/R(2016)16, 3.1.3).
//   L5 [?]  no lower bound on the fragment size (a 40 um HBS fragment does not fragment further at 1300 C).
//   L6 [N]  D and G are cumulative maxima.
//   L7 [?]  option 2 starts the transient at the first step in which the fission rate falls, and keeps the state of the
//           step before as the reference (T_ss, P_h,ss); if the fission rate falls below 1 % of that value and later returns above
//           50 % of it, the power-off was an outage and the reference state is tracked again. A power step-down that never
//           reaches 1 % is read as the transient start for the rest of the history.
//   L8 [?]  the porosity model relaxes the vented pores (capillary shrinkage; with partial venting the survivors lose gas).
//   L9 [?]  the criteria act only for a mean pore radius of at least 0.1 um and a porosity below 0.35.
//   L10 [?] the free parameter of option 3 (G_hbs) is fitted on one dataset (NFIR Kr-85 release, OHP Fig. 9, a slow
//           anneal), so the threshold at which it ruptures under a P_h transient is unvalidated. Options 1, 2 and 4 are not fitted.
//   L11 [R] the pore state that SCIANTIX builds (iHighBurnupStructureFormation = 4, porosity 3: about 10.5 % porosity, 0.35 um
//           radius at the end of the NFIR base irradiation) differs from the measurement (11 %, 0.88 um): the criteria act on the
//           modelled state, not on the measured one.
//   L12 [?] option 2 uses only xi, T, P_h of the SCIANTIX state: its classes are the fixed log-normal of K15.
//   L13 [?] the confining stress of the base irradiation is an input of the history (no PCMI model) and changes the pore state
//           through the equilibrium pressure of the porosity model.
//   L14 [?] the IFA-650 cases are one rim point with a transient taken from another test: no radial or axial structure, no
//           cladding strain, no real T and P_h traces.
//   L15 [?] no heating-rate dependence except the relaxation time of option 4; only the HBS is fragmented.
//   L16 [?] option 4: the pressure shape c_dp / R is the steady-state limit of J19, not a measured distribution; kappa is
//           recomputed each step, so in a heat-up at fixed gas the gas is slightly redistributed among the classes; the
//           relaxation time tau = 2 s is borrowed from the J19 interconnection and microcracking modes (J19 rupture is instantaneous);
//           the pores do not interact after a break.

#include "Simulation.h"

#include <algorithm>
#include <cmath>
#include <vector>

void Simulation::HighBurnupStructureFragmentation()
{
    if (!int(input_variable["iHighBurnupStructureFragmentation"].getValue()))
        return;

    if (int(input_variable["iHighBurnupStructurePorosity"].getValue()) != 2 &&
        int(input_variable["iHighBurnupStructurePorosity"].getValue()) != 3)
        ErrorMessages::Fatal(__FILE__,
                             "iHighBurnupStructureFragmentation reads the pore state of "
                             "iHighBurnupStructurePorosity = 2 or 3; other values do not compute it.");

    // Model declaration
    Model model_;
    model_.setName("High-burnup structure fragmentation");

    Matrix fuel_(matrices["UO2HBS"]);

    // The gas of the pores that already vented was removed from the state, [R] L2
    const double retention_old = sciantix_variable["HBS gas retention fraction"].getInitialValue() > 0.0
                                     ? sciantix_variable["HBS gas retention fraction"].getInitialValue()
                                     : 1.0;

    const double T    = history_variable["Temperature"].getFinalValue();
    const double Ph   = -history_variable["Hydrostatic stress"].getFinalValue() * 1.0e6;  // (Pa)
    const double xi   = sciantix_variable["HBS porosity"].getFinalValue();
    const double R    = sciantix_variable["HBS pore radius"].getFinalValue();
    const double V    = sciantix_variable["HBS pore volume"].getFinalValue();
    const double n    = sciantix_variable["Xe atoms per HBS pore"].getFinalValue() / retention_old;
    const double varn = sciantix_variable["Xe atoms per HBS pore - variance"].getFinalValue() / (retention_old * retention_old);

    // Pore pressure [R], hard-sphere (Carnahan-Starling) EoS of HighBurnupStructurePorosity.C:
    //  p_g = n k_B T Z(eta) / V_p,   Z = (1 + eta + eta^2 - eta^3) / (1 - eta)^3,   eta = min(n v_Xe(T) / V_p, 0.65)
    //  v_Xe = (pi/6) d_Xe^3,   d_Xe = 4.45e-10 [0.8542 - 0.03996 ln(T / 231.2)] m
    //  n = Xe atoms per pore, V_p = pore volume.
    const double xe_diameter = 4.45e-10 * (0.8542 - 0.03996 * std::log(T / 231.2));
    const double xe_volume   = M_PI / 6.0 * std::pow(xe_diameter, 3.0);
    const double eta_cap     = 0.65;  // cap of the hard-sphere packing fraction
    auto hard_sphere_Z = [](double eta) { return (1.0 + eta + eta * eta - eta * eta * eta) / std::pow(1.0 - eta, 3.0); };
    auto pore_pressure = [&](double atoms)
    {
        if (V <= 0.0 || atoms <= 0.0)
            return 0.0;
        const double eta = std::min(atoms * xe_volume / V, eta_cap);
        return atoms * boltzmann_constant * T * hard_sphere_Z(eta) / V;
    };
    const double pg = pore_pressure(n);

    // last-cycle reference state of K15: it stops updating at the transient start [?] L7
    double T_ss      = sciantix_variable["HBFF reference temperature"].getInitialValue();
    double Ph_ss      = sciantix_variable["HBFF reference hydrostatic pressure"].getInitialValue();
    // HBFF transient flag: 0 = the reference state is tracked; F_ref > 0 = transient since the step in which the fission
    // rate fell from F_ref; -F_ref = the fission rate is also below 1 % of F_ref (power off). If the power then returns above
    // 50 % of F_ref the power-off was an outage, not the transient, and the reference state is tracked again. The fission
    // rate carries a relative noise of about 1e-9 where the history changes T or P_h at constant power: the 1 % and 50 %
    // thresholds are not sensitive to it, the start test (any fall) is.
    double transient = sciantix_variable["HBFF transient flag"].getInitialValue();
    if (int(input_variable["iHighBurnupStructureFragmentation"].getValue()) == 2)
    {
        const double fission_rate     = history_variable["Fission rate"].getFinalValue();
        const double fission_rate_old = history_variable["Fission rate"].getInitialValue();

        if (transient > 0.0 && fission_rate < 0.01 * transient)
            transient = -transient;
        else if (transient < 0.0 && fission_rate >= -0.5 * transient)
            transient = 0.0;

        if (transient == 0.0)
        {
            const bool start = fission_rate < fission_rate_old;
            // first step in which the fission rate falls: the reference state is the last one at full power, whether the
            // history switches the fission rate off in one step or ramps it down together with the temperature
            if (start)
                transient = fission_rate_old;
            if (!start || T_ss == 0.0)
            {
                T_ss  = T;
                Ph_ss = Ph;
            }
        }
    }
    sciantix_variable["HBFF reference temperature"].setFinalValue(T_ss);
    sciantix_variable["HBFF reference hydrostatic pressure"].setFinalValue(Ph_ss);
    sciantix_variable["HBFF transient flag"].setFinalValue(transient);

    // [?] validity window of the criteria
    const double r_min_eval  = 1.0e-7;  // (m) K15 covers 0.1 to 2 um; nanometric nuclei are not the micron pores
    const double xi_max_eval = 0.35;    // (/) MATPRO strength vanishes at xi = 0.38; HBS porosity saturates below 0.3 (J19)
    const bool   state_valid = (R >= r_min_eval && xi > 0.0 && xi < xi_max_eval);

    // Parameter definition: each option resolves the instantaneous criterion into (D*, G*), the pore-number and
    // gas-weighted fractions broken this step (0 outside the validity window, or before option 2's transient starts).
    double      d_star = 0.0, g_star = 0.0;
    std::string reference;

    switch (int(input_variable["iHighBurnupStructureFragmentation"].getValue()))
    {
        case 1:
        {
            // Option 1: Jernkvist (2019) [P]
            // J19 Eq. 22 = J20 Eq. 7 with phi_2 -> phi_3.
            //
            // Force balance across the plane (Olander 1997, J20 Eq. 7) with the areal fraction replaced by the porosity
            //   xi and the far-field stress by -P_h (J19 Eq. 22):
            //     P_cr = P_s + [ sigma_hbs^cr (1 - xi) + P_h ] / xi,      sigma_hbs^cr = 21 MPa (J19 Table 4).
            //
            // The rupture releases the whole population.
            // Rupture when p_g >= P_cr on the mean pore: D* = G* = 1.
            reference = ": Jernkvist (2019), EPJ Nucl. Sci. Technol. 5, 11, Eq. 22";

            if (state_valid)
            {
                // [P] J19 Table 4
                const double sigma_hbs_cr = 21.0e6;  // (Pa)
                const double p_cr = 2.0 * fuel_.getSurfaceTension() / R + (sigma_hbs_cr * (1.0 - xi) + Ph) / xi;

                if (pg >= p_cr)
                {
                    d_star = 1.0;
                    g_star = 1.0;
                }
            }
            break;
        }

        case 2:
        {
            // Option 2: Kulacsy (2015) [P]
            //  Before the transient every class of radius r is at the dislocation punching pressure (K15 Eq. 3, 12-14):
            //     p_0(r) = P_h,ss + 2 gamma_ss / r + G b / r
            //  T_ss and P_h,ss are the last-cycle state; the gas per class n_i follows from p_0 with the EoS at T_ss.
            //  Stress and strength (K15 Eq. 7, 11, 15):
            //     sigma_t = p_i - (3/2 P_h + 2 gamma(T, xi) / r),   gamma(T, xi) = 0.41 (0.85 - 1.4e-4 T) (1 - xi)^4.025 N/m
            //     sigma_f(T, xi) = 170 MPa exp(-191.34 / min(T, 1000)) sqrt(1 - 2.62 xi)                    (MATPRO FFRACS)
            //  Classes are log-normal with median exp(-0.5) um and sigma_lnr = 0.356 (K15 Table 2). 
            //  The classes below the smallest stable pore do not exist. With w_i the renormalised weights:
            //     D* = sum_i w_i [sigma_t,i > sigma_f],      G* = sum_i w_i n_i [sigma_t,i > sigma_f] / sum_i w_i n_i.
            reference = ": Kulacsy (2015), J. Nucl. Mater. 466, 409";

            if (state_valid && transient != 0.0)
            {
                const double burgers = 0.39e-9;  // (m) K15, 3.2
                const double kul_m   = -0.5;     // K15 Table 2, r in um
                const double kul_s   = 0.356;    // K15 Table 2

                // [R] [N] radii (m) and number weights of the log-normal classes: 81 nodes at ln R = ln R_mean +/- 4 sigma
                const int    n_classes  = 81;
                const double class_span = 4.0;
                const double median     = std::exp(kul_m) * 1.0e-6;  // [P] r in um

                std::vector<double> radii, w;
                double               w_norm = 0.0;
                for (int i = 0; i < n_classes; ++i)
                {
                    const double x = -class_span + (2.0 * class_span) * i / (n_classes - 1);
                    radii.push_back(median * std::exp(kul_s * x));
                    w.push_back(std::exp(-0.5 * x * x));
                    w_norm += w.back();
                }
                for (double& weight : w)
                    weight /= w_norm;

                // base-irradiation state, no porosity dependence [P] K15 3.1.1.
                const double gamma_ss = 0.41 * (0.85 - 1.4e-4 * T_ss);
                const double e_ss     = 2.334e11 * (1.0 - 1.0915e-4 * T_ss);
                const double g_ss     = e_ss / (2.0 * (1.0 + fuel_.getPoissonRatio()));

                // [P] K15 Eq. 15, T capped at 1000 K (MATPRO)
                const double kulacsySigmaF_ss = 170.0e6 * std::exp(-191.34 / std::min(T_ss, 1000.0)) *
                                                std::sqrt(std::max(1.0 - 2.62 * xi, 0.0));

                // smallest stable pore: sigma_t = Gb/r - Ph/2 = sigma_f at T_ss [P] K15 4.1.1, Eq. 8
                const double r_min = g_ss * burgers / (kulacsySigmaF_ss + 0.5 * Ph_ss);

                double w_sum = 0.0;
                for (size_t i = 0; i < radii.size(); ++i)
                    if (radii[i] >= r_min)
                        w_sum += w[i];

                if (w_sum > 0.0)
                {
                    double w_broken = 0.0, gas_sum = 0.0, gas_broken = 0.0;
                    for (size_t i = 0; i < radii.size(); ++i)
                    {
                        if (radii[i] < r_min)
                            continue;
                        const double weight = w[i] / w_sum;
                        const double Vc     = 4.0 / 3.0 * M_PI * std::pow(radii[i], 3.0);
                        const double p0     = Ph_ss + 2.0 * gamma_ss / radii[i] + g_ss * burgers / radii[i];  // [P] K15 Eq. 3

                        // During the transient the pores are rigid and p_i(T) follows the EoS. 
                        // Van der Waals replaces the Ronchi EoS of K15 [E]:
                        //     p = n k_B T / (V - n b_w) - a_w n^2 / V^2
                        // K15, 3.1.2: "The gas content of the pores is calculated using Ronchi's equation of state for xenon [31]", given as a table (T, V) -> P to be interpolated; pending digitisation of that table, van der Waals implementation is temporary.
                        const double vdw_a = 1.17e-48;  // (J m3) per atom pair: a = 4.25 L^2 bar/mol2
                        const double vdw_b = 8.49e-29;  // (m3) per atom: b = 0.0511 L/mol

                        // [N] inverse of the van der Waals pressure at fixed V, T_ss, by bisection
                        double lo = 0.0, hi = 0.99 * Vc / vdw_b;
                        for (int it = 0; it < 100; ++it)
                        {
                            const double mid   = 0.5 * (lo + hi);
                            const double p_mid = (mid > 0.0)
                                ? mid * boltzmann_constant * T_ss / (Vc - mid * vdw_b) - vdw_a * mid * mid / (Vc * Vc)
                                : 0.0;
                            if (p_mid < p0)
                                lo = mid;
                            else
                                hi = mid;
                        }
                        const double n_i = 0.5 * (lo + hi);
                        const double p_i = (n_i > 0.0)
                            ? n_i * boltzmann_constant * T / (Vc - n_i * vdw_b) - vdw_a * n_i * n_i / (Vc * Vc)
                            : 0.0;

                        // [P] K15 Eq. 11 (N/m)
                        const double kulacsyGamma = 0.41 * (0.85 - 1.4e-4 * T) * std::pow(1.0 - xi, 4.025);
                        // K15 Eq. 7, 1.0 factor on the net pressure; J20 gives 1/2 for a spherical pore [E]
                        // J20: "we note that Kulacsy used the rupture criterion in Eq. (5) in her recent study on rim zone fragmentation during LOCA (Kulacsy, 2015). She assumed that the overpressurized rim zone pores were spherical, but obviously used F1 = 1 instead of the correct value F1 = 1_2."
                        const double sigma_t = p_i - 2.0 * kulacsyGamma / radii[i] - 1.5 * Ph;
                        gas_sum += weight * n_i;

                        // [P] K15 Eq. 15, T capped at 1000 K (MATPRO)
                        const double kulacsySigmaF = 170.0e6 * std::exp(-191.34 / std::min(T, 1000.0)) *
                                                     std::sqrt(std::max(1.0 - 2.62 * xi, 0.0));
                        if (sigma_t > kulacsySigmaF)
                        {
                            w_broken += weight;
                            gas_broken += weight * n_i;
                        }
                    }
                    d_star = w_broken;
                    g_star = gas_broken / gas_sum;
                }
            }
            break;
        }

        case 3:
        {
            // Option 3: LEFM [P] penny crack, [R] capillarity, [?] interaction factor
            // Penny-shaped crack of radius R under the pore pressure (IR57, J20 Eq. 8):
            //     P_cr(R) = P_h + 2 gamma / R + (1 / (2 Y)) sqrt( pi E G_hbs / ((1 - nu^2) R) )
            // IR57/J20 Eq. 8 is derived for a flat penny-shaped crack (R1, R2 -> infinity), so it has no capillary term. A real pore is closer to a filled cavity than to a flat crack, so the capillary pressure 2 gamma / R is added;
            //  P_cr decreases with R: the pores with R >= R_c, P_cr(R_c) = p_g, break (bisection on R_c).
            // With z = ln(R_c / R_mean) / sigma_lnR:
            //   D* = (1/2) erfc(z / sqrt(2))
            //   G* = (1/2) erfc((z - 3 sigma_lnR) / sqrt(2))
            //   G* is the third moment of the log-normal: at uniform pressure the gas is proportional to R^3 [R].
            reference = ": LEFM, Jernkvist (2020), Prog. Nucl. Energy 119, 103188, Eq. 8";

            if (state_valid && pg > 0.0)
            {
                // G_hbs is fitted on the NFIR Kr-85 release (OHP Fig. 9, 29.8 % at 1200 C) [?]; with the bulk 2 J/m2 of SetMatrix.C the criterion gives hundreds of MPa and cannot break the pores (J20: the local G is below the bulk G).
                const double g_hbs = 0.055;  // (J/m2)

                // The interaction factor Y (J20 gives F3 in [2, 2.61] for a similar geometry dependence in the Harwell criterion, Eq. 9) is left at 1, unfitted [?]:
                const double y_factor = 1.0;

                // [R] UO2HBS elastic modulus of SetMatrix.C (Pa), recomputed here from the current xi, T, Bu: matrices["UO2HBS"].getElasticModulus() would stay frozen at the t = 0 state for the whole run [N].
                const double burnup = sciantix_variable["Burnup"].getFinalValue();
                const double E      = 2.237e11 * (1.0 - 2.6 * xi) * (1.0 - 1.394e-4 * (T - 273.0 - 20.0)) *
                                       (1.0 - 0.1506 * (1.0 - std::exp(-0.035 * burnup)));

                const double c      = 0.5 * std::sqrt(M_PI * E * g_hbs / (1.0 - fuel_.getPoissonRatio() * fuel_.getPoissonRatio())) / y_factor;
                const double target = pg - Ph;
                if (target > 0.0)
                {
                    double lo = 1.0e-12, hi = 1.0e-3;
                    if (c / std::sqrt(hi) + 2.0 * fuel_.getSurfaceTension() / hi < target)
                    {
                        for (int it = 0; it < 100; ++it)  // [N] bisection on the decreasing function c/sqrt(R) + 2 gamma / R
                        {
                            const double mid = std::sqrt(lo * hi);
                            if (c / std::sqrt(mid) + 2.0 * fuel_.getSurfaceTension() / mid >= target)
                                lo = mid;
                            else
                                hi = mid;
                        }
                        const double r_c = std::sqrt(lo * hi);

                        //   Pore classes [R] [N]. SCIANTIX keeps two moments of n (mean n and Var(n)); the radius is log-normal, R ~ n^(1/3):
                        //     CV_R = sqrt(Var(n)) / (3 n),   sigma_lnR = sqrt(ln(1 + CV_R^2)),
                        double sigma_lnR = 0.0;
                        if (n > 0.0 && varn > 0.0)
                        {
                            const double cv_r = std::sqrt(varn) / n / 3.0;
                            sigma_lnR         = std::sqrt(std::log(1.0 + cv_r * cv_r));
                        }

                        if (sigma_lnR <= 0.0)
                        {
                            if (R >= r_c)
                            {
                                d_star = 1.0;
                                g_star = 1.0;
                            }
                        }
                        else
                        {
                            const double z = (std::log(r_c) - std::log(R)) / sigma_lnR;
                            d_star         = 0.5 * std::erfc(z / std::sqrt(2.0));                       // number fraction above R_c
                            g_star         = 0.5 * std::erfc((z - 3.0 * sigma_lnR) / std::sqrt(2.0));  // third moment of the log-normal
                        }
                    }
                }
            }
            break;
        }

        case 4:
        {
            // Option 4: Jernkvist (2019) force balance per pore class, pressure from the SCIANTIX gas state [P] [R], progressive rupture [?]
            //  Classes: R_i log-normal around the mean pore, sigma_lnR from CV_n (as option 3), weights w_i, volumes V_i.
            //  Pressure per class: p_i = P_h + 2 gamma / R_i + kappa c_dp / R_i, with c_dp = 55 N/m (J19 Eq. 21, Table 4: the
            //  steady-state overpressure of a punching-limited pore is c_dp / R, the inverse dependence on the radius is that of J19
            //  and K15). kappa is the loading relative to that limit: it is found by bisection each step so that the gas of
            //  the classes, n_i = EoS^-1(p_i; V_i, T), gives the SCIANTIX mean, sum_i w_i n_i = n [R] (mass-consistent).
            //  Rupture of a class, J19 Eq. 22 on the class radius: p_i >= P_cr,i = 2 gamma / R_i + [sigma_hbs^cr (1 - xi) + P_h] / xi.
            //  Interconnected pores, xi >= 0.29 (J19): the whole population is open.
            //  D* = sum_i w_i [broken],   G* = sum_i w_i n_i [broken] / sum_i w_i n_i.
            //  Progressive rupture [?]: the instantaneous (D*, G*) are approached with a relaxation time tau (J19 Table 3 gives
            //  tau = 2 s to the interconnection and microcracking modes; its rupture mode is instantaneous). D <- D + (D* - D)(1 - exp(-dt / tau)).
            reference = ": Jernkvist (2019), EPJ Nucl. Sci. Technol. 5, 11, Eq. 21-22; progressive rupture as GrainBoundaryMicroCracking";

            if (state_valid && pg > 0.0)
            {
                const double sigma_hbs_cr = 21.0e6;  // (Pa) [P] J19 Table 4
                const double c_dp         = 55.0;    // (N/m) [P] J19 Table 4
                const double xi_perc      = 0.29;    // (/) [P] J19, percolation of the HBS pores
                const double tau_rupture  = 2.0;     // (s) [?]
                const double gamma        = fuel_.getSurfaceTension();

                if (xi >= xi_perc)
                {
                    d_star = 1.0;
                    g_star = 1.0;
                }
                else
                {
                    // [R] [N] log-normal classes, 81 nodes at ln R = ln R_mean +/- 4 sigma_lnR; one class if the variance is null
                    double sigma_lnR = 0.0;
                    if (n > 0.0 && varn > 0.0)
                    {
                        const double cv_r = std::sqrt(varn) / n / 3.0;
                        sigma_lnR         = std::sqrt(std::log(1.0 + cv_r * cv_r));
                    }
                    const int    n_classes  = (sigma_lnR > 0.0) ? 81 : 1;
                    const double class_span = 4.0;

                    std::vector<double> radii, volumes, w;
                    double               w_norm = 0.0;
                    for (int i = 0; i < n_classes; ++i)
                    {
                        const double x = (n_classes > 1) ? -class_span + (2.0 * class_span) * i / (n_classes - 1) : 0.0;
                        radii.push_back(R * std::exp(sigma_lnR * x));
                        volumes.push_back(4.0 / 3.0 * M_PI * std::pow(radii.back(), 3.0));
                        w.push_back(std::exp(-0.5 * x * x));
                        w_norm += w.back();
                    }
                    for (double& weight : w)
                        weight /= w_norm;

                    // [N] inverse of the hard-sphere EoS at fixed volume and T: eta Z(eta) = p v_Xe / (k_B T), Newton from the right
                    // (eta Z(eta) is convex and increasing; d(eta Z)/d eta = (1 + 4 eta + 4 eta^2 - 4 eta^3 + eta^4) / (1 - eta)^4).
                    // Above the cap the pressure is linear in the gas, Z = Z(0.65).
                    auto atoms_at_pressure = [&](double p, double volume)
                    {
                        if (p <= 0.0)
                            return 0.0;
                        const double y     = p * xe_volume / (boltzmann_constant * T);
                        const double z_cap = hard_sphere_Z(eta_cap);
                        double       eta   = y;
                        if (y >= eta_cap * z_cap)
                            eta = y / z_cap;
                        else
                            for (int it = 0; it < 50; ++it)
                            {
                                const double f  = eta * hard_sphere_Z(eta) - y;
                                const double df = (1.0 + 4.0 * eta + 4.0 * eta * eta - 4.0 * std::pow(eta, 3.0) + std::pow(eta, 4.0)) /
                                                  std::pow(1.0 - eta, 4.0);
                                const double step = f / df;
                                eta -= step;
                                if (std::fabs(step) < 1.0e-14 * eta)
                                    break;
                            }
                        return eta * volume / xe_volume;
                    };
                    auto class_pressure = [&](size_t i, double kappa)
                    { return Ph + 2.0 * gamma / radii[i] + kappa * c_dp / radii[i]; };
                    auto mean_atoms = [&](double kappa)
                    {
                        double sum = 0.0;
                        for (size_t i = 0; i < radii.size(); ++i)
                            sum += w[i] * atoms_at_pressure(class_pressure(i, kappa), volumes[i]);
                        return sum;
                    };

                    // [N] kappa by bisection: lower bound where every class is above zero pressure, upper bound by doubling
                    double kappa_lo = -(Ph + 2.0 * gamma / radii[0]) * radii[0] / c_dp;
                    for (size_t i = 1; i < radii.size(); ++i)
                        kappa_lo = std::max(kappa_lo, -(Ph + 2.0 * gamma / radii[i]) * radii[i] / c_dp);
                    kappa_lo *= 0.999;
                    double kappa_hi = 1.0;
                    for (int it = 0; it < 80 && mean_atoms(kappa_hi) < n; ++it)
                        kappa_hi *= 2.0;
                    for (int it = 0; it < 60; ++it)
                    {
                        const double mid = 0.5 * (kappa_lo + kappa_hi);
                        if (mean_atoms(mid) < n)
                            kappa_lo = mid;
                        else
                            kappa_hi = mid;
                    }
                    const double kappa = 0.5 * (kappa_lo + kappa_hi);

                    double gas_sum = 0.0, gas_broken = 0.0, w_broken = 0.0;
                    for (size_t i = 0; i < radii.size(); ++i)
                    {
                        const double p_i  = class_pressure(i, kappa);
                        const double n_i  = atoms_at_pressure(p_i, volumes[i]);
                        const double p_cr = 2.0 * gamma / radii[i] + (sigma_hbs_cr * (1.0 - xi) + Ph) / xi;  // [P] J19 Eq. 22
                        gas_sum += w[i] * n_i;
                        if (p_i >= p_cr)
                        {
                            w_broken += w[i];
                            gas_broken += w[i] * n_i;
                        }
                    }
                    d_star = w_broken;
                    g_star = (gas_sum > 0.0) ? gas_broken / gas_sum : 0.0;
                }

                // Progressive rupture [?]: relaxation towards (D*, G*) from the previous (diluted) values
                const double alpha_old_ = sciantix_variable["Restructured volume fraction"].getInitialValue();
                const double alpha_new_ = sciantix_variable["Restructured volume fraction"].getFinalValue();
                const double dilution   = (alpha_new_ > alpha_old_ && alpha_old_ > 0.0) ? alpha_old_ / alpha_new_ : 1.0;
                const double relax      = 1.0 - std::exp(-physics_variable["Time step"].getFinalValue() / tau_rupture);
                const double d_old      = sciantix_variable["HBS fragmented fraction"].getInitialValue() * dilution;
                const double g_old      = sciantix_variable["HBS burst release fraction"].getInitialValue() * dilution;
                d_star                  = d_old + std::max(d_star - d_old, 0.0) * relax;
                g_star                  = g_old + std::max(g_star - g_old, 0.0) * relax;
            }
            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iHighBurnupStructureFragmentation", int(input_variable["iHighBurnupStructureFragmentation"].getValue()));
            break;
    }

    std::vector<double> parameter;
    parameter.push_back(d_star);
    parameter.push_back(g_star);
    model_.setParameter(parameter);
    model_.setRef(reference);
    model.push(model_);

    // Model resolution
    //   Broken fractions [N] [U]. With alpha the restructured volume fraction and (D*, G*) the instantaneous result of the criterion:
    //     if alpha > alpha_old:  D <- D alpha_old / alpha,  G <- G alpha_old / alpha   (new restructured material is intact)
    //     D <- max(D, D*),  G <- max(G, G*)  (a broken pore never heals)
    double D = sciantix_variable["HBS fragmented fraction"].getInitialValue();
    double G = sciantix_variable["HBS burst release fraction"].getInitialValue();

    const double alpha_old = sciantix_variable["Restructured volume fraction"].getInitialValue();
    const double alpha     = sciantix_variable["Restructured volume fraction"].getFinalValue();
    if (alpha > alpha_old && alpha_old > 0.0)
    {
        D *= alpha_old / alpha;
        G *= alpha_old / alpha;
    }

    D                   = std::max(D, model["High-burnup structure fragmentation"].getParameter().at(0));
    const double G_new = std::max(G, model["High-burnup structure fragmentation"].getParameter().at(1));

    //   Venting [R]. A broken pore vents its gas and stays as a void. When G grows from G_old to G_new, with
    //     f = max[(1 - G_new) / (1 - G_old), 1e-12 / S]:
    //     A -> f A,  B -> f^2 B,  n -> f n,  Var(n) -> f^2 Var(n),  S -> f S,
    //   A = "Xe in HBS pores", B its variance, S = "HBS gas retention fraction". The gas leaves through "Xe released" by the
    //   mass balance of GasRelease.C. The criteria are evaluated on the state before the venting: n / S and Var(n) / S^2.
    // the gas of the population and its variance scale with the survivors [R] L2
    double retention = retention_old;
    if (G_new > G)
    {
        double factor = (G < 1.0) ? (1.0 - G_new) / (1.0 - G) : 1.0;
        // [N] the gas retention of the population never falls below this value
        const double retention_floor = 1.0e-12;
        factor    = std::min(1.0, std::max(factor, retention_floor / retention_old));
        retention *= factor;
        sciantix_variable["Xe in HBS pores"].rescaleFinalValue(factor);
        sciantix_variable["Xe in HBS pores - variance"].rescaleFinalValue(factor * factor);
        sciantix_variable["Xe atoms per HBS pore"].rescaleFinalValue(factor);
        sciantix_variable["Xe atoms per HBS pore - variance"].rescaleFinalValue(factor * factor);
    }
    G = G_new;

    sciantix_variable["HBS fragmented fraction"].setFinalValue(D);
    sciantix_variable["HBS burst release fraction"].setFinalValue(G);
    sciantix_variable["HBS gas retention fraction"].setFinalValue(retention);
    // the output is the pressure of the gas the pores hold after the venting; the criteria above used pg, before it
    sciantix_variable["HBS pore pressure"].setFinalValue(pore_pressure(sciantix_variable["Xe atoms per HBS pore"].getFinalValue()));

    // Fragment size [R] [U]
    // Cubic fragments bounded by the cracked faces of the Wigner-Seitz cells of the pores (specific surface of the cracks, as in KH20 Eq. 2-4 for the vented pores of the grain face).
    const double n_pores = sciantix_variable["HBS pore density"].getFinalValue();
    double       d       = 0.0;
    if (D > 1.0e-9 && n_pores > 0.0)
    {
        // R_WS = (3 / (4 pi N_p))^(1/3)
        const double r_ws = std::pow(3.0 / (4.0 * M_PI * n_pores), 1.0 / 3.0);

        // A_f = 2 pi R_WS^2 [?] is the cracked area per broken pore: half of the surface of its Wigner-Seitz sphere, each face being shared with the neighbouring cell. D -> 1 gives d = 2 R_WS, the cell diameter.
        // d_max = 500 um [U] (above, pellet cracks, NEA/CSNI/R(2016)16 section 3); d = 0 if D < 1e-9.
        const double a_f   = 2.0 * M_PI * r_ws * r_ws;  // (m2)
        const double d_max = 5.0e-4;                    // (m)

        // S_v = N_p D A_f,   d = min(3 / S_v, d_max)
        d = std::min(3.0 / (n_pores * D * a_f), d_max);
    }
    sciantix_variable["HBS fragment size"].setFinalValue(d);
}
