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
//  Authors: V. Bellani, E. Cappellari.                                             //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"
#include <algorithm>

/**
 *
 * // [FMP, Nov. 2026]
 *
 * Five metal particles (FMP): nucleation (CNT), growth and irradiation-induced re-solution of the Mo-Tc-Ru-Rh-Pd
 * precipitates in three zones.
 */
void Simulation::FiveMetalParticles()
{
    if ((int)input_variable["iFMP"].getValue() != 1)
        return;

    Model model_;
    model_.setName("Five metal particles");
    model_.setRef(": V. Bellani, E. Cappellari, under development.");
    model.push(model_);

    const double dt = physics_variable["Time step"].getFinalValue();    // (s)
    const double T  = history_variable["Temperature"].getFinalValue();  // (K)
    const double kB = boltzmann_constant;                               // (J/K)

    // Literature values, nothing fitted
    const double gamma  = 1.0;  // metal/UO2 interface energy, incoherent interfaces 0.5-1 J/m2 (Porter, Easterling)
    const double delta  = 100.0e-9;     // near-boundary bubble-free zone of ~100 nm on each side (Angleraud 2024)
    const double V_eff  = 1.44123e-29;  // atomic volume of the FMP phase (m3/at), hcp packing (V. Bellani MSc thesis)
    const double n_min  = 2.0;          // minimum nucleus, metallic dimers (at) (Hong et al. 2012)
    const double lambda = 15e-9;        // spacing of the nucleation sites (m) (V. Bellani MSc thesis)

    // Production, Mo share and metal diffusivity (Mo system: Turnbull et al. 1988)
    double production = 0.0, produced_total = 0.0, produced_mo = 0.0, D = 0.0;  // (at/m3), (m2/s)
    for (auto& system : sciantix_system)
    {
        if (!system.isMetallicFP() || system.getRestructuredMatrix() != 0)
            continue;
        const std::string produced = system.getFissionProductName() + " produced";
        production += sciantix_variable[produced].getIncrement();
        produced_total += sciantix_variable[produced].getFinalValue();
        if (system.getFissionProductName() == "Mo")
        {
            produced_mo = sciantix_variable[produced].getFinalValue();
            D           = system.getFissionProductDiffusivity();
        }
    }
    const double mo_fraction = (produced_total > 0.0) ? produced_mo / produced_total : 0.0;

    // Mo oxidised according to OC at the previous time step;
    double mo_oxide_fraction = 0.0;
    if ((int)input_variable["iThermochimica"].getValue() != 0)
        mo_oxide_fraction = std::clamp(sciantix_variable["Mo in oxide fraction"].getInitialValue(), 0.0, 1.0);
    const double metallic_fraction = std::max(1.0 - mo_fraction * mo_oxide_fraction, 0.0);

    // Irradiation-induced re-solution: FMP shrinkage of Sakib et al.
    // (R 0.91 -> 0.80 between 0.30 and 1.75 dpa, n ~ R^3, about 1 dpa/day),
    // scaled with the fission rate (reference 1.48e19 fiss/m3 s)
    const double k_res = 3.085078e-6 * (history_variable["Fission rate"].getFinalValue() / 1.48e19);  // (1/s)

    // State at the beginning of the time step
    const double C_m2_old = sciantix_variable["FMP in near-boundary zone matrix"].getInitialValue();
    const double C_m1_old = sciantix_variable["FMP in matrix"].getInitialValue() - C_m2_old;
    const double C_p1_old = sciantix_variable["FMP in intragranular particles"].getInitialValue();
    const double C_p2_old = sciantix_variable["FMP in near-boundary particles"].getInitialValue();
    const double C_gb_old = sciantix_variable["FMP at grain boundary"].getInitialValue();
    const double N1_old   = std::max(sciantix_variable["Intragranular FMP concentration"].getInitialValue(), 0.0);
    const double N2_old   = std::max(sciantix_variable["Near-boundary FMP concentration"].getInitialValue(), 0.0);
    const double Ngb_old  = std::max(sciantix_variable["Intergranular FMP concentration"].getInitialValue(), 0.0);

    // Zone volume fractions:
    // intra2 is the shell of thickness delta inside each spherical grain of radius a,
    // phi_2 limited to 0.999 for grains smaller than the layer
    const double a     = sciantix_variable["Grain radius"].getFinalValue();
    const double phi_2 = (a > delta) ? std::min(1.0 - std::pow(1.0 - delta / a, 3.0), 0.999) : 0.999;
    const double phi_1 = 1.0 - phi_2;
    // uniform production
    const double P_1 = phi_1 * production;  // (at/m3)
    const double P_2 = phi_2 * production;  // (at/m3)

    // Equilibrium concentration C_eq = x_eq N_U exp[-(H/kB)(1/T - 1/T_ref)]
    // Mo solubility 0.006 at.% at 1900 degC (Kleykamp), 4/a_UO2^3 cation sites (a_UO2 = 547.5 pm),
    // H = 1.14 eV (DFT solution energy of Mo in UO2)
    const double C_eq_ref = 6.0e-5 * (4.0 / std::pow(547.5e-12, 3.0));  // (at/m3)
    const double C_eq =
        (T > 0.0) ? C_eq_ref * std::exp(-(1.14 / boltzmann_constant_eV) * (1.0 / T - 1.0 / 2173.15)) : C_eq_ref;

    // Nucleation sites:
    // - dislocations (spacing lambda) in intra1 and intra2
    // - areal density 1/lambda^2 on the boundary area 3/(2a) per unit volume for gb
    const double f_T = 0.608 + (1.0 - 0.608) / (1.0 + std::exp((T - 1109.0) / 25.8));  // dislocations
    const double sites_disl =
        (sciantix_variable["Burnup"].getFinalValue() > 0.0 && T > 0.0)
            ? 6.545e12 * std::pow(sciantix_variable["Burnup"].getFinalValue(), 1.151) * f_T / lambda
            : 0.0;  // (1/m3)
    const double sites_1  = phi_1 * sites_disl;
    const double sites_2  = phi_2 * sites_disl;
    const double sites_gb = (a > 0.0) ? 3.0 / (2.0 * a) / (lambda * lambda) : 0.0;

    // Particle radius from the metallic part of the inventory
    auto radius = [&](double atoms, double particles)
    {
        return std::pow(3.0 * (std::max(atoms, 0.0) * metallic_fraction) * V_eff /
                            (4.0 * M_PI * std::max(particles, 1e-30)),
                        1.0 / 3.0);
    };
    const double R1_old = radius(C_p1_old, N1_old);
    const double R2_old = radius(C_p2_old, N2_old);

    // Diffusion-limited growth k = 4 pi D N R (Ham), state at the beginning of the time step
    const double k_1 = 4.0 * M_PI * D * N1_old * R1_old;  // (1/s)
    double       k_2 = 4.0 * M_PI * D * N2_old * R2_old;  // (1/s)

    // Exchange coefficients between the three zones: intra1 -> intra2 -> grain boundary
    // Sinks: the particles of each zone, and the grain boundary, treated as a perfect sink at concentration C_eq.

    // The transport is modelled as conductances in series:
    //   c_1 --[intra1 -> its edge]--[half of intra2]-- c_2 --[half of intra2]-- C_eq
    //                                                   |
    //                                           [intra2 particles]
    // giving the fluxes (per unit grain volume):
    //   intra1 -> intra2:  K_12 (c_1 - c_2)
    //   intra2 -> gb:      K_23 (c_2 - C_eq)
    //
    // intra1: sphere of radius a with internal sinks (Brailsford and Bullough).
    //   k_1^2 = 4 pi n_1 R_1, sink strength of the intra1 particles.
    //   z = k_1 a compares the grain radius with the diffusion length before capture, 1/k_1.
    //   F = 3 (coth z - 1/z) / z, fraction of the atoms that reach the boundary.
    //   k_GB^2 = k_1^2 F / (1 - F), sink strength of the boundary seen from intra1:
    //     z -> 0 (few particles):   k_GB^2 -> 15 / a^2  (pure diffusion in a sphere)
    //     z >> 1 (many particles):  k_GB^2 -> 3 k_1 / a (only atoms near the boundary reach it)
    //
    // intra2: slab of thickness delta between intra1 and the boundary.
    //   kappa_2^2 = 4 pi n_2 R_2, theta = kappa_2 delta.
    //   The slab is replaced by its exact steady-state T equivalent:
    //     each half has effective length L = tanh(theta/2) / kappa_2 (L = delta/2 without particles),
    //     the intra2 sink acts at the central node and is multiplied by sinh(theta) / theta.
    //   c_2 is therefore the concentration at the middle of the slab.
    //
    // Resistance of half of intra2 per unit grain volume: L / (3/a)
    //   K_12 = D / (1/k_GB^2 + a L / 3)
    //   K_23 = 3 D / (a L)
    double K_12 = 0.0, K_23 = 0.0;
    if (a > 0.0)
    {
        // intra1: sink strength of the particles (local density n_1 = N1 / phi_1)
        const double k1_squared = 4.0 * M_PI * (N1_old / phi_1) * R1_old;
        // z = grain radius / diffusion length before capture in an intra1 particle
        const double z = std::sqrt(k1_squared) * a;
        // boundary sink strength without particles (limit z -> 0)
        double k_gb_squared = 15.0 / (a * a);
        if (z > 1.0e-2)  // below this, the small-z limit is used (avoids round-off in 1 - F)
        {
            // F = fraction of atoms reaching the boundary; coth(z) = 1 for z > 20 (avoids overflow)
            const double F = 3.0 / z * (((z > 20.0) ? 1.0 : std::cosh(z) / std::sinh(z)) - 1.0 / z);
            if (F < 1.0)
                // max: safety against round-off, k_GB^2 >= 15/a^2 analytically
                k_gb_squared = std::max(k1_squared * F / (1.0 - F), k_gb_squared);
        }
        // intra2: inverse diffusion length before capture in an intra2 particle (local density n_2 = N2 / phi_2)
        const double kappa_2 = std::sqrt(4.0 * M_PI * (N2_old / phi_2) * R2_old);
        const double theta   = kappa_2 * delta;
        // effective length of half slab (T equivalent); delta/2 in the limit of no particles
        const double L = (theta > 1.0e-3) ? std::tanh(0.5 * theta) / kappa_2 : 0.5 * delta;
        // intra2 sink at the central node of the T equivalent
        if (theta > 1.0e-3)
            k_2 *= std::sinh(theta) / theta;
        K_12 = D / (1.0 / k_gb_squared + a * L / 3.0);
        K_23 = 3.0 * D / (a * L);
    }

    // CNT (Kashchiev)
    // per site: nu = Z beta* exp(-dG*/kB T)
    //           dG* = 16 pi gamma^3 Omega^2 / (3 dmu^2), spherical nucleus
    //           dmu = kB T ln S, n* = max(n_min, 2 dG*/dmu),
    //           r* = 2 gamma Omega / dmu,
    //           beta* = 4 pi r* D c,
    //           Z = [dG*/(3 pi kB T)]^(1/2) / n*;
    // S of the zone after the production of the time step
    struct Nucleation
    {
        double dG = 0.0, n_crit = 2.0, nu = 0.0;
    };
    auto nucleation = [&](double c)
    {
        Nucleation   n;
        const double S = (C_eq > 0.0) ? c / C_eq : 0.0;
        if (T <= 0.0 || S <= 1.0 || std::log(S) <= 1.0e-12)
            return n;
        const double dmu  = kB * T * std::log(S);                                                    // (J)
        n.dG              = 16.0 * M_PI * std::pow(gamma, 3.0) * V_eff * V_eff / (3.0 * dmu * dmu);  // (J)
        n.n_crit          = std::max(n_min, 2.0 * n.dG / dmu);
        const double Z    = std::sqrt(n.dG / (3.0 * M_PI * kB * T)) / n.n_crit;
        const double beta = 4.0 * M_PI * (2.0 * gamma * V_eff / dmu) * D * c;  // (1/s)
        if (n.dG / (kB * T) < 700.0)
            n.nu = Z * beta * std::exp(-n.dG / (kB * T));
        return n;
    };
    const double c_1 = std::max(C_m1_old + P_1, 0.0) / phi_1;
    const double c_2 = std::max(C_m2_old + P_2, 0.0) / phi_2;

    // Nucleation: implicit dN/dt = nu (N_site - N), N never decreases; each nucleus takes n* atoms, limited by the
    // atoms available: from the matrix of its zone for intra1 and intra2, from the metal already on the boundary for gb
    // (supersaturation of intra2, the mass stays in the gb inventory)
    auto nucleate = [&](const Nucleation& n, double sites, double N_old, double available, double& mass)
    {
        const double dN = std::max((N_old + sites * n.nu * dt) / (1.0 + n.nu * dt) - N_old, 0.0);
        mass            = std::min(n.n_crit * dN, std::max(available, 0.0));
        return N_old + mass / n.n_crit;
    };
    double       mass_1 = 0.0, mass_2 = 0.0, mass_gb = 0.0;
    const double N1  = nucleate(nucleation(c_1), sites_1, N1_old, C_m1_old + P_1, mass_1);
    const double N2  = nucleate(nucleation(c_2), sites_2, N2_old, C_m2_old + P_2, mass_2);
    const double Ngb = nucleate(nucleation(c_2), sites_gb, Ngb_old, C_gb_old, mass_gb);

    // Growth J = k (c - C_eq), switched on in each zone if c > C_eq after production and nucleation; dissolution only
    // by re-solution. Implicit 5x5 balance, X = (C_m1, C_m2, C_p1, C_p2, C_gb):
    //   dC_m1/dt = P_1 - k_1 (c_1 - C_eq) - K_12 (c_1 - c_2) + k_res C_p1
    //   dC_m2/dt = P_2 - (k_2 + K_23) (c_2 - C_eq) + K_12 (c_1 - c_2) + k_res (C_p2 + C_gb)
    //   dC_p1/dt = k_1 (c_1 - C_eq) - k_res C_p1
    //   dC_p2/dt = k_2 (c_2 - C_eq) - k_res C_p2
    //   dC_gb/dt = K_23 (c_2 - C_eq) - k_res C_gb
    // Every column sums to one: the total inventory is conserved.
    const bool   supersaturated_2 = (C_m2_old + P_2 - mass_2) / phi_2 > C_eq;
    const double g_1              = ((C_m1_old + P_1 - mass_1) / phi_1 > C_eq) ? k_1 : 0.0;
    const double g_2              = supersaturated_2 ? k_2 : 0.0;
    const double g_gb             = supersaturated_2 ? K_23 : 0.0;

    const double a1 = dt / phi_1, a2 = dt / phi_2, r = k_res * dt;
    double       A[25] = {1.0 + (g_1 + K_12) * a1,
                          -K_12 * a2,
                          -r,
                          0.0,
                          0.0,
                          -K_12 * a1,
                          1.0 + (g_2 + g_gb + K_12) * a2,
                          0.0,
                          -r,
                          -r,
                          -g_1 * a1,
                          0.0,
                          1.0 + r,
                          0.0,
                          0.0,
                          0.0,
                          -g_2 * a2,
                          0.0,
                          1.0 + r,
                          0.0,
                          0.0,
                          -g_gb * a2,
                          0.0,
                          0.0,
                          1.0 + r};
    double       X[5]  = {C_m1_old + P_1 - mass_1 + g_1 * C_eq * dt,
                          C_m2_old + P_2 - mass_2 + (g_2 + g_gb) * C_eq * dt,
                          C_p1_old + mass_1 - g_1 * C_eq * dt,
                          C_p2_old + mass_2 - g_2 * C_eq * dt,
                          C_gb_old - g_gb * C_eq * dt};
    solver.Laplace(5, A, X);

    if (!std::all_of(X, X + 5, [](double x) { return std::isfinite(x); }))
        std::cerr << "ERROR FMP: non-finite concentration | T = " << T << " | dt = " << dt << " | P = " << production
                  << " | C_eq = " << C_eq << " | D = " << D << " | k_1 = " << g_1 << " | k_2 = " << g_2
                  << " | K_23 = " << g_gb << " | K_12 = " << K_12 << " | k_res = " << k_res << std::endl;

    auto radius_out = [&](double atoms, double particles)
    { return (particles >= 1.0 && atoms > 0.0) ? radius(atoms, particles) : 0.0; };
    sciantix_variable["FMP in matrix"].setFinalValue(X[0] + X[1]);
    sciantix_variable["FMP in near-boundary zone matrix"].setFinalValue(X[1]);
    sciantix_variable["FMP in intragranular particles"].setFinalValue(X[2]);
    sciantix_variable["FMP in near-boundary particles"].setFinalValue(X[3]);
    sciantix_variable["FMP at grain boundary"].setFinalValue(X[4]);
    sciantix_variable["Intragranular FMP concentration"].setFinalValue(N1);
    sciantix_variable["Near-boundary FMP concentration"].setFinalValue(N2);
    sciantix_variable["Intergranular FMP concentration"].setFinalValue(Ngb);
    sciantix_variable["FMP near-boundary zone volume fraction"].setFinalValue(phi_2);
    sciantix_variable["Intragranular FMP radius"].setFinalValue(radius_out(X[2], N1));
    sciantix_variable["Near-boundary FMP radius"].setFinalValue(radius_out(X[3], N2));
    sciantix_variable["Intergranular FMP radius"].setFinalValue(radius_out(X[4], Ngb));
}
