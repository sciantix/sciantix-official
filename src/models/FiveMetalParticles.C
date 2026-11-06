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
 * Five metal particles (FMP): nucleation (CNT), growth and irradiation-induced re-solution of the Mo-Tc-Ru-Rh-Pd
 * precipitates in three zones.
 *
 * EC November 2026, changes with respect to the model of V. Bellani:
 *  - FMP naming, single switch iFMP, radii as outputs;
 *  - production from the Mo, Tc, Ru, Rh, Pd systems ("X produced", LWR yields) instead of y = 0.578;
 *  - three zones: intra1, the grain interior; intra2, the near-boundary zone (delta on each side of the boundaries,
 *    bubble-free zone of Angleraud 2024), with its own dissolved metal; gb, the particles on the boundary, fed from
 *    intra2;
 *  - literature parameters only (no calibration): Turnbull diffusivity, CNT barrier from the interface energy, CNT
 *    nucleation frequency Z beta*;
 *  - Mo oxidation from OpenCalphad applied to all the particles; the oxygen taken by the oxides is removed from the
 *    fuel in SetPhaseDiagram.
 * All concentrations are per unit fuel volume; c = C / phi is the local concentration in a zone.
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

    // EC November 2026: literature values, nothing fitted on the TEM data used for the validation
    const double gamma  = 1.0;          // metal/UO2 interface energy (J/m2), incoherent interfaces 0.5-1 J/m2 (Porter, Easterling)
    const double delta  = 100.0e-9;     // near-boundary zone, bubble-free zone of ~100 nm on each side of the boundaries (m) (Angleraud 2024)
    const double V_eff  = 1.44123e-29;  // atomic volume of the FMP phase (m3/at), atomic radii and hcp packing (V. Bellani)
    const double n_min  = 2.0;          // minimum nucleus, metallic dimers (at) (Hong et al. 2012)
    const double lambda = 15e-9;        // spacing of the nucleation sites (m) (V. Bellani)

    // EC November 2026: grain-boundary nucleation on a lens, Phi = (2 + cos)(1 - cos)^2 / 2 with cos = gamma_gb / (2 gamma), gamma_gb = 2 gamma_s cos(theta_b) from the UO2 surface tension and semi-dihedral angle (0.7 J/m2, 50 deg: gamma_gb = 0.90 J/m2)
    const double cos_gb = std::min(matrices[0].getSurfaceTension() * std::cos(matrices[0].getSemidihedralAngle()) / gamma, 1.0);
    const double phi_het_gb = 0.5 * (2.0 + cos_gb) * (1.0 - cos_gb) * (1.0 - cos_gb);

    // Production (EC November 2026: from the per-element systems), Mo share and metal diffusivity (Mo system: Turnbull et al. 1988)
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

    // EC November 2026: Mo oxidised according to OC at the previous time step;
    double mo_oxide_fraction = 0.0;
    if ((int)input_variable["iThermochimica"].getValue() != 0)
        mo_oxide_fraction = std::clamp(sciantix_variable["Mo in oxide fraction"].getInitialValue(), 0.0, 1.0);
    const double metallic_fraction = std::max(1.0 - mo_fraction * mo_oxide_fraction, 0.0);

    // Irradiation-induced re-solution: FMP shrinkage of Sakib et al. (R 0.91 -> 0.80 between 0.30 and 1.75 dpa, n ~ R^3, about 1 dpa/day), scaled with the fission rate (reference 1.48e19 fiss/m3 s)
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

    // Zone volume fractions: intra2 is the shell of thickness delta inside each spherical grain of radius a, phi_2 = 1 - (1 - delta/a)^3, limited to 0.999 for grains smaller than the layer; uniform production
    const double a     = sciantix_variable["Grain radius"].getFinalValue();
    const double phi_2 = (a > delta) ? std::min(1.0 - std::pow(1.0 - delta / a, 3.0), 0.999) : 0.999;
    const double phi_1 = 1.0 - phi_2;
    const double P_1   = phi_1 * production;  // (at/m3)
    const double P_2   = phi_2 * production;

    // Equilibrium concentration C_eq = x_eq N_U exp[-(H/kB)(1/T - 1/T_ref)]: Mo solubility 0.006 at.% at 1900 degC (Kleykamp), 4/a_UO2^3 cation sites (a_UO2 = 547.5 pm), H = 1.14 eV (DFT solution energy of Mo in UO2)
    const double C_eq_ref = 6.0e-5 * (4.0 / std::pow(547.5e-12, 3.0));  // (at/m3)
    const double C_eq     = (T > 0.0) ? C_eq_ref * std::exp(-(1.14 / boltzmann_constant_eV) * (1.0 / T - 1.0 / 2173.15))
                                      : C_eq_ref;

    // Nucleation sites: dislocations (weight 0.67, spacing lambda) in intra1 and intra2, intragranular bubbles (weight 0.33) only in intra1 (EC November 2026: no bubble/precipitate pairs in the bubble-free zone, Angleraud 2024), areal density 1/lambda^2 on the boundary area 3/(2a) per unit volume for gb
    const double burnup = sciantix_variable["Burnup"].getFinalValue();
    const double f_T    = 0.608 + (1.0 - 0.608) / (1.0 + std::exp((T - 1109.0) / 25.8));
    const double sites_disl =
        (burnup > 0.0 && T > 0.0) ? 0.67 * 6.545e12 * std::pow(burnup, 1.151) * f_T / lambda : 0.0;  // (1/m3)
    const double sites_1  = phi_1 * sites_disl + 0.33 * sciantix_variable["Intragranular bubble concentration"].getFinalValue();
    const double sites_2  = phi_2 * sites_disl;
    const double sites_gb = (a > 0.0) ? 3.0 / (2.0 * a) / (lambda * lambda) : 0.0;

    // Particle radius from the metallic part of the inventory
    auto radius = [&](double atoms, double particles)
    {
        return std::pow(3.0 * (std::max(atoms, 0.0) * metallic_fraction) * V_eff /
                            (4.0 * M_PI * std::max(particles, 1e-30)),
                        1.0 / 3.0);
    };
    const double R1_old  = radius(C_p1_old, N1_old);
    const double R2_old  = radius(C_p2_old, N2_old);
    const double Rgb_old = radius(C_gb_old, Ngb_old);

    // Diffusion-limited growth k = 4 pi D N R (Ham), state at the beginning of the time step
    const double k_1  = 4.0 * M_PI * D * N1_old * R1_old;    // (1/s)
    const double k_2  = 4.0 * M_PI * D * N2_old * R2_old;    // (1/s)
    const double k_gb = 4.0 * M_PI * D * Ngb_old * Rgb_old;  // (1/s)

    // Exchange intra1 -> intra2, flux K_12 (c_1 - c_2): grain-boundary sink strength of a sphere with internal sinks k_1^2 = 4 pi n_1 R_1 (Brailsford and Bullough), k_GB^2 = k_1^2 F / (1 - F), F = 3 (coth z - 1/z) / z, z = k_1 a, from 15/a^2 without particles to 3 k_1 / a; in series with the half-thickness of intra2 (interface area 3/a): K_12 = D / (1/k_GB^2 + a delta / 6).
    // EC November 2026: intra2 -> gb, diffusion from the middle of intra2 to the boundary, K_23 = 6 D / (a delta), in series with the capture by the gb particles, k_23 = K_23 k_gb / (K_23 + k_gb); quasi-steady concentration at the boundary c_b = (K_23 c_2 + k_gb C_eq) / (K_23 + k_gb).
    double K_12 = 0.0, K_23 = 0.0;
    if (a > 0.0)
    {
        const double k1_squared   = 4.0 * M_PI * (N1_old / phi_1) * R1_old;
        const double z            = std::sqrt(k1_squared) * a;
        double       k_gb_squared = 15.0 / (a * a);
        if (z > 1.0e-2)
        {
            const double F = 3.0 / z * (((z > 20.0) ? 1.0 : std::cosh(z) / std::sinh(z)) - 1.0 / z);
            if (F < 1.0)
                k_gb_squared = std::max(k1_squared * F / (1.0 - F), k_gb_squared);
        }
        K_12 = D / (1.0 / k_gb_squared + a * delta / 6.0);
        K_23 = 6.0 * D / (a * delta);
    }
    const double k_23 = (K_23 + k_gb > 0.0) ? K_23 * k_gb / (K_23 + k_gb) : 0.0;

    // CNT (Kashchiev), per site: nu = Z beta* exp(-dG*/kB T), dG* = 16 pi gamma^3 Omega^2 Phi / (3 dmu^2), dmu = kB T ln S, n* = max(n_min, 2 dG*/dmu), r* = 2 gamma Omega / dmu, beta* = 4 pi r* D c, Z = [dG*/(3 pi kB T)]^(1/2) / n*; S of the zone after the production of the time step
    struct Nucleation
    {
        double dG = 0.0, n_crit = 2.0, nu = 0.0;
    };
    auto nucleation = [&](double c, double phi_het)
    {
        Nucleation   n;
        const double S = (C_eq > 0.0) ? c / C_eq : 0.0;
        if (T <= 0.0 || S <= 1.0 || std::log(S) <= 1.0e-12)
            return n;
        const double dmu = kB * T * std::log(S);                                                       // (J)
        n.dG             = 16.0 * M_PI * std::pow(gamma, 3.0) * V_eff * V_eff * phi_het / (3.0 * dmu * dmu);  // (J)
        n.n_crit         = std::max(n_min, 2.0 * n.dG / dmu);
        const double Z   = std::sqrt(n.dG / (3.0 * M_PI * kB * T)) / n.n_crit;
        const double beta = 4.0 * M_PI * (2.0 * gamma * V_eff / dmu) * D * c;  // (1/s)
        if (n.dG / (kB * T) < 700.0)
            n.nu = Z * beta * std::exp(-n.dG / (kB * T));
        return n;
    };
    const double c_1 = std::max(C_m1_old + P_1, 0.0) / phi_1;
    const double c_2 = std::max(C_m2_old + P_2, 0.0) / phi_2;
    const double c_b = (K_23 + k_gb > 0.0) ? (K_23 * c_2 + k_gb * C_eq) / (K_23 + k_gb) : c_2;

    // Nucleation: implicit dN/dt = nu (N_site - N), N never decreases; each nucleus takes n* atoms from the matrix of its zone, limited by the atoms available there (gb first, then intra2, from the metal of intra2)
    auto nucleate = [&](const Nucleation& n, double sites, double N_old, double available, double& mass)
    {
        const double dN = std::max((N_old + sites * n.nu * dt) / (1.0 + n.nu * dt) - N_old, 0.0);
        mass            = std::min(n.n_crit * dN, std::max(available, 0.0));
        return N_old + mass / n.n_crit;
    };
    double       mass_1 = 0.0, mass_2 = 0.0, mass_gb = 0.0;
    const double N1  = nucleate(nucleation(c_1, 1.0), sites_1, N1_old, C_m1_old + P_1, mass_1);
    const double Ngb = nucleate(nucleation(c_b, phi_het_gb), sites_gb, Ngb_old, C_m2_old + P_2, mass_gb);
    const double N2  = nucleate(nucleation(c_2, 1.0), sites_2, N2_old, C_m2_old + P_2 - mass_gb, mass_2);

    // Growth J = k (c - C_eq), switched on in each zone if c > C_eq after production and nucleation; dissolution only by re-solution (back to the zone of the particles). Implicit 5x5 balance, X = (C_m1, C_m2, C_p1, C_p2, C_gb):
    //   dC_m1/dt = P_1 - k_1 (c_1 - C_eq) - K_12 (c_1 - c_2) + k_res C_p1
    //   dC_m2/dt = P_2 - (k_2 + k_23) (c_2 - C_eq) + K_12 (c_1 - c_2) + k_res (C_p2 + C_gb)
    //   dC_p1/dt = k_1 (c_1 - C_eq) - k_res C_p1
    //   dC_p2/dt = k_2 (c_2 - C_eq) - k_res C_p2
    //   dC_gb/dt = k_23 (c_2 - C_eq) - k_res C_gb
    // Every column sums to one: the total inventory is conserved.
    const bool   supersaturated_2 = (C_m2_old + P_2 - mass_gb - mass_2) / phi_2 > C_eq;
    const double g_1  = ((C_m1_old + P_1 - mass_1) / phi_1 > C_eq) ? k_1 : 0.0;
    const double g_2  = supersaturated_2 ? k_2 : 0.0;
    const double g_gb = supersaturated_2 ? k_23 : 0.0;

    const double a1 = dt / phi_1, a2 = dt / phi_2, r = k_res * dt;
    double       A[25] = {1.0 + (g_1 + K_12) * a1, -K_12 * a2,                      -r,      0.0,     0.0,
                          -K_12 * a1,              1.0 + (g_2 + g_gb + K_12) * a2, 0.0,     -r,      -r,
                          -g_1 * a1,               0.0,                            1.0 + r, 0.0,     0.0,
                          0.0,                     -g_2 * a2,                      0.0,     1.0 + r, 0.0,
                          0.0,                     -g_gb * a2,                     0.0,     0.0,     1.0 + r};
    double       X[5]  = {C_m1_old + P_1 - mass_1 + g_1 * C_eq * dt,
                          C_m2_old + P_2 - mass_gb - mass_2 + (g_2 + g_gb) * C_eq * dt,
                          C_p1_old + mass_1 - g_1 * C_eq * dt,
                          C_p2_old + mass_2 - g_2 * C_eq * dt,
                          C_gb_old + mass_gb - g_gb * C_eq * dt};
    solver.Laplace(5, A, X);

    if (!std::all_of(X, X + 5, [](double x) { return std::isfinite(x); }))
        std::cerr << "ERROR FMP: non-finite concentration | T = " << T << " | dt = " << dt << " | P = " << production
                  << " | C_eq = " << C_eq << " | D = " << D << " | k_1 = " << g_1 << " | k_2 = " << g_2
                  << " | k_23 = " << g_gb << " | K_12 = " << K_12 << " | k_res = " << k_res << std::endl;

    auto radius_out = [&](double atoms, double particles) { return (particles >= 1.0 && atoms > 0.0) ? radius(atoms, particles) : 0.0; };
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
