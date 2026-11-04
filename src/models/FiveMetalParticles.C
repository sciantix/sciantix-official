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
 * precipitates, intragranular and at the grain boundary.
 *
 * EC November 2026, changes with respect to the model of V. Bellani:
 *  - FMP naming, single switch iFMP, calibrated parameters in the code, radii as outputs;
 *  - production from the Mo, Tc, Ru, Rh, Pd systems ("X produced", LWR yields) instead of y = 0.578;
 *  - two zones coupled by diffusion, each with its own dissolved metal: zone 1, the grain interior, with the
 *    intragranular particles; zone 2, the grain-boundary zone (layer of thickness delta_gb on each side of the
 *    boundaries), with the grain-boundary particles;
 *  - Mo oxidation from OpenCalphad applied to all the particles (uniform oxygen potential); the oxygen taken by
 *    the oxides is removed from the fuel in SetPhaseDiagram.
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
    const double kB = boltzmann_constant_eV;                            // (eV/K)

    // EC November 2026: parameters calibrated with the two-zone model on the UO2 LWR cases, with free grain-boundary
    // values (validation/metallicinclusions/calibration_two_zones). Calibration of V. Bellani, common values:
    // k_0 = 4.980333299845e-15 m2/s, B = 2.665764962509e8 eV K2.
    const double k_intra_0 = 9.834342e-16, k_gb_0 = 3.009503e-15;  // growth prefactors (m2/s)
    const double dG_intra = 1.0, dG_gb = 1.0;                       // growth activation energies (eV)
    const double B_intra = 2.711740e8, B_gb = 2.641409e8;           // CNT barrier coefficients (eV K2)
    const double nu0_intra = 1.0, nu0_gb = 1.0;                     // nucleation prefactors (1/s)

    // EC November 2026: half-width of the grain-boundary zone, equal to TEM grain-boundary regions of the calibration (V. Bellani, thesis, Figs. 5.3-5.16)
    const double delta_gb = 100.0e-9;  // (m)

    const double V_eff  = 1.44123e-29;  // effective atomic volume of the FMP phase (m3/at)
    const double n_min  = 2.0;          // minimum nucleus size, metallic dimers (at)
    const double lambda = 15e-9;        // spacing of the effective nucleation sites (m)

    // Production (EC November 2026: from the per-element systems) and Mo share of the lumped species
    double production = 0.0, produced_total = 0.0, produced_mo = 0.0;  // (at/m3)
    for (auto& system : sciantix_system)
    {
        if (!system.isMetallicFP() || system.getRestructuredMatrix() != 0)
            continue;
        const std::string produced = system.getFissionProductName() + " produced";
        production += sciantix_variable[produced].getIncrement();
        produced_total += sciantix_variable[produced].getFinalValue();
        if (system.getFissionProductName() == "Mo")
            produced_mo = sciantix_variable[produced].getFinalValue();
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
    const double C_m2_old    = sciantix_variable["FMP in grain-boundary zone matrix"].getInitialValue();
    const double C_m1_old    = sciantix_variable["FMP in matrix"].getInitialValue() - C_m2_old;
    const double C_intra_old = sciantix_variable["FMP in intragranular particles"].getInitialValue();
    const double C_gb_old    = sciantix_variable["FMP at grain boundary"].getInitialValue();
    const double N_intra_old = sciantix_variable["Intragranular FMP concentration"].getInitialValue();
    const double N_gb_old    = sciantix_variable["Intergranular FMP concentration"].getInitialValue();

    // Zone volume fractions: zone 2 is the shell of thickness delta_gb inside each spherical grain of radius a, phi_2 = 1 - (1 - delta_gb/a)^3, limited to 0.999 for grains smaller than the layer
    const double a     = sciantix_variable["Grain radius"].getFinalValue();
    const double phi_2 = (a > delta_gb) ? std::min(1.0 - std::pow(1.0 - delta_gb / a, 3.0), 0.999) : 0.999;
    const double phi_1 = 1.0 - phi_2;
    const double P_1   = phi_1 * production;  // uniform production (at/m3)
    const double P_2   = phi_2 * production;

    // Equilibrium concentration C_eq = x_eq N_U exp[-(H/kB)(1/T - 1/T_ref)]: Mo solubility 0.006 at.% at 1900 degC (Kleykamp), 4/a_UO2^3 cation sites (a_UO2 = 547.5 pm), H = 1.14 eV (DFT solution energy of Mo in UO2)
    const double C_eq_ref = 6.0e-5 * (4.0 / std::pow(547.5e-12, 3.0));  // (at/m3)
    const double C_eq     = (T > 0.0) ? C_eq_ref * std::exp(-(1.14 / kB) * (1.0 / T - 1.0 / 2173.15)) : C_eq_ref;

    // Nucleation sites. Intragranular: dislocations and intragranular bubbles of the previous time step, weights 0.67/0.33 from image analysis; EC November 2026: fraction phi_1 in zone 1. Grain boundary: areal density 1/lambda^2 on the boundary area 3/(2a) per unit volume.
    const double burnup = sciantix_variable["Burnup"].getFinalValue();
    const double f_T    = 0.608 + (1.0 - 0.608) / (1.0 + std::exp((T - 1109.0) / 25.8));
    const double rho_dislocations =
        (burnup > 0.0 && T > 0.0) ? 6.545e12 * std::pow(burnup, 1.151) * f_T : 0.0;  // (1/m2)
    const double sites_intra = phi_1 * (0.67 * rho_dislocations / lambda +
                                        0.33 * sciantix_variable["Intragranular bubble concentration"].getFinalValue());
    const double sites_gb    = (a > 0.0) ? 3.0 / (2.0 * a) / (lambda * lambda) : 0.0;

    // CNT nucleation frequency nu = nu0 exp(-dG*/kB T), dG* = B / [T^2 ln(S)^2], driving force dmu = kB T ln(S), with the supersaturation S of the zone after the production of the time step (EC November 2026: per zone)
    struct Nucleation
    {
        double dmu = 0.0, dG = 0.0, nu = 0.0;
    };
    auto nucleation = [&](double C_local, double B, double nu0)
    {
        Nucleation   n;
        const double S = (C_eq > 0.0) ? C_local / C_eq : 0.0;
        if (T <= 0.0 || S <= 1.0 || std::log(S) <= 1.0e-12)
            return n;
        n.dmu = kB * T * std::log(S);
        n.dG  = B / (T * T * std::log(S) * std::log(S));
        if (n.dG / (kB * T) < 700.0)
            n.nu = nu0 * std::exp(-n.dG / (kB * T));
        return n;
    };
    const Nucleation nucl_intra = nucleation(std::max(C_m1_old + P_1, 0.0) / phi_1, B_intra, nu0_intra);
    const Nucleation nucl_gb    = nucleation(std::max(C_m2_old + P_2, 0.0) / phi_2, B_gb, nu0_gb);

    // Particle radius from the metallic part of the inventory
    auto radius = [&](double atoms, double particles)
    {
        return std::pow(3.0 * (std::max(atoms, 0.0) * metallic_fraction) * V_eff /
                            (4.0 * M_PI * std::max(particles, 1e-30)),
                        1.0 / 3.0);
    };
    const double R_intra_old = radius(C_intra_old, N_intra_old);
    const double R_gb_old    = radius(C_gb_old, N_gb_old);

    // Diffusion-limited growth, k = D 4 pi N R with D = k_0 exp(-dG / kB T), from the state at the beginning of the time step (no iteration: positive feedback of R on the growth rate)
    const double D_intra = k_intra_0 * std::exp(-dG_intra / (kB * T));
    const double D_gb    = k_gb_0 * std::exp(-dG_gb / (kB * T));
    const double k_intra = D_intra * 4.0 * M_PI * std::max(N_intra_old, 1e-30) * R_intra_old;  // (1/s)
    const double k_gb    = D_gb * 4.0 * M_PI * std::max(N_gb_old, 1e-30) * R_gb_old;           // (1/s)

    // [TBC] EC November 2026: exchange between the zones, flux K_ex (c_1 - c_2) per unit volume. Conductance of the grain interior: grain-boundary sink strength of a sphere with internal sinks k_1^2 = 4 pi n_intra R_intra (Brailsford and Bullough), k_GB^2 = k_1^2 F / (1 - F), F = 3 (coth z - 1/z) / z, z = k_1 a, from 15/a^2 without particles to 3 k_1 / a; in series with the half-thickness of zone 2 (interface area 3/a): K_ex = D / (1/k_GB^2 + a delta_gb / 6).
    double K_ex = 0.0;
    if (a > 0.0)
    {
        const double k1_squared   = 4.0 * M_PI * (std::max(N_intra_old, 0.0) / phi_1) * R_intra_old;
        const double z            = std::sqrt(k1_squared) * a;
        double       k_gb_squared = 15.0 / (a * a);
        if (z > 1.0e-2)
        {
            const double F = 3.0 / z * (((z > 20.0) ? 1.0 : std::cosh(z) / std::sinh(z)) - 1.0 / z);
            if (F < 1.0)
                k_gb_squared = std::max(k1_squared * F / (1.0 - F), k_gb_squared);
        }
        K_ex = D_intra / (1.0 / k_gb_squared + a * delta_gb / 6.0);
    }

    // Nucleation: implicit dN/dt = S - nu N, N never decreases; each nucleus takes n* = max(n_min, 2 dG*/dmu) atoms from the matrix of its zone, limited by the atoms available there (then dN is reduced at fixed n*)
    auto nucleate = [&](const Nucleation& n, double sites, double N_old, double available, double& mass)
    {
        const double dN     = std::max((N_old + sites * n.nu * dt) / (1.0 + n.nu * dt) - N_old, 0.0);
        const double n_crit = (n.dmu > 0.0 && dN > 0.0 && n.dG > 0.0) ? std::max(n_min, 2.0 * n.dG / n.dmu) : n_min;
        mass                = std::min(n_crit * dN, std::max(available, 0.0));
        return N_old + mass / n_crit;
    };
    double       mass_intra = 0.0, mass_gb = 0.0;
    const double N_intra = nucleate(nucl_intra, sites_intra, N_intra_old, C_m1_old + P_1, mass_intra);
    const double N_gb    = nucleate(nucl_gb, sites_gb, N_gb_old, C_m2_old + P_2, mass_gb);

    // Growth J = k (c - C_eq), switched on in each zone if c > C_eq after production and nucleation; dissolution only by re-solution. Implicit 4x4 balance, X = (C_m1, C_m2, C_intra, C_gb):
    //   dC_m1/dt    = P_1 - k_intra (c_1 - C_eq) - K_ex (c_1 - c_2) + k_res C_intra
    //   dC_m2/dt    = P_2 - k_gb (c_2 - C_eq)    + K_ex (c_1 - c_2) + k_res C_gb
    //   dC_intra/dt = k_intra (c_1 - C_eq) - k_res C_intra
    //   dC_gb/dt    = k_gb (c_2 - C_eq)    - k_res C_gb
    // Every column sums to one: the total inventory is conserved.
    const double k_1 = ((C_m1_old + P_1 - mass_intra) / phi_1 > C_eq) ? k_intra : 0.0;
    const double k_2 = ((C_m2_old + P_2 - mass_gb) / phi_2 > C_eq) ? k_gb : 0.0;

    double A[16] = {1.0 + (k_1 + K_ex) * dt / phi_1,
                    -K_ex * dt / phi_2,
                    -k_res * dt,
                    0.0,
                    -K_ex * dt / phi_1,
                    1.0 + (k_2 + K_ex) * dt / phi_2,
                    0.0,
                    -k_res * dt,
                    -k_1 * dt / phi_1,
                    0.0,
                    1.0 + k_res * dt,
                    0.0,
                    0.0,
                    -k_2 * dt / phi_2,
                    0.0,
                    1.0 + k_res * dt};
    double X[4]  = {C_m1_old + P_1 - mass_intra + k_1 * C_eq * dt,
                    C_m2_old + P_2 - mass_gb + k_2 * C_eq * dt,
                    C_intra_old + mass_intra - k_1 * C_eq * dt,
                    C_gb_old + mass_gb - k_2 * C_eq * dt};
    solver.Laplace(4, A, X);

    if (!std::isfinite(X[0]) || !std::isfinite(X[1]) || !std::isfinite(X[2]) || !std::isfinite(X[3]))
        std::cerr << "ERROR FMP: non-finite concentration | T = " << T << " | dt = " << dt << " | P = " << production
                  << " | C_eq = " << C_eq << " | k_intra = " << k_1 << " | k_gb = " << k_2 << " | K_ex = " << K_ex
                  << " | k_res = " << k_res << " | N_intra = " << N_intra << " | N_GB = " << N_gb << std::endl;

    sciantix_variable["FMP in matrix"].setFinalValue(X[0] + X[1]);
    sciantix_variable["FMP in grain-boundary zone matrix"].setFinalValue(X[1]);
    sciantix_variable["FMP in intragranular particles"].setFinalValue(X[2]);
    sciantix_variable["FMP at grain boundary"].setFinalValue(X[3]);
    sciantix_variable["Intragranular FMP concentration"].setFinalValue(N_intra);
    sciantix_variable["Intergranular FMP concentration"].setFinalValue(N_gb);
    sciantix_variable["FMP grain-boundary zone volume fraction"].setFinalValue(phi_2);
    sciantix_variable["Intragranular FMP radius"].setFinalValue((N_intra >= 1.0 && X[2] > 0.0) ? radius(X[2], N_intra)
                                                                                               : 0.0);
    sciantix_variable["Intergranular FMP radius"].setFinalValue((N_gb >= 1.0 && X[3] > 0.0) ? radius(X[3], N_gb) : 0.0);
}
