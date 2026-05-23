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
//  Version: 2.2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Constants.h"
#include "Simulation.h"
#include <algorithm>
#include <cmath>

// UN AD URANIUMNITRIDE
namespace
{
    constexpr double un_pi                = 3.14159265358979323846;
    constexpr double un_kb_ev             = 8.617333262e-5;
    constexpr double un_omega_fg          = 8.5e-29;
    constexpr double un_lattice_parameter = 4.889e-10;
    constexpr double un_gamma             = 1.11;
    constexpr double un_fn      = 5.5e-4;  // UN AD URANIUMNITRIDE: Rizk nominal = 1.0e-6; used/calibrated = 5.5e-4.
    constexpr double un_kd      = 3.0e5;   // UN AD URANIUMNITRIDE: Rizk nominal = 5.0e5; used/calibrated = 3.0e5.
    constexpr double un_rho_fab = 3.0e13;  // UN AD URANIUMNITRIDE: Rizk nominal constant rho_d = 3.0e13;
                                           // used/calibrated dynamic rho_d floor = 3.0e13.
    constexpr double un_rho_amp =
        7.5e14;  // UN AD URANIUMNITRIDE: Rizk nominal dynamic amplitude = 0.0; used/calibrated = 7.5e14.
    constexpr double un_rho_scale =
        0.10;  // UN AD URANIUMNITRIDE: Rizk nominal dynamic scale = 0.0; used/calibrated = 0.10.
    constexpr double un_rho_fc_percent =
        3.0;  // UN AD URANIUMNITRIDE: Rizk nominal burnup scale = not specified; used/calibrated = 3.0 FIMA percent.
    constexpr double un_rho_t_half =
        1550.0;  // UN AD URANIUMNITRIDE: Rizk nominal temperature half-point = not specified; used/calibrated = 1550 K.
    constexpr double un_rho_width =
        120.0;  // UN AD URANIUMNITRIDE: Rizk nominal temperature width = not specified; used/calibrated = 120 K.
    constexpr double un_rho_f_min = 0.08;  // UN AD URANIUMNITRIDE: Rizk nominal high-temperature floor factor = not
                                           // specified; used/calibrated = 0.08.
    constexpr double un_rho_cap =
        4.0e15;  // UN AD URANIUMNITRIDE: Rizk nominal dynamic cap = not specified; used/calibrated = 4.0e15.
    constexpr double un_dv_dislocation_scale =
        10.0;  // UN AD URANIUMNITRIDE: Rizk nominal dislocation vacancy scale = 1.0; used/calibrated = 10.0.
    constexpr double un_gf_ngf_areal_0 = 2.0e13;  // UN AD URANIUMNITRIDE: Rizk nominal grain-face areal density = not
                                                  // specified; used/calibrated = 2.0e13 m^-2.
    constexpr double un_gf_delta_gb = 4.0e-10;    // UN AD URANIUMNITRIDE: Rizk nominal grain-boundary thickness = not
                                                  // specified; used/calibrated = 4.0e-10 m.
    constexpr double un_gf_fc_sat =
        0.5;  // UN AD URANIUMNITRIDE: Rizk nominal grain-face saturation coverage = not specified; used/calibrated = 0.5.
    constexpr double un_gf_theta = 1.0297442586766543;  // UN AD URANIUMNITRIDE: Rizk nominal semidihedral angle = not
                                                        // specified; used/calibrated = 1.0297442586766543 rad.
    constexpr double un_gf_r0 = 2.42e-10;  // UN AD URANIUMNITRIDE: Rizk nominal grain-face seed radius = not specified;
                                           // used/calibrated = 2.42e-10 m.
    constexpr double un_gf_dv_gb_multiplier = 1.0e6;  // UN AD URANIUMNITRIDE: Rizk nominal grain-boundary vacancy
                                                      // multiplier = not specified; used/calibrated = 1.0e6.
    constexpr double un_first_pressure_factor =
        1.0 /
        3.0;  // UN AD URANIUMNITRIDE: Rizk nominal initial pressure factor = not specified; used/calibrated = 1.0/3.0.

    double positive(const double value)
    {
        return std::max(value, 0.0);
    }

    double safe_exp(const double value)
    {
        return std::exp(std::max(std::min(value, 700.0), -745.0));
    }

    double omega_matrix()
    {
        return std::pow(un_lattice_parameter, 3.0) / 4.0;
    }

    double uranium_atom_density()
    {
        return 4.0 / std::pow(un_lattice_parameter, 3.0);
    }

    double burnup_percent_from_time(const double time_s, const double fission_rate)
    {
        return 100.0 * fission_rate * time_s / uranium_atom_density();
    }

    double sphere_volume(const double radius)
    {
        return radius > 0.0 ? 4.0 / 3.0 * un_pi * std::pow(radius, 3.0) : 0.0;
    }

    double radius_from_volume(const double volume)
    {
        return volume > 0.0 ? std::pow(3.0 * volume / (4.0 * un_pi), 1.0 / 3.0) : 0.0;
    }

    double un_vacancy_diffusivity_d1(const double temperature)
    {
        if (temperature <= 0.0)
            return 0.0;
        return 1.122978768506e-02 * std::exp(-5.596873538604e+00 / (un_kb_ev * temperature));
    }

    double un_vacancy_diffusivity(const double temperature, const double fission_rate, const int option)
    {
        if (temperature <= 0.0)
            return 0.0;

        const double kbt = un_kb_ev * temperature;
        if (option == 0)
        {
            const double d1 =
                1.35e-2 * std::exp(-5.66 / kbt);  // UN AD URANIUMNITRIDE: Rizk nominal Dv1 = 1.35e-2*exp(-5.66/kT);
                                                  // used/calibrated = old notebook option.
            const double d2 = std::sqrt(std::max(fission_rate, 0.0)) * 4.6304523933553033e-29 *
                              safe_exp(-0.62 / kbt - 0.04 / (kbt * kbt));
            // UN AD URANIUMNITRIDE: Rizk nominal for U vacancies A20 = 1.32e-19, B21 = -0.62, B22 = -0.04;
            // used/calibrated old vacancy refit A20 = 4.6304523933553033e-29, B21 = -0.62, B22 = -0.04.
            return d1 + d2;
        }
        if (option == 1)
        {
            const double d1 =
                1.35e-2 * std::exp(-5.66 / kbt);  // UN AD URANIUMNITRIDE: Rizk nominal Dv1 = 1.35e-2*exp(-5.66/kT);
                                                  // used/calibrated = Rizk-2025 A20-only refit option.
            const double d2 = std::sqrt(std::max(fission_rate, 0.0)) * 1.386341579723e-28 *
                              safe_exp(-0.62 / kbt - 0.04 / (kbt * kbt));
            // UN AD URANIUMNITRIDE: Rizk nominal for U vacancies A20 = 1.32e-19, B21 = -0.62, B22 = -0.04;
            // used/calibrated A20-only refit A20 = 1.386341579723e-28, B21 = -0.62, B22 = -0.04.
            return d1 + d2;
        }

        const double d1 = un_vacancy_diffusivity_d1(temperature);
        const double d2 = std::sqrt(std::max(fission_rate, 0.0)) * 7.805188680989e-28 *
                          safe_exp(-9.932675113163e-01 / kbt - 2.082395503235e-02 / (kbt * kbt));
        // UN AD URANIUMNITRIDE: Rizk nominal for U vacancies A20 = 1.32e-19, B21 = -0.62, B22 = -0.04; used/calibrated
        // notebook-8 full refit A20 = 7.805188680989e-28, B21 = 9.932675113163e-01, B22 = 2.082395503235e-02.
        return d1 + d2;
    }

    double
    un_dynamic_rho(const double temperature, const double burnup_percent, const int option, const double constant_rho)
    {
        if (option == 0)
            return std::max(constant_rho, 1.0e10);

        const double burnup_part = 1.0 - std::exp(-std::max(burnup_percent, 0.0) / un_rho_fc_percent);
        const double ft =
            un_rho_f_min + (1.0 - un_rho_f_min) / (1.0 + std::exp((temperature - un_rho_t_half) / un_rho_width));
        const double rho = un_rho_fab + un_rho_scale * un_rho_amp * burnup_part * ft;
        return std::min(std::max(rho, 1.0e10), un_rho_cap);
    }

    double phi_population(const double gas, const double number_density)
    {
        if (gas <= 0.0 || number_density <= 0.0)
            return 0.0;
        const double atoms_per_bubble = gas / number_density;
        return atoms_per_bubble > 1.0 ? 1.0 / (atoms_per_bubble - 1.0) : 0.0;
    }

    double coalescence_lambda(const double volume, const double number_density)
    {
        const double xi = std::min(std::max(volume * number_density, 0.0), 0.999999);
        return (2.0 - xi) / (2.0 * std::pow(1.0 - xi, 3.0));
    }

    double pressure_internal(const double temperature, const double gas, const double vacancies)
    {
        if (gas <= 0.0)
            return 0.0;
        if (vacancies <= 0.0)
            return 1.0e30;
        return boltzmann_constant * temperature * gas / (vacancies * omega_matrix());
    }

    double pressure_equilibrium(const double radius, const double hydrostatic_stress_pa)
    {
        if (radius <= 0.0)
            return 0.0;
        return 2.0 * un_gamma / std::max(radius, 1.0e-15) - hydrostatic_stress_pa;
    }

    double wigner_seitz_delta(const double number_density)
    {
        return std::pow(3.0 / (4.0 * un_pi * std::max(number_density, 1.0)), 1.0 / 3.0);
    }

    double zeta_geometry(const double radius, const double number_density)
    {
        const double delta = wigner_seitz_delta(number_density);
        const double psi   = std::max(radius / delta, 1.0e-12);
        if (!std::isfinite(psi))
            return 1.0e300;
        if (psi >= 1.0)
            return psi > 1.0e75 ? 1.0e300 : std::max(10.0 * psi * (1.0 + std::pow(psi, 3.0)) / 1.0e-30, 1.0e-30);
        const double den = std::max(-std::pow(psi, 6.0) + 5.0 * std::pow(psi, 2.0) - 9.0 * psi + 5.0, 1.0e-30);
        return std::max(10.0 * psi * (1.0 + std::pow(psi, 3.0)) / den, 1.0e-30);
    }

    double vacancy_implicit_step(const double temperature,
                                 const double hydrostatic_stress_pa,
                                 const double diffusivity,
                                 const double radius,
                                 const double number_density,
                                 const double gas,
                                 const double old_vacancies,
                                 const double dt)
    {
        if (dt <= 0.0 || number_density <= 0.0 || gas <= 0.0)
            return old_vacancies;

        const double update_radius = radius > 0.0 ? radius : radius_from_volume(un_omega_fg * gas / number_density);
        if (update_radius <= 0.0)
            return old_vacancies;

        const double p_eq  = pressure_equilibrium(update_radius, hydrostatic_stress_pa);
        const double p_old = pressure_internal(temperature, gas, old_vacancies);
        if (p_old <= p_eq)
            return old_vacancies;

        const double delta = wigner_seitz_delta(number_density);
        const double zeta  = zeta_geometry(update_radius, number_density);
        const double a     = 2.0 * un_pi * diffusivity * delta * number_density /
                         (boltzmann_constant * temperature * std::max(zeta, 1.0e-300));
        const double c         = boltzmann_constant * temperature * gas / omega_matrix();
        const double b         = old_vacancies - dt * a * p_eq;
        const double disc      = std::max(b * b + 4.0 * dt * a * c, 0.0);
        const double sqrt_disc = std::sqrt(disc);
        double       n_new     = 0.0;
        if (b >= 0.0)
            n_new = 0.5 * (b + sqrt_disc);
        else
        {
            const double den = sqrt_disc - b;
            n_new            = den <= 0.0 ? 0.0 : (2.0 * dt * a * c) / den;
        }
        return std::max(n_new, old_vacancies);
    }

    double seed_vacancies_for_pressure_factor(const double temperature,
                                              const double hydrostatic_stress_pa,
                                              const double gas_atoms_per_bubble,
                                              const double pressure_factor)
    {
        const double ng       = std::max(gas_atoms_per_bubble, 1.0e-30);
        const double factor   = std::max(pressure_factor, 1.0e-12);
        double       lo       = 1.0e-30;
        double       hi       = 1.0;
        auto         residual = [&](const double nv)
        {
            const double volume = un_omega_fg * ng + omega_matrix() * nv;
            const double radius = radius_from_volume(std::max(volume, 1.0e-300));
            const double p_int  = boltzmann_constant * temperature * ng / (nv * omega_matrix());
            const double p_eq   = pressure_equilibrium(radius, hydrostatic_stress_pa);
            return p_int - factor * p_eq;
        };

        for (int i = 0; i < 300 && residual(hi) >= 0.0; ++i)
            hi *= 2.0;
        for (int i = 0; i < 160; ++i)
        {
            const double mid = std::sqrt(lo * hi);
            if (residual(mid) > 0.0)
                lo = mid;
            else
                hi = mid;
        }
        return hi;
    }

    double gf_lens_factor()
    {
        const double c = std::cos(un_gf_theta);
        return 1.0 - 1.5 * c + 0.5 * std::pow(c, 3.0);
    }

    double gf_volume_from_radius(const double radius)
    {
        return radius > 0.0 ? 4.0 / 3.0 * un_pi * std::pow(radius, 3.0) * gf_lens_factor() : 0.0;
    }

    double gf_radius_from_volume(const double volume)
    {
        return volume > 0.0 ? std::pow(3.0 * volume / (4.0 * un_pi * std::max(gf_lens_factor(), 1.0e-30)), 1.0 / 3.0)
                            : 0.0;
    }

    double gf_area_from_radius(const double radius)
    {
        return un_pi * std::pow(std::max(radius, 0.0), 2.0) * std::pow(std::sin(un_gf_theta), 2.0);
    }

    double gf_zeta_from_coverage(const double coverage)
    {
        const double fc = std::min(std::max(coverage, 1.0e-12), 0.999999999);
        return -(((3.0 - fc) * (1.0 - fc) + 2.0 * std::log(fc)) / 4.0);
    }
}  // namespace
// END UN AD URANIUMNITRIDE

void Simulation::IntraGranularBubbleBehavior()
{
    // Model declaration
    Model model_;

    model_.setName("Intragranular bubble behavior");

    std::string         reference;
    std::vector<double> parameter;

    switch (int(input_variable["iIntraGranularBubbleBehavior"].getValue()))
    {
        case 0:
        {
            reference += ": Constant bubble concentration and radius.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(7.0e23);
            sciantix_variable["Intragranular bubble radius"].setInitialValue(1.0e-9);

            sciantix_variable["Intragranular bubble concentration"].setFinalValue(7.0e23);
            sciantix_variable["Intragranular bubble radius"].setFinalValue(1.0e-9);

            parameter.push_back(0.);
            parameter.push_back(0.);

            break;
        }

        case 1:
        {
            reference += ": Pizzocri et al., JNM, 502 (2018) 323-330.";

            parameter.push_back(sciantix_system[0].getResolutionRate());
            parameter.push_back(sciantix_system[0].getNucleationRate());

            break;
        }

        case 2:
        {
            reference += "White and Tucker, JNM, 118 (1983), 1-38.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(
                1.52e+27 / history_variable["Temperature"].getFinalValue() - 3.3e+23);
            parameter.push_back(0.0);
            parameter.push_back(0.0);

            break;
        }

            // case 3:
            // AD
            // bolle + piccole + bolle medie

        case 3:
        {
            reference += "Case specific for annealing experiments and helium intragranular behaviour.";

            if (physics_variable["Time step"].getFinalValue() > 0.0)
                parameter.push_back((1.0 / sciantix_variable["Intragranular similarity ratio"].getFinalValue() - 1.0) /
                                    physics_variable["Time step"].getFinalValue());
            else
                parameter.push_back(0.);

            parameter.push_back(0.);

            break;
        }

        // UN AD URANIUMNITRIDE
        case 5:
        {
            reference += "UN notebook 8 full intragranular plus grain-face bubble/FGR model.";
            parameter.push_back(0.0);
            parameter.push_back(0.0);
            break;
        }
            // END UN AD URANIUMNITRIDE

        case 99:
        {
            reference += "No intragranular bubbles.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(0.0);
            sciantix_variable["Intragranular bubble radius"].setInitialValue(0.0);
            sciantix_variable["Intragranular atoms per bubble"].setInitialValue(0.0);

            sciantix_variable["Intragranular bubble concentration"].setFinalValue(0.0);
            sciantix_variable["Intragranular bubble radius"].setFinalValue(0.0);
            sciantix_variable["Intragranular atoms per bubble"].setFinalValue(0.0);

            parameter.push_back(0.);
            parameter.push_back(0.);

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__,
                                  "iIntraGranularBubbleBehavior",
                                  int(input_variable["iIntraGranularBubbleBehavior"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    // dN / dt = - getParameter().at(0) * N + getParameter().at(1)
    sciantix_variable["Intragranular bubble concentration"].setFinalValue(
        solver.Decay(sciantix_variable["Intragranular bubble concentration"].getInitialValue(),
                     model["Intragranular bubble behavior"].getParameter().at(0),
                     model["Intragranular bubble behavior"].getParameter().at(1),
                     physics_variable["Time step"].getFinalValue()));

    // Atom per bubbles and bubble radius
    for (auto& system : sciantix_system)
    {
        if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            if (sciantix_variable["Intragranular bubble concentration"].getFinalValue() > 0.0)
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].setFinalValue(
                    sciantix_variable[system.getGasName() + " in intragranular bubbles"].getFinalValue() /
                    sciantix_variable["Intragranular bubble concentration"].getFinalValue());

            else
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].setFinalValue(0.0);

            sciantix_variable["Intragranular bubble volume"].addValue(
                system.getVolumeInLattice() *
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].getFinalValue());
        }
    }

    // Intragranular bubble radius
    sciantix_variable["Intragranular bubble radius"].setFinalValue(
        0.620350491 * pow(sciantix_variable["Intragranular bubble volume"].getFinalValue(), (1.0 / 3.0)));

    // Intragranular gaseous swelling
    // 4/3 pi N R^3
    sciantix_variable["Intragranular gas bubble swelling"].setFinalValue(
        4.188790205 * pow(sciantix_variable["Intragranular bubble radius"].getFinalValue(), 3) *
        sciantix_variable["Intragranular bubble concentration"].getFinalValue());

    // UN AD URANIUMNITRIDE
    if (int(input_variable["iIntraGranularBubbleBehavior"].getValue()) == 5)
    {
        const double dt                    = physics_variable["Time step"].getFinalValue();
        const double temperature           = history_variable["Temperature"].getFinalValue();
        const double fission_rate          = history_variable["Fission rate"].getFinalValue();
        const double hydrostatic_stress_pa = history_variable["Hydrostatic stress"].getFinalValue() * 1.0e6;
        const int    rho_option            = int(input_variable["iUNDislocationDensity"].getValue());
        const int    vacancy_option        = int(input_variable["iUNVacancyDiffusivity"].getValue());
        const int    intergranular_option  = int(input_variable["iUNInterGranularBehavior"].getValue());

        const double time_old_s = std::max(history_variable["Time"].getFinalValue() * 3600.0 - dt, 0.0);
        const double time_new_s = history_variable["Time"].getFinalValue() * 3600.0;
        const double burnup_old = burnup_percent_from_time(time_old_s, fission_rate);
        const double burnup_new = burnup_percent_from_time(time_new_s, fission_rate);

        const double rho_old =
            un_dynamic_rho(temperature, burnup_old, rho_option, matrices["UN"].getDislocationDensity());
        const double rho_next =
            un_dynamic_rho(temperature, burnup_new, rho_option, matrices["UN"].getDislocationDensity());

        double c  = positive(sciantix_variable["Xe in intragranular solution"].getFinalValue());
        double mb = positive(sciantix_variable["Xe in intragranular bubbles"].getFinalValue());
        double md = positive(sciantix_variable["Xe in dislocation bubbles"].getFinalValue());

        double Nb     = positive(sciantix_variable["Intragranular bubble concentration"].getInitialValue());
        double Nd     = positive(sciantix_variable["Dislocation bubble concentration"].getInitialValue());
        double Rb_old = positive(sciantix_variable["Intragranular bubble radius"].getInitialValue());
        double Rd_old = positive(sciantix_variable["Dislocation bubble radius"].getInitialValue());
        double Vb_old = sphere_volume(Rb_old);
        double Vd_old = sphere_volume(Rd_old);

        if (Nd <= 0.0)
            Nd = un_kd * rho_old;

        double nvb = positive(sciantix_variable["Bulk vacancies per bubble"].getInitialValue()) * Nb;
        double nvd = positive(sciantix_variable["Dislocation vacancies per bubble"].getInitialValue()) * Nd;

        const double Dv    = un_vacancy_diffusivity(temperature, fission_rate, vacancy_option);
        const double Dv_d  = Dv * un_dv_dislocation_scale;
        const double b_b   = sciantix_system[0].getResolutionRateIntra();
        const double nu_b  = positive(sciantix_variable["UN bulk nucleation rate"].getFinalValue());
        const double phi_b = phi_population(mb, Nb);

        if (dt > 0.0)
            Nb = std::max((Nb + dt * std::max(nu_b, 0.0)) / (1.0 + dt * b_b * phi_b), 0.0);

        if (mb > 0.0 && Nb > 0.0 && nvb <= 0.0)
        {
            const double nv_per_bubble = seed_vacancies_for_pressure_factor(
                temperature, hydrostatic_stress_pa, mb / Nb, un_first_pressure_factor);
            nvb = nv_per_bubble * Nb;
        }
        if (md > 0.0 && Nd > 0.0 && nvd <= 0.0)
        {
            const double nv_per_bubble = seed_vacancies_for_pressure_factor(
                temperature, hydrostatic_stress_pa, md / Nd, un_first_pressure_factor);
            nvd = nv_per_bubble * Nd;
        }

        const double nvb_old = nvb;
        const double nvd_old = nvd;
        nvb                  = vacancy_implicit_step(temperature, hydrostatic_stress_pa, Dv, Rb_old, Nb, mb, nvb, dt);
        nvd                  = vacancy_implicit_step(temperature, hydrostatic_stress_pa, Dv_d, Rd_old, Nd, md, nvd, dt);

        const double dmb_dt =
            dt > 0.0 ? (mb - positive(sciantix_variable["Xe in intragranular bubbles"].getInitialValue())) / dt : 0.0;
        const double dmd_dt =
            dt > 0.0 ? (md - positive(sciantix_variable["Xe in dislocation bubbles"].getInitialValue())) / dt : 0.0;
        const double dnvb_dt = dt > 0.0 ? (nvb - nvb_old) / dt : 0.0;
        const double dnvd_dt = dt > 0.0 ? (nvd - nvd_old) / dt : 0.0;

        const double Vb_growth =
            Nb > 0.0 ? std::max(Vb_old + dt * (un_omega_fg / Nb * dmb_dt + omega_matrix() / Nb * dnvb_dt), 0.0) : 0.0;
        const double dVd_growth_dt = Nd > 0.0 ? un_omega_fg / Nd * dmd_dt + omega_matrix() / Nd * dnvd_dt : 0.0;
        const double Vd_growth     = Nd > 0.0 ? std::max(Vd_old + dt * dVd_growth_dt, 0.0) : 0.0;

        const double lambda_d     = coalescence_lambda(Vd_old, Nd);
        const double dVd_positive = std::max(Vd_growth - Vd_old, 0.0);
        if (dVd_positive > 0.0 && Nd > 0.0)
            Nd = Nd / (1.0 + 4.0 * lambda_d * Nd * dVd_positive);

        if (rho_option != 0 && rho_old > 0.0 && Nd > 0.0)
            Nd = std::max(Nd * (rho_next / rho_old), 0.0);

        const double Vb = Nb > 0.0 ? (un_omega_fg * mb + omega_matrix() * nvb) / Nb : 0.0;
        const double Vd = Nd > 0.0 ? (un_omega_fg * md + omega_matrix() * nvd) / Nd : 0.0;
        const double Rb = radius_from_volume(Vb);
        const double Rd = radius_from_volume(Vd);

        const double p_b        = pressure_internal(temperature, mb, nvb);
        const double p_d        = pressure_internal(temperature, md, nvd);
        const double p_b_eq     = pressure_equilibrium(Rb, hydrostatic_stress_pa);
        const double p_d_eq     = pressure_equilibrium(Rd, hydrostatic_stress_pa);
        const double swelling_b = Nb * Vb;
        const double swelling_d = Nd * Vd;

        // UN AD URANIUMNITRIDE
        double q_gf      = positive(sciantix_variable["UN grain-face gas"].getInitialValue());
        double q_rel     = positive(sciantix_variable["UN released gas"].getInitialValue());
        double qv_gf     = positive(sciantix_variable["Grain-face vacancies per bubble"].getInitialValue());
        double Ngf_areal = positive(sciantix_variable["Grain-face bubble concentration"].getInitialValue());
        if (Ngf_areal <= 0.0)
            Ngf_areal = un_gf_ngf_areal_0;

        const double Ngf_vol_old = 3.0 * Ngf_areal / (2.0 * sciantix_variable["Grain radius"].getFinalValue());
        if (qv_gf <= 0.0 && Ngf_vol_old > 0.0)
            qv_gf = gf_volume_from_radius(un_gf_r0) / omega_matrix();
        double qv_gf_total = qv_gf * Ngf_vol_old;

        const double qgb_target = std::max(sciantix_variable["Xe produced"].getFinalValue() -
                                               sciantix_variable["Xe decayed"].getFinalValue() - (c + mb + md),
                                           0.0);
        const double qgb_before = q_gf + q_rel;
        const double dq_to_gb   = std::max(qgb_target - qgb_before, 0.0);

        if (intergranular_option != 0 && dt > 0.0)
        {
            const double grain_radius = std::max(sciantix_variable["Grain radius"].getFinalValue(), 1.0e-30);
            double       Ngf_vol      = 3.0 * Ngf_areal / (2.0 * grain_radius);
            double       ng           = Ngf_vol > 0.0 ? q_gf / Ngf_vol : 0.0;
            double       nv           = Ngf_vol > 0.0 ? qv_gf_total / Ngf_vol : 0.0;
            double       Vgf          = un_omega_fg * ng + omega_matrix() * nv;
            double       Rgf          = Vgf > 0.0 ? gf_radius_from_volume(Vgf) : un_gf_r0;
            double       Agf_old      = gf_area_from_radius(Rgf);

            q_gf += dq_to_gb;
            ng                   = Ngf_vol > 0.0 ? q_gf / Ngf_vol : 0.0;
            nv                   = Ngf_vol > 0.0 ? qv_gf_total / Ngf_vol : 0.0;
            Vgf                  = un_omega_fg * ng + omega_matrix() * nv;
            Rgf                  = gf_radius_from_volume(Vgf);
            double       Agf     = gf_area_from_radius(Rgf);
            double       Fc      = Ngf_areal * Agf;
            double       zeta_gf = gf_zeta_from_coverage(Fc);
            const double p_gf =
                ng > 0.0 && nv > 0.0 ? boltzmann_constant * temperature * ng / (nv * omega_matrix()) : 0.0;
            const double p_gf_eq = Rgf > 0.0 ? pressure_equilibrium(Rgf, hydrostatic_stress_pa) : 0.0;
            const double p_drive = std::max(p_gf - p_gf_eq, 0.0);
            const double Dv_gb   = un_gf_dv_gb_multiplier * un_vacancy_diffusivity_d1(temperature);

            if (Dv_gb > 0.0 && zeta_gf > 0.0 && Ngf_vol > 0.0)
            {
                const double dnv_dt =
                    2.0 * un_pi * Dv_gb * un_gf_delta_gb / (boltzmann_constant * temperature * zeta_gf) * p_drive;
                qv_gf_total = std::max(qv_gf_total + dt * Ngf_vol * dnv_dt, 0.0);
            }

            ng                 = Ngf_vol > 0.0 ? q_gf / Ngf_vol : 0.0;
            nv                 = Ngf_vol > 0.0 ? qv_gf_total / Ngf_vol : 0.0;
            Vgf                = un_omega_fg * ng + omega_matrix() * nv;
            Rgf                = gf_radius_from_volume(Vgf);
            Agf                = gf_area_from_radius(Rgf);
            const double dA_dt = std::max((Agf - Agf_old) / dt, 0.0);
            Fc                 = Ngf_areal * Agf;

            if (Fc < un_gf_fc_sat && dA_dt > 0.0 && Ngf_areal > 0.0)
            {
                const double dNgf_dt =
                    -(6.0 * Ngf_areal * Ngf_areal / (3.0 + 4.0 * Ngf_areal * std::max(Agf, 0.0))) * dA_dt;
                Ngf_areal = std::max(Ngf_areal + dt * dNgf_dt, 1.0e-30);
            }

            Ngf_vol = 3.0 * Ngf_areal / (2.0 * grain_radius);
            ng      = Ngf_vol > 0.0 ? q_gf / Ngf_vol : 0.0;
            nv      = Ngf_vol > 0.0 ? qv_gf_total / Ngf_vol : 0.0;
            Vgf     = un_omega_fg * ng + omega_matrix() * nv;
            Rgf     = gf_radius_from_volume(Vgf);
            Agf     = gf_area_from_radius(Rgf);
            Fc      = Ngf_areal * Agf;

            if (Fc >= un_gf_fc_sat && Ngf_areal > 0.0 && Ngf_vol > 0.0)
            {
                const double A_sat = un_gf_fc_sat / Ngf_areal;
                const double R_sat =
                    std::sqrt(A_sat / (un_pi * std::max(std::pow(std::sin(un_gf_theta), 2.0), 1.0e-30)));
                const double V_sat = gf_volume_from_radius(R_sat);
                double       p_ref =
                    ng > 0.0 && nv > 0.0 ? boltzmann_constant * temperature * ng / (nv * omega_matrix()) : 0.0;
                if (!std::isfinite(p_ref) || p_ref <= 0.0)
                    p_ref = pressure_equilibrium(R_sat, hydrostatic_stress_pa);
                p_ref = std::max(p_ref, 1.0e-30);
                const double ng_allowed =
                    std::max(V_sat / (un_omega_fg + boltzmann_constant * temperature / p_ref), 0.0);
                const double nv_allowed =
                    std::max(boltzmann_constant * temperature * ng_allowed / (p_ref * omega_matrix()), 0.0);
                const double qgf_allowed = Ngf_vol * ng_allowed;
                if (q_gf > qgf_allowed)
                {
                    q_rel += q_gf - qgf_allowed;
                    q_gf = qgf_allowed;
                }
                qv_gf_total = std::min(std::max(qv_gf_total, 0.0), Ngf_vol * nv_allowed);
            }
        }

        const double grain_radius = std::max(sciantix_variable["Grain radius"].getFinalValue(), 1.0e-30);
        const double Ngf_vol      = 3.0 * Ngf_areal / (2.0 * grain_radius);
        const double ng_gf        = Ngf_vol > 0.0 ? q_gf / Ngf_vol : 0.0;
        const double nv_gf        = Ngf_vol > 0.0 ? qv_gf_total / Ngf_vol : 0.0;
        const double Vgf          = un_omega_fg * ng_gf + omega_matrix() * nv_gf;
        const double Rgf          = Vgf > 0.0 ? gf_radius_from_volume(Vgf) : un_gf_r0;
        const double Agf          = gf_area_from_radius(Rgf);
        const double Fc           = Ngf_areal * Agf;
        const double swelling_gf  = Ngf_vol * Vgf;
        // END UN AD URANIUMNITRIDE

        sciantix_variable["Intragranular bubble concentration"].setFinalValue(Nb);
        sciantix_variable["Intragranular bubble radius"].setFinalValue(Rb);
        sciantix_variable["Intragranular bubble volume"].setFinalValue(Vb);
        sciantix_variable["Intragranular Xe atoms per bubble"].setFinalValue(Nb > 0.0 ? mb / Nb : 0.0);
        sciantix_variable["Intragranular atoms per bubble"].setFinalValue(Nb > 0.0 ? mb / Nb : 0.0);
        sciantix_variable["Bulk vacancies per bubble"].setFinalValue(Nb > 0.0 ? nvb / Nb : 0.0);
        sciantix_variable["Intragranular bulk gas bubble swelling"].setFinalValue(swelling_b);
        sciantix_variable["Dislocation bubble concentration"].setFinalValue(Nd);
        sciantix_variable["Dislocation bubble radius"].setFinalValue(Rd);
        sciantix_variable["Dislocation bubble volume"].setFinalValue(Vd);
        sciantix_variable["Dislocation gas atoms per bubble"].setFinalValue(Nd > 0.0 ? md / Nd : 0.0);
        sciantix_variable["Dislocation vacancies per bubble"].setFinalValue(Nd > 0.0 ? nvd / Nd : 0.0);
        sciantix_variable["Dislocation gas bubble swelling"].setFinalValue(swelling_d);
        sciantix_variable["Bulk bubble pressure"].setFinalValue(std::isfinite(p_b) ? p_b / 1.0e6 : 0.0);
        sciantix_variable["Bulk bubble equilibrium pressure"].setFinalValue(p_b_eq / 1.0e6);
        sciantix_variable["Dislocation bubble pressure"].setFinalValue(std::isfinite(p_d) ? p_d / 1.0e6 : 0.0);
        sciantix_variable["Dislocation bubble equilibrium pressure"].setFinalValue(p_d_eq / 1.0e6);
        sciantix_variable["Dislocation density"].setFinalValue(rho_next);
        sciantix_variable["UN gas to grain boundary diagnostic"].setFinalValue(q_gf + q_rel);
        sciantix_variable["Grain-face bubble concentration"].setFinalValue(Ngf_areal);
        sciantix_variable["Grain-face atoms per bubble"].setFinalValue(ng_gf);
        sciantix_variable["Grain-face vacancies per bubble"].setFinalValue(nv_gf);
        sciantix_variable["Grain-face bubble radius"].setFinalValue(Rgf);
        sciantix_variable["Grain-face bubble area"].setFinalValue(Agf);
        sciantix_variable["Grain-face bubble volume"].setFinalValue(Vgf);
        sciantix_variable["Grain-face fractional coverage"].setFinalValue(Fc);
        sciantix_variable["Grain-face gas swelling"].setFinalValue(swelling_gf);
        sciantix_variable["UN grain-face gas"].setFinalValue(q_gf);
        sciantix_variable["UN released gas"].setFinalValue(q_rel);
        sciantix_variable["UN fission gas release"].setFinalValue(
            sciantix_variable["Xe produced"].getFinalValue() > 0.0
                ? q_rel / sciantix_variable["Xe produced"].getFinalValue()
                : 0.0);
        sciantix_variable["UN total gas swelling"].setFinalValue(swelling_b + swelling_d + swelling_gf);
        sciantix_variable["Intragranular gas bubble swelling"].setFinalValue(swelling_b + swelling_d);
        sciantix_variable["Intergranular gas swelling"].setFinalValue(swelling_gf);
        sciantix_variable["Xe at grain boundary"].setFinalValue(q_gf);
        sciantix_variable["Xe released"].setFinalValue(q_rel);
        sciantix_variable["Fission gas release"].setFinalValue(
            sciantix_variable["Xe produced"].getFinalValue() > 0.0
                ? q_rel / sciantix_variable["Xe produced"].getFinalValue()
                : 0.0);
    }
    // END UN AD URANIUMNITRIDE

    if (sciantix_variable["He in intragranular bubbles"].getInitialValue() > 0.0)
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(
            sqrt(sciantix_variable["He in intragranular bubbles"].getFinalValue() /
                 sciantix_variable["He in intragranular bubbles"].getInitialValue()));
    else
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(0.0);
}
