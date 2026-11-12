#include "NucleationModels.h"
#pragma once

#include <algorithm>
#include <cmath>

// # COARSENING: fixed correlations for the selected two-population reference.
namespace barani_coarsening
{
constexpr double pi = 3.14159265358979323846;
constexpr double kb = 1.380649e-23;
constexpr double atomic_volume = 4.09e-29;
constexpr double rho = 4e13;
constexpr double K = 1e6;
constexpr double gamma = 0.7;
// # COARSENING: selected equivalent transport radius; capture-cell radius remains distinct.
constexpr double gas_core_radius = 1.5 * 3.85e-10;
constexpr double capture_radius = 5 * 3.85e-10;
constexpr double radius_increment = 2e-10;
constexpr double radius_max = 2.5e-7;

struct PopulationSeed
{
    double number;
    bool initialized;
};

// # COARSENING: initialize at first irradiation; never replenish the evolved population.
inline PopulationSeed initializePopulation(double number, bool initialized, double fission_rate)
{
    if (!initialized && fission_rate > 0)
        return {std::max(number, K * rho), true};
    return {number, initialized};
}

inline double positive(double value) { return std::max(value, 0.); }
inline double volume(double radius) { return 4 * pi / 3 * radius * radius * radius; }
inline double radius(double volume) { return volume > 0 ? std::cbrt(3 * volume / (4 * pi)) : 0; }

// # COARSENING: Brearley hard-sphere diameter used by the Carnahan-Starling EOS.
inline double hardSphereDiameter(double temperature)
{
    return 4.45e-10 * (.8542 - .03996 * std::log(temperature / 231.2));
}

// # COARSENING: operational gas co-volume from the CS second virial, omega=4*v_HS.
inline double gasVolume(double temperature)
{
    const double d = hardSphereDiameter(temperature);
    return 2 * pi * d * d * d / 3;
}

// # COARSENING: gas co-volume and vacancy volume are distinct in Eq. (23).
inline double bubbleVolume(double atoms, double vacancies, double temperature)
{
    return atoms * gasVolume(temperature) + vacancies * atomic_volume;
}

// # COARSENING: Setyawan per-atom resolution; correlation radius is in nm.
inline double resolution(double radius_m, double fission_rate)
{
    const double x = radius_m * 1e9;
    return fission_rate * (9.49e-24 * std::exp(-.0707 * x) +
           (9.18e-23 - 9.49e-24) * std::exp(-.0371 * x * x) / (1 + 7.982 * x * x));
}

// # COARSENING: phi applies to bubble-number disappearance, not to the per-atom gas rate.
inline double numberCorrection(double atoms_per_bubble)
{
    return atoms_per_bubble > 1 ? 1 / (atoms_per_bubble - 1) : 0;
}

// # COARSENING: isolated-Xe core mobility, Murphy et al. (2015), Table 1, in SI.
// DOI: 10.1016/j.jnucmat.2015.09.007; extrapolated below the MD temperature range.
inline double gasCoreDiffusivity(double temperature)
{
    return 2e-7 * std::exp(-1.62 * 1.602176634e-19 / (kb * temperature));
}

// # COARSENING: Hart-type mean, Murphy et al. (2014), Eq. (5).
// DOI: 10.1016/j.jnucmat.2013.12.029. Volume weighting assumes equal mobile-Xe
// density in core and bulk; a=1.5b and its use in beta-prime are local closures.
inline double meanGasDiffusivity(double temperature, double bulk_diffusivity)
{
    const double dynamic_rho=nucleation_tests::state().rho_sites;
    if(nucleation_tests::connected() && dynamic_rho!=rho) {
        const double f=pi*std::pow(gas_core_radius,2)*dynamic_rho;
        return (1-f)*bulk_diffusivity+f*gasCoreDiffusivity(temperature);
    }

    const double core_fraction = pi * std::pow(gas_core_radius, 2) * rho;
    return (1 - core_fraction) * bulk_diffusivity + core_fraction * gasCoreDiffusivity(temperature);
}

// # COARSENING: selected effective coefficient in line capture only; native bulk transport is unchanged.
inline double lineTrapping(double temperature, double bulk_diffusivity)
{
    if(nucleation_tests::connected() && nucleation_tests::state().rho_sites<=0)return 0.;
    const double dynamic_rho=nucleation_tests::state().rho_sites;
    if(nucleation_tests::connected() && dynamic_rho!=rho)
        return 2*pi*meanGasDiffusivity(temperature,bulk_diffusivity)*dynamic_rho/
            (std::log(1/std::sqrt(pi*dynamic_rho)/capture_radius)-.6);

    return 2 * pi * meanGasDiffusivity(temperature, bulk_diffusivity) * rho /
           (std::log(1 / std::sqrt(pi * rho) / capture_radius) - .6);
}

// # COARSENING: published vacancy correlation, without scaling.
inline double vacancyPipe(double temperature)
{
    return .038 * std::exp(-1.84 / (8.617333262145e-5 * temperature));
}

inline double pressure(double atoms, double bubble_volume, double temperature)
{
    if (atoms <= 0 || bubble_volume <= 0 || temperature <= 0)
        return 0;
    const double d = hardSphereDiameter(temperature);
    const double y = std::clamp(pi * d * d * d * atoms / (6 * bubble_volume), 0., .74);
    return atoms * kb * temperature / bubble_volume * (1 + y + y * y - y * y * y) / std::pow(1 - y, 3);
}

inline double equilibrium(double bubble_radius, double stress_mpa)
{
    return bubble_radius > 0 ? 2 * gamma / bubble_radius - stress_mpa * 1e6 : 0;
}

inline double vacancyRate(double vacancies, double atoms, double number, double temperature, double stress)
{
    if (number <= 0 || atoms <= 0 || temperature <= 0)
        return 0;
    const double v = bubbleVolume(atoms, positive(vacancies), temperature);
    const double r = radius(v);
    const double delta = std::cbrt(3 / (4 * pi * number));
    const double psi = std::clamp(r / delta, 0., .999999);
    const double denominator = -std::pow(psi, 6) + 5 * psi * psi - 9 * psi + 5;
    if (denominator <= 0 || psi <= 0)
        return 0;
    const double z = 10 * psi * (1 + psi * psi * psi) / denominator;
    return 2 * pi * vacancyPipe(temperature) * delta / (kb * temperature * z) *
           positive(pressure(atoms, v, temperature) - equilibrium(r, stress));
}

// # COARSENING: absorption-only update with the retained 0.2-nm/call and 250-nm uptake limits.
inline double uptake(double vacancies, double atoms, double number, double temperature, double stress, double dt)
{
    const double old_r = radius(bubbleVolume(atoms, vacancies, temperature));
    if (dt <= 0 || pressure(atoms, bubbleVolume(atoms, vacancies, temperature), temperature) <= equilibrium(old_r, stress))
        return vacancies;
    const double rate = vacancyRate(vacancies, atoms, number, temperature, stress);
    if (rate <= 0)
        return vacancies;
    const double trial = vacancies + dt * rate;
    const double r = std::min(radius(bubbleVolume(atoms, trial, temperature)), std::min(old_r + radius_increment, radius_max));
    return positive((volume(r) - atoms * gasVolume(temperature)) / atomic_volume);
}

inline double partition(double old, double total, double bulk_trapping, double dislocation_trapping,
                        double resolution_rate, double dt)
{
    if (total <= 0)
        return 0;
    old = std::clamp(old, 0., total);
    const double beta = bulk_trapping + dislocation_trapping;
    if (beta <= 0 || dt <= 0)
        return old;
    const double target = total * dislocation_trapping / beta;
    return std::clamp(old + (target - old) * (-std::expm1(-(beta + resolution_rate) * dt)), 0., total);
}
}
