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
//  Authors: D. Pizzocri, G. Nicodemo, G. Zullo.                                    //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#pragma once

#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <string>

#include "DislocationDensity.h"

// Kernels of the two-population intragranular bubble model (iIntraGranularBubbleBehavior = 5): bulk bubbles and
// bubbles on dislocations (Barani et al., JNM 538 (2020) 152195, revised by Nicodemo et al.).

// # COARSENING: experimental birth laws; none is enabled in the frozen base.
namespace intragranular_nucleation {
constexpr double pi=3.14159265358979323846;
constexpr double initial_density=dislocation_density::initial_density, K=1e6, embryo_atoms=2;
constexpr double core_radius=5*3.85e-10;
constexpr double oxide_to_heavy_metal=dislocation_density::oxide_to_heavy_metal;
inline const std::string& mode() {
    static const std::string value=[](){const char* p=std::getenv("SCIANTIX_NUCLEATION");return std::string(p?p:"vesh_matrix_network");}();
    return value;
}
inline bool capture() { return mode().find("capture")!=std::string::npos; }
inline bool stableCapture() { return mode().find("stable")!=std::string::npos; }
inline bool connected() { return mode().find("network")!=std::string::npos || mode()=="geometry_only"; }
inline bool linear() { return mode().find("linear")!=std::string::npos; }

inline double densityIncrement(double old_b,double new_b,double T,double F,double dt) {
    if(mode()=="control" || F<=0 || dt<=0 || new_b<=old_b)return 0.;
    return std::max(0.,dislocation_density::correlation(new_b*oxide_to_heavy_metal,T)-dislocation_density::correlation(old_b*oxide_to_heavy_metal,T));
}
struct State { double rho_sites=initial_density, cumulative_births=0; };
inline State& state(){static State s;return s;}
struct Packet {
    double increment=0, requested=0, born=0, gas=0;
    double old_number=0, birth_number=0, old_vacancy_total=0, birth_vacancy_total=0;
    double old_bulk_number=0, mode_balance_error=0;
};
// Diagnostic occupation distribution: Poisson with mean gas/bulk bubble.
// This is an explicit closure, not a measured cluster distribution in UO2.
inline double stableProbability(double mean) {
    if(mean<=0)return 0;
    if(mean<1e-3)return mean*mean*(.5-mean/3+mean*mean/8-mean*mean*mean/30);
    return -std::expm1(-mean)-mean*std::exp(-mean);
}
inline double stableMean(double mean) {
    const double p=stableProbability(mean);
    return p>0?mean*(-std::expm1(-mean))/p:embryo_atoms;
}
inline double requestedBirths(double increment,double bulk_number,double bulk_radius,double mean_gas) {
    if(mode()=="geometry_only")return 0;
    double per_length=K;
    if(capture()) per_length=std::min(K,pi*std::pow(core_radius+bulk_radius,2)*bulk_number*
                                   (stableCapture()?stableProbability(mean_gas):1.));
    return std::max(0.,increment*per_length);
}
inline double mixedVacancies(double old_number,double old_vacancies,double added_number) {
    return added_number>0 ? old_number*old_vacancies/(old_number+added_number) : old_vacancies;
}
inline void trace(double time,double T,double burnup,double pipe_density,const Packet& p,
                  double final_number,double final_radius) {
    const char* filename=std::getenv("SCIANTIX_NUCLEATION_TRACE");if(!filename)return;
    static bool first=true;
    static std::ofstream file(filename);
    if(first)file<<"time_h,T_K,B_GWd_tHM,rho_sites_m2,rho_pipe_m2,delta_rho_m2,requested_births_m3,births_m3,cumulative_births_m3,gas_transfer_m3,Nd_before_m3,Nd_after_birth_m3,vacancies_before_m3,vacancies_after_birth_m3,Nb_before_m3,mode_balance_error,Nd_final_m3,R_final_m\n";
    first=false;
    file<<std::setprecision(17)<<time<<','<<T<<','<<burnup*oxide_to_heavy_metal<<','<<state().rho_sites<<','
        <<pipe_density<<','<<p.increment<<','<<p.requested<<','<<p.born<<','<<state().cumulative_births<<','
        <<p.gas<<','<<p.old_number<<','<<p.birth_number<<','<<p.old_vacancy_total<<','<<p.birth_vacancy_total<<','
        <<p.old_bulk_number<<','<<p.mode_balance_error<<','<<final_number<<','<<final_radius<<'\n';
}
}

// # COARSENING: fixed correlations for the selected two-population reference.
namespace intragranular_coarsening
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
constexpr double radius_max = 2.5e-7;
// # COARSENING: vacancy supply along dislocations only above T_v. White et al. (2006): no second population after
// base irradiation (TEM 4001A, 4070A), only 4-7 nm dislocation bubbles at 1490-1610 C after ramps, while near the
// bore the measured swelling needs >= 40-86% vacancies. A hard threshold: with the published D_v the uptake is
// ~1e13 vacancies/s at 1200 K, so any smooth reduction still fills the bubbles during base irradiation.
// Calibrated value of the previous branch (with D_d = D and K = 1e5); to be rechecked on this model.
constexpr double vacancy_activation_temperature = 1873.0;

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
    const double dynamic_rho=intragranular_nucleation::state().rho_sites;
    if(intragranular_nucleation::connected() && dynamic_rho!=rho) {
        const double f=pi*std::pow(gas_core_radius,2)*dynamic_rho;
        return (1-f)*bulk_diffusivity+f*gasCoreDiffusivity(temperature);
    }

    const double core_fraction = pi * std::pow(gas_core_radius, 2) * rho;
    return (1 - core_fraction) * bulk_diffusivity + core_fraction * gasCoreDiffusivity(temperature);
}

// # COARSENING: selected effective coefficient in line capture only; native bulk transport is unchanged.
// # COARSENING: line sink beta' = 2 pi D rho / (ln(r_ws / r_cap) - 3/5) with the matrix diffusivity D (as Pastore):
// the dissolved gas reaches the line through the matrix, so the capture rate is set by D; core diffusion moves the
// captured gas along the line. The Hart mean (meanGasDiffusivity) gave D_d ~ 1000 D at 1300 K, the lines took the
// gas during base irradiation and the bulk bubbles stayed empty, against White et al. (2006) TEM (4001A, 4070A).
inline double lineTrapping(double temperature, double bulk_diffusivity)
{
    (void)temperature;
    if(intragranular_nucleation::connected() && intragranular_nucleation::state().rho_sites<=0)return 0.;
    const double dynamic_rho=intragranular_nucleation::state().rho_sites;
    if(intragranular_nucleation::connected() && dynamic_rho!=rho)
        return 2*pi*bulk_diffusivity*dynamic_rho/
            (std::log(1/std::sqrt(pi*dynamic_rho)/capture_radius)-.6);

    return 2 * pi * bulk_diffusivity * rho /
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
    if (psi <= 0 || temperature <= vacancy_activation_temperature)
        return 0;
    // # COARSENING: cell zeta, steady diffusion on a sphere with uniform source and central sink (Olander 1976,
    // Sec. 13.5.1; HBS R1 Appendix A): zeta -> 1/(2 psi) for psi -> 0 (4 pi D R), zeta -> (1 - psi)/6 for psi -> 1.
    // Barani's 10 psi (1 + psi^3) / (-psi^6 + 5 psi^2 - 9 psi + 5) does not recover the isolated sink.
    const double z = (-std::pow(psi, 6) + 5 * psi * psi * psi - 9 * psi + 5) /
                     (10 * psi * std::pow(1 - psi * psi * psi, 2));
    return 2 * pi * vacancyPipe(temperature) * delta / (kb * temperature * z) *
           positive(pressure(atoms, v, temperature) - equilibrium(r, stress));
}

// # COARSENING: vacancies per bubble at mechanical equilibrium, p = 2 gamma / R - sigma_h, by bisection between
// low (p > p_eq) and high; p - p_eq decreases with the volume.
inline double equilibriumVacancies(double atoms, double low, double high, double temperature, double stress)
{
    auto excess = [&](double nv) {
        const double v = bubbleVolume(atoms, nv, temperature);
        return pressure(atoms, v, temperature) - equilibrium(radius(v), stress);
    };
    if (excess(high) >= 0)
        return high;
    for (int i = 0; i < 60 && high - low > 1e-6 * std::max(high, 1.); ++i)
    {
        const double mid = .5 * (low + high);
        (excess(mid) > 0 ? low : high) = mid;
    }
    return high;
}

// # COARSENING: absorption-only uptake, at most up to mechanical equilibrium in the step (the vacancy flow relaxes
// the pressure in much less than a step). Replaces the 0.2 nm/call limiter, which made the growth proportional to
// the number of steps. The 250 nm guard is kept.
inline double uptake(double vacancies, double atoms, double number, double temperature, double stress, double dt)
{
    const double old_r = radius(bubbleVolume(atoms, vacancies, temperature));
    if (dt <= 0 || pressure(atoms, bubbleVolume(atoms, vacancies, temperature), temperature) <= equilibrium(old_r, stress))
        return vacancies;
    const double rate = vacancyRate(vacancies, atoms, number, temperature, stress);
    if (rate <= 0)
        return vacancies;
    const double nv = equilibriumVacancies(atoms, vacancies, vacancies + dt * rate, temperature, stress);
    const double r = std::min(radius(bubbleVolume(atoms, nv, temperature)), radius_max);
    return positive((volume(r) - atoms * gasVolume(temperature)) / atomic_volume);
}
}

namespace hart_trace {
inline void write(double time,double T,double Db,double rho,double beta) {
const char* path=std::getenv("SCIANTIX_HART_TRACE");if(!path)return;
static std::ofstream file(path);static bool first=true;
if(first)file<<"time_h,T_K,Dbulk_m2s,rho_m2,core_fraction,Dcore_m2s,Dmean_m2s,beta_line_s_inv\n";
first=false;
file<<std::setprecision(17)<<time<<','<<T<<','<<Db<<','<<rho<<','
    <<intragranular_coarsening::pi*std::pow(intragranular_coarsening::gas_core_radius,2)*rho<<','
    <<intragranular_coarsening::gasCoreDiffusivity(T)<<','
    <<intragranular_coarsening::meanGasDiffusivity(T,Db)<<','<<beta<<'\n';
}
}
