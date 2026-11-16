#include "IntraGranularBubbleCoarsening.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>

// #COARSENING: correlations and geometry, independent of the SCIANTIX adapter.
namespace coarsening
{
    namespace
    {
        constexpr double pi = 3.14159265358979323846, kB = 1.380649e-23, eV = 1.602176634e-19;
        // #COARSENING
        double radius(double v)
        {
            return std::cbrt(std::max(0., 3 * v / (4 * pi)));
        }
        // #COARSENING: Carnahan-Starling pressure in the whole bubble cavity.
        double gasPressure(double atoms, double volume, double temperature)
        {
            if (atoms <= 0. || volume <= 0.)
                return 0.;
            const double packing = gasCoVolume(temperature) * atoms / (4 * volume);
            if (!std::isfinite(packing) || packing < 0. || packing >= 1.)
                throw std::runtime_error("Coarsening Carnahan-Starling packing fraction outside [0,1)");
            const double Z = (1 + packing + packing * packing - packing * packing * packing) / std::pow(1 - packing, 3);
            return atoms * kB * temperature / volume * Z;
        }
    }  // namespace
    // #COARSENING: additive reference co-volume, B2=4*vHS, using the effective
    // Xe hard-sphere diameter of Barani 2020, Eq. (11). This is not V/n from EOS inversion.
    double gasCoVolume(double temperature)
    {
        if (!std::isfinite(temperature) || temperature <= 0.)
            throw std::invalid_argument("Invalid coarsening gas co-volume temperature");
        const double diameter = 4.45e-10 * (.8542 - .03996 * std::log(temperature / 231.2));
        if (!std::isfinite(diameter) || diameter <= 0.)
            throw std::invalid_argument("Coarsening hard-sphere diameter must be positive");
        return 2 * pi * diameter * diameter * diameter / 3;
    }
    // #COARSENING: applies equally to per-bubble and volume-density inventories.
    double bubbleVolume(double atoms, double vacancies, double temperature)
    {
        return atoms * gasCoVolume(temperature) + vacancies * vacancyVolume;
    }
    // #COARSENING
    State initialState(unsigned modes)
    {
        State s;
        for (auto& field : s.gas)
            field.assign(modes, 0.);
        return s;
    }
    // #COARSENING
    std::array<double, 3> average(const State& s)
    {
        std::array<double, 3> a{};
        for (unsigned j = 0; j < 3; ++j)
            for (double v : s.gas[j])
                a[j] += v;
        return a;
    }
    // #COARSENING
    double diffusivity(const Environment& e)
    {
        return 7.6e-10 * std::exp(-4.86e-19 / (kB * e.temperature)) +
               5.64e-25 * std::sqrt(e.fissionRate) * std::exp(-1.91e-19 / (kB * e.temperature)) + 8e-40 * e.fissionRate;
    }
    // #COARSENING
    double captureDiffusivity(const Environment& e, const Parameters& p)
    {
        const double rho      = dislocation_density::correlation(e.burnup, e.temperature, p.densityModel);
        const double fraction = pi * std::pow(.385e-9, 2) * rho;
        if (fraction < 0. || fraction > 1.)
            throw std::runtime_error("Coarsening pipe volume fraction outside [0,1]");
        const double pipe = 7.74e-6 * std::exp(-1.21e-19 / (kB * e.temperature));
        // Hart-type homogenization used ONLY for line capture. b is an assumed
        // atomic-scale pipe radius; this is not a derivation of local radial flux.
        return p.lineDiffusivityMultiplier * ((1 - fraction) * diffusivity(e) + fraction * pipe);
    }
    // #COARSENING
    double vacancyDiffusivity(double T, const Parameters& p)
    {
        return p.vacancyDiffusivityMultiplier * 7.12e-7 * std::exp(-4.72 * eV / (kB * T));
    }
    // #COARSENING
    double resolution(double R, double F)
    {
        const double x = R * 1e9;
        return F * (9.49e-24 * std::exp(-.0707 * x) +
                    (9.18e-23 - 9.49e-24) / (1 + 7.982 * x * x) * std::exp(-.0371 * x * x));
    }
    // #COARSENING
    double thermalKFraction(double T)
    {
        const double x = (T - 1800.) / 150.;
        return x <= 0. ? 0. : (x >= 1. ? 1. : x * x * x * (10 + x * (-15 + 6 * x)));
    }
    // #COARSENING
    double thermalKFractionDerivative(double T)
    {
        const double x = (T - 1800.) / 150.;
        return x <= 0. || x >= 1. ? 0. : 30 * x * x * (1 - x) * (1 - x) / 150.;
    }
    // #COARSENING
    double vacancyRate(double nv, double atoms, double number, const Environment& e, const Parameters& p)
    {
        if (atoms <= 0. || number <= 0. || p.vacancyDiffusivityMultiplier == 0.)
            return 0.;
        const double volume = bubbleVolume(atoms, nv, e.temperature), R = radius(volume), delta = radius(1 / number),
                     x = R / delta;
        if (x >= 1.)
            throw std::runtime_error("Coarsening bubble exceeds Wigner-Seitz cell");
        const double zeta     = 10 * x * (1 + std::pow(x, 3)) / (-std::pow(x, 6) + 5 * x * x - 9 * x + 5);
        const double pressure = gasPressure(atoms, volume, e.temperature), peq = 1.4 / R - e.stress;
        return 2 * pi * vacancyDiffusivity(e.temperature, p) * delta / (kB * e.temperature * zeta) *
               std::max(pressure - peq, 0.);
    }
    // #COARSENING
    Observation observe(const State& s, const Environment& e, const Parameters& p)
    {
        const auto  m = average(s);
        Observation o;
        o.rho           = dislocation_density::correlation(e.burnup, e.temperature, p.densityModel);
        o.bulkSwelling  = vacancyVolume * m[1];
        o.largeSwelling = bubbleVolume(m[2], s.vacancies, e.temperature);
        if (s.bulkNumber > 0.)
            o.bulkRadius = radius(o.bulkSwelling / s.bulkNumber);
        if (s.largeNumber > 0. && o.largeSwelling > 0.)
        {
            o.largeRadius         = radius(o.largeSwelling / s.largeNumber);
            o.pressure            = gasPressure(m[2], o.largeSwelling, e.temperature);
            o.equilibriumPressure = 1.4 / o.largeRadius - e.stress;
        }
        if (o.rho > 0. && s.activeFraction > 0.)
        {
            const double denominator = std::log(1 / std::sqrt(pi * o.rho) / (5 * .385e-9)) - .6;
            if (denominator <= 0.)
                throw std::runtime_error("Line capture outside dilute-network domain");
            o.lineCapture = s.activeFraction * 2 * pi * o.rho * captureDiffusivity(e, p) / denominator;
        }
        return o;
    }
}  // namespace coarsening
