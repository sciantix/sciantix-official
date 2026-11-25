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
    static double lineCaptureRate(const Environment& e, double rho, LineCaptureModel model)
    {
        constexpr double burgersMagnitude = .385e-9;
        constexpr double coreRadius       = 5 * burgersMagnitude;
        constexpr double fastRegionRadius = 50 * burgersMagnitude;
        const double     cellRadius       = 1 / std::sqrt(pi * rho);
        if (cellRadius <= coreRadius)
            throw std::runtime_error("Dislocation core exceeds its radial capture cell");

        const double nearCore = 7.74e-6 * std::exp(-1.21e-19 / (kB * e.temperature));
        if (model == LineCaptureModel::Barani2020)
        {
            // Barani et al. (2020), Eq. (6) and Table 1.
            const double geometry = std::log(cellRadius / coreRadius) - 3. / 5.;
            if (geometry <= 0.)
                throw std::runtime_error("Barani line capture requires log(cell radius / core radius) > 3/5");
            return 2 * pi * rho * nearCore / geometry;
        }

        const double core      = coreRadius / cellRadius;
        const double interface = std::min(fastRegionRadius / cellRadius, 1.);
        const double bulk      = diffusivity(e);

        // Cylindrical cell with uniform production, an absorbing core and a
        // reflecting outer boundary. Integrate (1 - r^2)^2 / r in each region
        // using the radius normalized by the cell radius.
        const auto geometry = [](double lo, double hi)
        {
            const double lo2 = lo * lo, hi2 = hi * hi;
            return std::log(hi / lo) - (hi2 - lo2) + (hi2 * hi2 - lo2 * lo2) / 4;
        };
        const double resistance = geometry(core, interface) / nearCore + geometry(interface, 1.) / bulk;
        return 2 * pi * rho * (1 - core * core) / resistance;
    }
    // #COARSENING
    double vacancyDiffusivity(double T, const Parameters& p)
    {
        // #COARSENING: bulk vacancy law with an explicit effective prefactor.
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
    double vacancyRate(double nv, double atoms, double number, const Environment& e, const Parameters& p)
    {
        if (atoms <= 0. || number <= 0. || p.vacancyDiffusivityMultiplier == 0. ||
            e.temperature < p.vacancyActivationTemperature)
            return 0.;
        const double volume = bubbleVolume(atoms, nv, e.temperature), R = radius(volume), delta = radius(1 / number),
                     x = R / delta;
        if (x >= 1.)
            throw std::runtime_error("Coarsening bubble exceeds Wigner-Seitz cell");
        // #COARSENING: reciprocal of printed Barani Eq.9. The dilute limit
        // gives 4*pi*Dv*R*(p-peq)/(kT), the isolated-sphere diffusion flux.
        // Factor the numerator to avoid cancellation as x approaches one.
        const double numerator = (1 - x) * (5 - 4 * x + x * x + std::pow(x, 3) + std::pow(x, 4) + std::pow(x, 5));
        const double zeta      = numerator / (10 * x * (1 + std::pow(x, 3)));
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
            o.packingFraction     = gasCoVolume(e.temperature) * m[2] / (4 * o.largeSwelling);
            o.equilibriumPressure = 1.4 / o.largeRadius - e.stress;
        }
        if (o.rho > 0. && p.lineCaptureMultiplier > 0.)
            o.lineCapture = p.lineCaptureMultiplier * lineCaptureRate(e, o.rho, p.lineCaptureModel);
        return o;
    }
}  // namespace coarsening
