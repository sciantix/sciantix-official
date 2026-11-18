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
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"
#include <cstdlib>

#include "IntraGranularBubbleCoarsening.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>

// #COARSENING: three implicit gas equations, vacancy uptake, density-driven births and
// coalescence use one adaptive integration throughout the irradiation history.
namespace coarsening
{
    namespace
    {
        constexpr double pi = 3.14159265358979323846;
        // #COARSENING
        double radius(double v)
        {
            return std::cbrt(std::max(0., 3 * v / (4 * pi)));
        }
        // #COARSENING
        Environment interpolate(const Environment& a, const Environment& b, double f)
        {
            return {a.temperature + f * (b.temperature - a.temperature),
                    a.fissionRate + f * (b.fissionRate - a.fissionRate),
                    a.stress + f * (b.stress - a.stress),
                    a.grainRadius + f * (b.grainRadius - a.grainRadius),
                    a.burnup + f * (b.burnup - a.burnup)};
        }
        // #COARSENING
        State step(const State&       old,
                   double             h,
                   const Environment& a,
                   const Environment& b,
                   const Parameters&  p,
                   bool               nonCoolingHistory)
        {
            State        s      = old;
            const auto   middle = interpolate(a, b, .5);
            const double rho0   = dislocation_density::correlation(a.burnup, a.temperature, p.densityModel);
            const double rho1   = dislocation_density::correlation(b.burnup, b.temperature, p.densityModel);
            // Integrate the positive full density change during irradiation.
            // The algebraic cooling rebound is not new irradiation damage.
            const bool   formation = middle.fissionRate > 0. && nonCoolingHistory;
            const double born      = formation ? p.K * std::max(0., rho1 - rho0) : 0.;
            s.largeNumber += born;
            s.cumulativeBirths += born;
            const auto   initial  = average(s);
            const double initialQ = s.vacancies;
            const auto   o        = observe(s, middle, p);
            const double D        = diffusivity(middle);
            // Keep native Turnbull small-bubble disappearance/return throughout the
            // history. Large-bubble gas uses Setyawan return without atomic phi.
            const double ab = 2 * pi * 6e-6 * std::pow(o.bulkRadius + 1e-9, 2) * middle.fissionRate;
            const double ad = resolution(o.largeRadius, middle.fissionRate);
            const double bb = 4 * pi * D * (o.bulkRadius + .21e-9) * s.bulkNumber;
            const double bd = s.largeNumber > 0. ? 4 * pi * D * o.largeRadius * s.largeNumber + o.lineCapture : 0.;
            // Native bulk closure: nucleation enters the bubble number only;
            // atomic inventories exchange gas through trapping and re-solution.
            s.bulkNumber = (s.bulkNumber + h * 50 * middle.fissionRate) / (1 + h * ab);
            // Atomic return and effective bubble disappearance are distinct.
            // Phi appears only in the number sink. Removed cavities return Qv
            // proportionally to the matrix; this is an explicit volume closure.
            const double occupancy = s.largeNumber > 0. ? initial[2] / s.largeNumber : 0.;
            const double loss      = occupancy >= 2. ? ad / std::max(1., occupancy - 1.) : 0.;
            s.largeNumber /= 1 + h * loss;
            s.vacancies /= 1 + h * loss;
            for (unsigned i = 0; i < s.gas[0].size(); ++i)
            {
                const double k = i + 1., ell = pi * pi * D * k * k / (middle.grainRadius * middle.grainRadius);
                const double weight = 6 / (pi * pi * k * k);  // native finite spectral projection
                const double B = 1 + h * ab, E = 1 + h * ad;
                const double c = (s.gas[0][i] + h * .27 * middle.fissionRate * weight + h * ab * s.gas[1][i] / B +
                                  h * ad * s.gas[2][i] / E) /
                                 (1 + h * ell + h * bb / B + h * bd / E);
                s.gas[1][i] = (s.gas[1][i] + h * bb * c) / B;
                s.gas[2][i] = (s.gas[2][i] + h * bd * c) / E;
                s.gas[0][i] = c;
                s.projectedProduction += h * .27 * middle.fissionRate * weight;
                s.diffusedGas += h * ell * c;
            }
            auto gas = average(s);
            if (s.largeNumber > 0. && gas[2] > 0. && p.vacancyDiffusivityMultiplier > 0.)
            {
                const double atoms = gas[2] / s.largeNumber, nv0 = s.vacancies / s.largeNumber;
                const double cell =
                    ((1 - 1e-10) / s.largeNumber - atoms * gasCoVolume(middle.temperature)) / vacancyVolume;
                if (nv0 >= cell)
                    throw std::runtime_error("Coarsening porosity exceeds cell");
                auto residual = [&](double nv)
                { return nv - nv0 - h * vacancyRate(nv, atoms, s.largeNumber, middle, p); };
                if (residual(nv0) < 0.)
                {
                    double lo = nv0, hi = std::min(cell, std::max(1., 2 * nv0));
                    for (unsigned j = 0; residual(hi) < 0.; ++j)
                    {
                        if (j > 100)
                            throw std::runtime_error("Cannot bracket transient vacancy root");
                        hi = std::min(cell, 2 * hi);
                    }
                    for (unsigned j = 0; j < 65; ++j)
                    {
                        const double m = .5 * (lo + hi);
                        if (residual(m) > 0.)
                            hi = m;
                        else
                            lo = m;
                    }
                    s.vacancies = .5 * (lo + hi) * s.largeNumber;
                }
            }
            if (s.largeNumber > 0.)
            {
                // Use the same total volume as pressure and swelling, including
                // its temperature dependence. Birth/number-loss dilution alone
                // is excluded from the growth that drives geometric overlap.
                const double initialVolume = bubbleVolume(initial[2], initialQ, a.temperature);
                const double porosity      = bubbleVolume(gas[2], s.vacancies, b.temperature);
                const double dV            = std::max(0., porosity - initialVolume) / s.largeNumber;
                if (porosity >= 1.)
                    throw std::runtime_error("Coarsening porosity >=1");
                const double lambda = (2 - porosity) / (2 * std::pow(1 - porosity, 3));
                s.largeNumber /= 1 + 4 * lambda * s.largeNumber * dV;
                const double V  = porosity / s.largeNumber;
                const double Rb = s.bulkNumber > 0. ? radius(vacancyVolume * gas[1] / s.bulkNumber) : 0.;
                const double excluded =
                    4 * pi / 3 * (std::pow(radius(V) + Rb, 3) - std::pow(radius(std::max(0., V - dV)) + Rb, 3));
                const double fraction = -std::expm1(-s.largeNumber * std::max(0., excluded));
                s.bulkNumber *= 1 - fraction;
                for (unsigned i = 0; i < s.gas[0].size(); ++i)
                {
                    const double moved = fraction * s.gas[1][i];
                    s.gas[1][i] -= moved;
                    s.gas[2][i] += moved;
                }
            }
            return s;
        }

        // #COARSENING
        double discrepancy(const State& a, const State& b, double tolerance)
        {
            double       error = 0.;
            const auto   x = average(a), y = average(b);
            const double scale   = x[0] + x[1] + x[2] + y[0] + y[1] + y[2];
            auto         compare = [&](double u, double v, double floor)
            { error = std::max(error, std::abs(u - v) / (floor + tolerance * std::max(std::abs(u), std::abs(v)))); };
            for (unsigned j = 0; j < 3; ++j)
                for (unsigned i = 0; i < a.gas[j].size(); ++i)
                    compare(a.gas[j][i], b.gas[j][i], 1e4 + tolerance * 1e-7 * scale / a.gas[j].size());
            compare(a.bulkNumber, b.bulkNumber, 1.);
            compare(a.largeNumber, b.largeNumber, 1.);
            compare(a.vacancies, b.vacancies, 1e10);
            return error;
        }
    }  // namespace

    // #COARSENING
    Statistics evolve(State& s, double duration, const Environment& a, const Environment& b, const Parameters& p)
    {
        if (!std::isfinite(duration) || duration < 0. || !std::isfinite(p.relativeTolerance) ||
            !(p.relativeTolerance > 0.) || !std::isfinite(p.K) || p.K < 0. ||
            !std::isfinite(p.lineDiffusivityMultiplier) || p.lineDiffusivityMultiplier < 0. ||
            !std::isfinite(p.vacancyDiffusivityMultiplier) || p.vacancyDiffusivityMultiplier < 0. ||
            !std::isfinite(p.vacancyActivationTemperature) || p.vacancyActivationTemperature < 0.)
            throw std::invalid_argument("Invalid coarsening parameters");
        for (const auto& e : {a, b})
            if (!std::isfinite(e.temperature) || e.temperature <= 0. || !std::isfinite(e.fissionRate) ||
                e.fissionRate < 0. || !std::isfinite(e.stress) || !std::isfinite(e.grainRadius) ||
                e.grainRadius <= 0. || !std::isfinite(e.burnup) || e.burnup < 0.)
                throw std::invalid_argument("Invalid coarsening environment");
        Statistics stats;
        // Fresh B=0 histories start with rho=Nd=0; no Nd=K*rho seed is imposed.
        // A finite-burnup start needs its physical initial population or restart.
        if (duration == 0.)
            return stats;
        double t = 0., h = duration;
        while (t < duration)
        {
            h = std::min(h, duration - t);
            // Resolve the hard vacancy gate at its exact history crossing.
            if (b.temperature != a.temperature)
            {
                const double crossing =
                    duration * (p.vacancyActivationTemperature - a.temperature) / (b.temperature - a.temperature);
                if (crossing > t + 1e-13 * std::max(1., duration) && crossing < t + h)
                    h = crossing - t;
            }
            const auto left = interpolate(a, b, t / duration), right = interpolate(a, b, (t + h) / duration);
            const bool nonCoolingHistory = b.temperature >= a.temperature;
            State      full              = step(s, h, left, right, p, nonCoolingHistory);
            State      half = step(s, .5 * h, left, interpolate(a, b, (t + .5 * h) / duration), p, nonCoolingHistory);
            half = step(half, .5 * h, interpolate(a, b, (t + .5 * h) / duration), right, p, nonCoolingHistory);
            const double error = discrepancy(full, half, p.relativeTolerance);
            if (error <= 1.)
            {
                s = std::move(half);
                t += h;
                ++stats.accepted;
                stats.smallestStep = std::min(stats.smallestStep, h);
            }
            else
                ++stats.rejected;
            // An accepted final remainder may be below the rejection threshold.
            // Completion is success, not step underflow.
            if (t >= duration)
                break;
            if (stats.accepted + stats.rejected > 1000000)
                throw std::runtime_error("Coarsening step limit");
            h *= std::clamp(error > 0. ? .85 / std::sqrt(error) : 2., .1, 2.);
            if (h < 1e-14 * std::max(1., duration))
                throw std::runtime_error("Coarsening step underflow");
        }
        return stats;
    }
}  // namespace coarsening

#ifndef SCIANTIX_COARSENING_STANDALONE
// #COARSENING: SCIANTIX state adapter and public diagnostics.
namespace
{
    // #COARSENING
    coarsening::Environment environment(double T, double F, double stress, double grain, double B)
    {
        return {T, F, stress * 1e6, grain, B * dislocation_density::oxide_to_heavy_metal};
    }
}  // namespace

// #COARSENING
void Simulation::IntraGranularBubbleCoarsening()
{
    using namespace coarsening;
    if (input_variable["iDiffusionSolver"].getValue() != 2 || input_variable["iFuelMatrix"].getValue() != 0 ||
        input_variable["iHelium"].getValue() != 0 || input_variable["iRadioactiveFissionGas"].getValue() != 0 ||
        input_variable["iGrainBoundarySweeping"].getValue() != 0 ||
        input_variable["iHighBurnupStructureFormation"].getValue() != 0 ||
        input_variable["iHighBurnupStructurePorosity"].getValue() != 0 ||
        input_variable["iResolutionRate"].getValue() != 1 || input_variable["iFissionGasDiffusivity"].getValue() != 1 ||
        input_variable["iTrappingRate"].getValue() != 1 || input_variable["iNucleationRate"].getValue() != 1)
        throw std::runtime_error(
            "Coarsening requires diffusion solver2, native bulk options1, stable Xe/Kr UO2, no HBS or sweeping");
    Parameters p;
    const int  density = static_cast<int>(input_variable["iDislocationDensity"].getValue());
    if (density < 0 || density > 1)
        throw std::invalid_argument("iDislocationDensity must be 0 (constant) or 1 (Nicodemo2026)");
    p.densityModel = static_cast<dislocation_density::Model>(density);
    p.K *= scaling_factors["Coarsening K"].getValue();
    p.lineDiffusivityMultiplier *= scaling_factors["Coarsening gas diffusivity"].getValue();
    p.vacancyDiffusivityMultiplier *= scaling_factors["Coarsening vacancy diffusivity"].getValue();
    if (const char* raw = std::getenv("SCIANTIX_COARSENING_TOLERANCE"))
    {
        const std::string text(raw);
        std::size_t       used = 0;
        p.relativeTolerance    = std::stod(text, &used);
        if (used != text.size() || !std::isfinite(p.relativeTolerance) || p.relativeTolerance <= 0.)
            throw std::invalid_argument("Invalid SCIANTIX_COARSENING_TOLERANCE");
    }

    const double yield = sciantix_system["Xe in UO2"].getYield() + sciantix_system["Kr in UO2"].getYield();
    if (std::abs(yield - .27) > 1e-12)
        throw std::runtime_error("Stable gas yield must remain .27");
    for (const std::string name : {"Diffusivity", "Resolution rate", "Trapping rate", "Nucleation rate"})
        if (std::abs(scaling_factors[name].getValue() - 1.) > 1e-12)
            throw std::runtime_error("Coarsening model4 requires unit native scaling: " + name);
    auto s                = initialState(n_modes);
    s.bulkNumber          = sciantix_variable["Intragranular bubble concentration"].getInitialValue();
    s.largeNumber         = sciantix_variable["Coarsening dislocation concentration"].getInitialValue();
    s.vacancies           = sciantix_variable["Coarsening vacancy concentration"].getInitialValue();
    s.cumulativeBirths    = sciantix_variable["Coarsening cumulative nucleated bubbles"].getInitialValue();
    s.projectedProduction = sciantix_variable["Coarsening projected production"].getInitialValue();
    s.diffusedGas         = sciantix_variable["Coarsening diffused gas"].getInitialValue();
    const double pi = std::acos(-1.), projection = -std::sqrt(8 / pi) / (4 * pi / 3);
    for (int i = 0; i < n_modes; ++i)
    {
        const double w = projection * (i % 2 == 0 ? -1. : 1.) / (i + 1.);
        s.gas[0][i]    = w * (getDiffusionModesSolution("Xe")[i] + getDiffusionModesSolution("Kr")[i]);
        s.gas[1][i]    = w * (getDiffusionModesBubbles("Xe")[i] + getDiffusionModesBubbles("Kr")[i]);
        s.gas[2][i]    = w * (getDiffusionModesDislocation("Xe")[i] + getDiffusionModesDislocation("Kr")[i]);
        // The reduced kernel uses identical Xe/Kr transport and their physical
        // production ratio. Never silently change a species' initial inventory.
        const double xeFraction = sciantix_system["Xe in UO2"].getYield() / yield;
        const double xe[3]      = {w * getDiffusionModesSolution("Xe")[i],
                                   w * getDiffusionModesBubbles("Xe")[i],
                                   w * getDiffusionModesDislocation("Xe")[i]};
        for (unsigned pool = 0; pool < 3; ++pool)
            if (std::abs(xe[pool] - xeFraction * s.gas[pool][i]) > 1e-8 * std::max(1., std::abs(s.gas[pool][i])))
                throw std::runtime_error("Coarsening requires production-proportional initial Xe/Kr inventories");
    }
    const auto   old     = average(s);
    const double source0 = s.projectedProduction, out0 = s.diffusedGas;
    const auto   a  = environment(history_variable["Temperature"].getInitialValue(),
                               history_variable["Fission rate"].getInitialValue(),
                               history_variable["Hydrostatic stress"].getInitialValue(),
                               sciantix_variable["Grain radius"].getInitialValue(),
                               sciantix_variable["Burnup"].getInitialValue());
    const auto   b  = environment(history_variable["Temperature"].getFinalValue(),
                               history_variable["Fission rate"].getFinalValue(),
                               history_variable["Hydrostatic stress"].getFinalValue(),
                               sciantix_variable["Grain radius"].getFinalValue(),
                               sciantix_variable["Burnup"].getFinalValue());
    const double dt = physics_variable["Time step"].getFinalValue();
    evolve(s, dt, a, b, p);
    const auto   m = average(s);
    const auto   o = observe(s, b, p);
    const double balance =
        m[0] + m[1] + m[2] - old[0] - old[1] - old[2] - (s.projectedProduction - source0) + (s.diffusedGas - out0);
    if (std::abs(balance) > 2e-10 * std::max(1., m[0] + m[1] + m[2] + old[0] + old[1] + old[2]))
        throw std::runtime_error("Coarsening accepted spectral flux balance failed");
    auto set = [&](const std::string& name, double value) { sciantix_variable[name].setFinalValue(value); };
    for (const std::string name : {"Xe", "Kr"})
    {
        const double f = sciantix_system[name + " in UO2"].getYield() / yield;
        for (int i = 0; i < n_modes; ++i)
        {
            const double w                        = projection * (i % 2 == 0 ? -1. : 1.) / (i + 1.);
            getDiffusionModesSolution(name)[i]    = f * s.gas[0][i] / w;
            getDiffusionModesBubbles(name)[i]     = f * s.gas[1][i] / w;
            getDiffusionModesDislocation(name)[i] = f * s.gas[2][i] / w;
        }
        set(name + " in intragranular solution", f * m[0]);
        set(name + " in intragranular bubbles", f * (m[1] + m[2]));
        set(name + " in grain", f * (m[0] + m[1] + m[2]));
        set(name + " in bulk bubbles", f * m[1]);
        set(name + " in dislocation bubbles", f * m[2]);
        set(name + " produced",
            sciantix_variable[name + " produced"].getInitialValue() +
                f * yield * .5 * (a.fissionRate + b.fissionRate) * dt);
        set("Intragranular " + name + " atoms per bubble", s.bulkNumber > 0. ? f * m[1] / s.bulkNumber : 0.);
    }
    set("Intragranular bubble concentration", s.bulkNumber);
    set("Intragranular bubble radius", o.bulkRadius);
    set("Intragranular bubble volume", s.bulkNumber > 0. ? o.bulkSwelling / s.bulkNumber : 0.);
    set("Intragranular gas bubble swelling", o.bulkSwelling + o.largeSwelling);
    set("Coarsening dislocation concentration", s.largeNumber);
    set("Coarsening vacancy concentration", s.vacancies);
    set("Coarsening dislocation radius", o.largeRadius);
    set("Coarsening vacancies per bubble", s.largeNumber > 0. ? s.vacancies / s.largeNumber : 0.);
    set("Coarsening pressure", o.pressure);
    set("Coarsening packing fraction", o.packingFraction);
    set("Coarsening equilibrium pressure", o.equilibriumPressure);
    set("Coarsening dislocation swelling", o.largeSwelling);
    set("Coarsening bulk swelling", o.bulkSwelling);
    set("Coarsening line trapping rate", o.lineCapture);
    set("Coarsening cumulative nucleated bubbles", s.cumulativeBirths);
    set("Coarsening projected production", s.projectedProduction);
    set("Coarsening diffused gas", s.diffusedGas);
}

// #COARSENING
void Simulation::CoarseningDiagnostics()
{
    using namespace dislocation_density;
    coarsening::Parameters p;
    p.densityModel =
        static_cast<dislocation_density::Model>(static_cast<int>(input_variable["iDislocationDensity"].getValue()));
    const double T0      = history_variable["Temperature"].getInitialValue(),
                 T1      = history_variable["Temperature"].getFinalValue();
    const double B0      = sciantix_variable["Burnup"].getInitialValue() * oxide_to_heavy_metal;
    const double B1      = sciantix_variable["Burnup"].getFinalValue() * oxide_to_heavy_metal;
    const double dt      = physics_variable["Time step"].getFinalValue();
    const double tangent = dt > 0. ? timeDerivative(B1, T1, (B1 - B0) / dt, (T1 - T0) / dt, p.densityModel) : 0.;
    sciantix_variable["Dislocation density"].setFinalValue(correlation(B1, T1, p.densityModel));
    sciantix_variable["Dislocation density rate"].setFinalValue(tangent);
    sciantix_variable["Dislocation density interval rate"].setFinalValue(
        intervalRate(B0, T0, B1, T1, dt, p.densityModel));
    // With no dislocation sites, expose native bulk inventories without changing them.
    if (sciantix_variable["Coarsening dislocation concentration"].getFinalValue() == 0.)
    {
        sciantix_variable["Coarsening bulk swelling"].setFinalValue(
            sciantix_variable["Intragranular gas bubble swelling"].getFinalValue());
        for (const std::string gas : {"Xe", "Kr"})
            sciantix_variable[gas + " in bulk bubbles"].setFinalValue(
                sciantix_variable[gas + " in intragranular bubbles"].getFinalValue());
    }
}

void Simulation::IntraGranularBubbleBehavior()
{
    // #COARSENING: the only additional bubble model; legacy options remain native.
    if (input_variable["iIntraGranularBubbleBehavior"].getValue() == 4)
    {
        IntraGranularBubbleCoarsening();
        return;
    }
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

    if (sciantix_variable["He in intragranular bubbles"].getInitialValue() > 0.0)
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(
            sqrt(sciantix_variable["He in intragranular bubbles"].getFinalValue() /
                 sciantix_variable["He in intragranular bubbles"].getInitialValue()));
    else
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(0.0);
}
#endif  // SCIANTIX_COARSENING_STANDALONE
