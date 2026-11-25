#pragma once
#include "DislocationDensity.h"
#include <array>
#include <vector>

// #COARSENING: density-driven nucleation and conservative C, Mb, Md balances; parameters are
// explicit for refinement tests. K and the two mobilities have input scalings.
namespace coarsening
{
    // Omega (m^3/vacancy), also the native fixed atomic volume of bulk bubbles.
    constexpr double vacancyVolume = 4.09e-29;
    struct Environment
    {
        double temperature = 1200., fissionRate = 0., stress = 0., grainRadius = 5e-6, burnup = 0.;
    };
    enum class LineCaptureModel
    {
        Barani2020   = 0,
        Nicodemo2026 = 1
    };
    struct Parameters
    {
        dislocation_density::Model densityModel     = dislocation_density::Model::Nicodemo2026;
        LineCaptureModel           lineCaptureModel = LineCaptureModel::Nicodemo2026;
        double                     K = 1600000, lineCaptureMultiplier = 1, vacancyDiffusivityMultiplier = 30;
        double                     vacancyActivationTemperature = 1500.;
        double                     relativeTolerance            = 2e-5;
    };
    struct State
    {
        std::array<std::vector<double>, 3> gas;
        double                             bulkNumber = 0., largeNumber = 0., vacancies = 0.;
        double                             cumulativeBirths    = 0.;
        double                             projectedProduction = 0., diffusedGas = 0.;
    };
    struct Observation
    {
        double bulkRadius = 0., largeRadius = 0., pressure = 0., equilibriumPressure = 0.;
        double bulkSwelling = 0., largeSwelling = 0., rho = 0., lineCapture = 0.;
        double packingFraction = 0.;
    };
    struct Statistics
    {
        unsigned accepted = 0, rejected = 0;
        double   smallestStep = 1e300;
    };
    State                 initialState(unsigned modes = 40);
    std::array<double, 3> average(const State&);
    double                gasCoVolume(double temperature);  // omega(T)=B2=4*vHS (m^3/atom)
    double                bubbleVolume(double atoms, double vacancies, double temperature);  // n*omega(T)+nv*Omega
    double                diffusivity(const Environment&);
    double                vacancyDiffusivity(double temperature, const Parameters&);
    double                resolution(double radius, double fissionRate);
    double                vacancyRate(double nv, double atoms, double number, const Environment&, const Parameters&);
    Observation           observe(const State&, const Environment&, const Parameters&);
    Statistics evolve(State&, double seconds, const Environment& start, const Environment& end, const Parameters&);
}  // namespace coarsening
