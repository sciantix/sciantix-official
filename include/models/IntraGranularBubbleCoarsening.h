#pragma once
#include "DislocationDensity.h"
#include <array>
#include <vector>

// #COARSENING: one conservative C, Mb, Md model; numerical parameters are
// explicit for refinement tests. K and the two mobilities have input scalings.
namespace coarsening
{
    struct Environment
    {
        double temperature = 1200., fissionRate = 0., stress = 0., grainRadius = 5e-6, burnup = 0.;
    };
    struct Parameters
    {
        dislocation_density::Model densityModel = dislocation_density::Model::Nicodemo2026;
        double                     K = 3e6, lineDiffusivityMultiplier = 1., vacancyDiffusivityMultiplier = 3.;
        double                     relativeTolerance = 2e-5;
    };
    struct State
    {
        std::array<std::vector<double>, 3> gas;
        double                             bulkNumber = 0., largeNumber = 0., vacancies = 0., activeFraction = 0.;
        double                             cumulativeBirths = 0., cumulativeRecruitedGas = 0.;
        double                             projectedProduction = 0., diffusedGas = 0.;
    };
    struct Observation
    {
        double bulkRadius = 0., largeRadius = 0., pressure = 0., equilibriumPressure = 0.;
        double bulkSwelling = 0., largeSwelling = 0., rho = 0., lineCapture = 0.;
    };
    struct Statistics
    {
        unsigned accepted = 0, rejected = 0;
        double   smallestStep = 1e300;
    };
    State                 initialState(unsigned modes = 40);
    std::array<double, 3> average(const State&);
    double                diffusivity(const Environment&);
    double                captureDiffusivity(const Environment&, const Parameters&);
    double                vacancyDiffusivity(double temperature, const Parameters&);
    double                resolution(double radius, double fissionRate);
    double                thermalKFraction(double temperature);
    double                thermalKFractionDerivative(double temperature);
    double                vacancyRate(double nv, double atoms, double number, const Environment&, const Parameters&);
    Observation           observe(const State&, const Environment&, const Parameters&);
    double                recruit(State&, double requested);
    Statistics evolve(State&, double seconds, const Environment& start, const Environment& end, const Parameters&);
}  // namespace coarsening
