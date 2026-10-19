#ifndef SCIANTIX_UN_L3_INTRAGRANULAR_H
#define SCIANTIX_UN_L3_INTRAGRANULAR_H

#include "UNParameters.h"
#include "UNState.h"
#include <map>
#include <string>

class Solver;

namespace un_l3
{
    // Numerical exchange only: mutates modes and averages, preserving every other
    // state field. dt is in seconds and can be zero. No time/production/GF update.
    void advanceExchange(Solver& solver, State& state, const ExchangeCoefficients& coefficients, double dt);

    struct IntraGranularDiagnostics
    {
        std::map<std::string, double>      values;
        std::array<std::vector<double>, 3> after_exchange;
    };
    // Physical intragranular step only. Does not execute grain-face physics,
    // advance cumulative production/time, or enter the normal Simulation cycle.
    IntraGranularDiagnostics advanceIntraGranular(
        Solver& solver, State& state, const Parameters& parameters, const PhysicsOptions& options, double dt);
}  // namespace un_l3

#endif
