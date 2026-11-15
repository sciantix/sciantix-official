#include "DislocationDensity.h"
#include <cmath>
#include <limits>
#include <stdexcept>
namespace dislocation_density
{
    // #COARSENING: frozen Veshchunov-White fit; B in GWd/tHM, T in K.
    double burnupExponent(double T);
    namespace
    {
        constexpr double log10_A20 = 14.636995815590845;
        constexpr double n_cold = 0.5271037725180502, n_hot = 1.048816395670735;
        constexpr double f_inf = 0.06994900781169244;
        constexpr double Tc = 1050.3656167288575, width = 189.81458246783382;
        constexpr double anchor = 900., smoothing = 20., floor20 = 1e13;
        constexpr double log_tau = 4.532184881113394, power = 0.5500000000000006;
        // #COARSENING
        double sigmoid(double T)
        {
            const double x = (T - Tc) / width;
            return x >= 0 ? 1 / (1 + std::exp(-x)) : std::exp(x) / (1 + std::exp(x));
        }
        // #COARSENING
        double veshchunov(double B, double T)
        {
            return std::pow(10., log10_A20) * std::pow(B / 20., burnupExponent(T)) * (1 - (1 - f_inf) * sigmoid(T));
        }
    }  // namespace
    // #COARSENING
    double burnupExponent(double T)
    {
        return n_cold + (n_hot - n_cold) * sigmoid(T);
    }
    // #COARSENING
    double correlation(double B, double T, Model model)
    {
        if (!std::isfinite(B) || B < 0 || !std::isfinite(T) || T <= 0)
            throw std::invalid_argument("Dislocation density requires finite B >= 0 and T > 0");
        switch (model)
        {
            case Model::Barani2020:
                return 4e13;
            case Model::Nicodemo2026:
            {
                if (B == 0)
                    return 0.;
                if (T < anchor)
                    return veshchunov(B, T);
                const double delta        = T - anchor;
                const double u            = std::hypot(delta, smoothing) - smoothing;
                const double asymptote100 = floor20 * std::pow(5., n_hot);
                const double R100         = asymptote100 + (veshchunov(100., anchor) - asymptote100) *
                                                       std::exp(-std::pow(u / std::exp(log_tau), power));
                return R100 * std::pow(B / 100., burnupExponent(T));
            }
        }
        throw std::invalid_argument("Unknown dislocation density model");
    }

    // #COARSENING
    Response response(double B, double T, Model model)
    {
        const double value = correlation(B, T, model);
        if (model == Model::Barani2020)
            return {value, 0., 0.};
        const double s = sigmoid(T), ds = s * (1. - s) / width;
        const double n = burnupExponent(T), dn = (n_hot - n_cold) * ds;
        if (B == 0.)
        {
            const double atOne = correlation(1., T, model);
            return {0., n < 1. ? std::numeric_limits<double>::infinity() : (n == 1. ? atOne : 0.), 0.};
        }
        double thermal = 0.;
        if (T < anchor)
        {
            const double fraction = 1. - (1. - f_inf) * s;
            thermal               = value * (dn * std::log(B / 20.) - (1. - f_inf) * ds / fraction);
        }
        else
        {
            const double delta = T - anchor, u = std::hypot(delta, smoothing) - smoothing;
            const double tau = std::exp(log_tau), floor100 = floor20 * std::pow(5., n_hot);
            const double R100 = floor100 + (veshchunov(100., anchor) - floor100) * std::exp(-std::pow(u / tau, power));
            // At the join the right derivative of the smoothed thermal term is0
            // (2*power>1). The lower-temperature branch has a different derivative.
            const double dR = delta > 0. ? -(R100 - floor100) * power / tau * std::pow(u / tau, power - 1.) * delta /
                                               std::hypot(delta, smoothing)
                                         : 0.;
            thermal         = value * dn * std::log(B / 100.) + std::pow(B / 100., n) * dR;
        }
        return {value, value * n / B, thermal};
    }

    // #COARSENING
    double timeDerivative(double B, double T, double burnupRate, double temperatureRate, Model model)
    {
        if (!std::isfinite(burnupRate) || !std::isfinite(temperatureRate))
            throw std::invalid_argument("Density driving rates must be finite");
        const auto r = response(B, T, model);
        return (burnupRate == 0. ? 0. : r.dBurnup * burnupRate) + r.dTemperature * temperatureRate;
    }

    // #COARSENING
    double intervalRate(double B0, double T0, double B1, double T1, double seconds, Model model)
    {
        if (!std::isfinite(seconds) || seconds < 0.)
            throw std::invalid_argument("Density interval must be finite and nonnegative");
        return seconds > 0. ? (correlation(B1, T1, model) - correlation(B0, T0, model)) / seconds : 0.;
    }
}  // namespace dislocation_density
