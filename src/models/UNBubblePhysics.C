#include "UNBubblePhysics.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>
namespace un_l3
{
    double matrixVolume(const Parameters& p)
    {
        return std::pow(p.lattice_parameter, 3) / 4.0;
    }
    double sphereVolume(double r)
    {
        return r <= 0.0 ? 0.0 : (4.0 / 3.0) * M_PI * std::pow(r, 3);
    }
    double radiusFromVolume(double v)
    {
        return v <= 0.0 ? 0.0 : std::pow(3.0 * v / (4.0 * M_PI), 1.0 / 3.0);
    }
    double phiPopulation(double gas, double number)
    {
        if (number <= 0.0 || gas <= 0.0)
            return 0.0;
        const double n = gas / number;
        return n <= 1.0 ? 0.0 : 1.0 / (n - 1.0);
    }
    double coalescenceLambda(double v, double n)
    {
        const double x = std::max(0.0, std::min(v * n, 0.999999));
        return (2.0 - x) / (2.0 * std::pow(1.0 - x, 3));
    }
    double internalPressure(const Parameters& p, double gas, double vacancy)
    {
        if (gas <= 0.0)
            return 0.0;
        if (vacancy <= 0.0)
            return std::numeric_limits<double>::infinity();
        const double denom = vacancy * matrixVolume(p);
        return denom <= 0.0 ? std::numeric_limits<double>::infinity() : p.kB_J * p.temperature * gas / denom;
    }
    double equilibriumPressure(const Parameters& p, double radius)
    {
        return 2.0 * p.gamma_b / std::max(radius, p.min_radius_for_pressure) - p.hydrostatic_stress;
    }
    std::pair<double, double> pressureSeed(const Parameters& p, double atoms, double factor)
    {
        const double ng = std::max(atoms, 1e-30), f = std::max(factor, 1e-12), om = matrixVolume(p);
        const auto   radius = [&](double nv) { return radiusFromVolume(std::max(p.omega_fg * ng + om * nv, 1e-300)); };
        const auto   residual = [&](double nv)
        {
            return p.kB_J * p.temperature * ng / (nv * om) -
                   f * (2.0 * p.gamma_b / std::max(radius(nv), p.min_radius_for_pressure) - p.hydrostatic_stress);
        };
        double lo = 1e-30, hi = 1.0;
        bool   bracket = false;
        for (int i = 0; i < 300; ++i)
        {
            if (residual(hi) < 0.0)
            {
                bracket = true;
                break;
            }
            hi *= 2.0;
        }
        if (!bracket)
            return {hi, radius(hi)};  // exact notebook fallback, not a new correction
        for (int i = 0; i < 160; ++i)
        {
            const double mid = std::sqrt(lo * hi);
            if (residual(mid) > 0.0)
                lo = mid;
            else
                hi = mid;
        }
        return {hi, radius(hi)};
    }
    std::pair<double, double> seedVacancyInventory(const Parameters& p, double gas, double number, double factor)
    {
        if (gas <= 0.0 || number <= 0.0)
            return {0.0, 0.0};
        const auto seed = pressureSeed(p, std::max(gas / number, 1e-30), factor);
        return {seed.first * number, seed.second};
    }
    double wignerSeitzDelta(double n)
    {
        return std::pow(3.0 / (4.0 * M_PI * std::max(n, 1.0)), 1.0 / 3.0);
    }
    double zetaGeometry(const PhysicsOptions& o, double r, double n)
    {
        const double psi = std::max(r / wignerSeitzDelta(n), 1e-12);
        if (!std::isfinite(psi))
            return 1e300;
        if (o.zeta_overflow_guard && psi >= 1.0)
        {
            if (psi > 1e75)
                return 1e300;
            return std::max(10.0 * psi * (1.0 + std::pow(psi, 3)) / 1e-30, 1e-30);
        }
        const double den = std::max(-std::pow(psi, 6) + 5.0 * std::pow(psi, 2) - 9.0 * psi + 5.0, 1e-30);
        return std::max(10.0 * psi * (1.0 + std::pow(psi, 3)) / den, 1e-30);
    }
    VacancyStep advanceVacancies(
        const Parameters& p, const PhysicsOptions& o, double dv, double r, double n, double gas, double old, double dt)
    {
        if (n <= 0.0 || gas <= 0.0)
            return {old, 0.0};
        const double update_r = r > 0.0 ? r : radiusFromVolume(p.omega_fg * gas / n);
        if (update_r <= 0.0)
            return {old, 0.0};
        const double eq = 2.0 * p.gamma_b / update_r - p.hydrostatic_stress;
        if (p.vacancy_absorption_only && internalPressure(p, gas, old) <= eq)
            return {old, 0.0};
        const double delta = wignerSeitzDelta(n), zeta = zetaGeometry(o, update_r, n);
        const double A = 2.0 * M_PI * dv * delta * n / (p.kB_J * p.temperature * zeta);
        const double C = p.kB_J * p.temperature * gas / matrixVolume(p);
        const double B = old - dt * A * eq, disc = B * B + 4.0 * dt * A * C;
        if (disc < 0.0)
            throw std::domain_error("Negative UN vacancy discriminant");
        const double root = std::sqrt(disc);
        double       value;
        if (B >= 0.0)
            value = 0.5 * (B + root);
        else
        {
            const double denominator = root - B;
            value                    = denominator <= 0.0 ? 0.0 : (2.0 * dt * A * C) / denominator;
        }
        if (p.vacancy_absorption_only)
            value = std::max(value, old);
        return {value, (value - old) / dt};
    }
}  // namespace un_l3
