#include "UNBubblePhysics.h"
#include "UNGrainFace.h"
#include "UNTransport.h"
#include <algorithm>
#include <cmath>
#include <stdexcept>
namespace un_l3
{
    namespace
    {
        double theta(const PhysicsOptions& o)
        {
            return o.gf_theta_degrees * (M_PI / 180.0);
        }
        double lens(const PhysicsOptions& o)
        {
            const double c = std::cos(theta(o));
            return 1.0 - 1.5 * c + 0.5 * (o.lenticular_cos3 ? std::pow(c, 3) : std::pow(c, 2));
        }
        double gfEquilibrium(const Parameters& p, double r)
        {
            return r <= 0.0 ? 1e30 : equilibriumPressure(p, r);
        }
    }  // namespace
    double grainFaceVolume(const PhysicsOptions& o, double r)
    {
        return (4.0 / 3.0) * M_PI * std::pow(std::max(r, 0.0), 3) * lens(o);
    }
    double grainFaceRadius(const PhysicsOptions& o, double v)
    {
        return v <= 0.0 ? 0.0 : std::pow(3.0 * std::max(v, 0.0) / (4.0 * M_PI * std::max(lens(o), 1e-30)), 1.0 / 3.0);
    }
    double grainFaceArea(const PhysicsOptions& o, double r)
    {
        return M_PI * std::pow(std::max(r, 0.0), 2) * std::pow(std::sin(theta(o)), 2);
    }
    double grainFaceZeta(double fc)
    {
        fc = std::min(std::max(fc, 1e-12), 0.999999999);
        return -(((3.0 - fc) * (1.0 - fc) + 2.0 * std::log(fc)) / 4.0);
    }
    void refreshGrainFace(const Parameters& p, const PhysicsOptions& o, GrainFaceState& s)
    {
        s.number_areal         = std::max(s.number_areal, 0.0);
        s.number_volumetric    = s.number_areal > 0.0 ? 3.0 * s.number_areal / (2.0 * p.grain_radius) : 0.0;
        s.atoms_per_bubble     = s.number_volumetric > 0.0 ? std::max(s.gas, 0.0) / s.number_volumetric : 0.0;
        s.vacancies_per_bubble = s.number_volumetric > 0.0 ? std::max(s.vacancies, 0.0) / s.number_volumetric : 0.0;
        s.volume               = p.omega_fg * s.atoms_per_bubble + matrixVolume(p) * s.vacancies_per_bubble;
        s.radius               = grainFaceRadius(o, s.volume);
        s.area                 = grainFaceArea(o, s.radius);
        s.coverage             = s.number_areal * s.area;
        s.zeta                 = grainFaceZeta(std::max(s.coverage, 1e-12));
        s.pressure             = s.atoms_per_bubble <= 0.0
                                     ? 0.0
                                     : (s.vacancies_per_bubble <= 0.0 ? 1e30
                                                                      : p.kB_J * p.temperature * s.atoms_per_bubble /
                                                                (s.vacancies_per_bubble * matrixVolume(p)));
        s.equilibrium_pressure = gfEquilibrium(p, s.radius);
    }
    GrainFaceState initializeGrainFace(const Parameters& p, const PhysicsOptions& o)
    {
        GrainFaceState s;
        s.number_areal      = o.gf_number_areal_initial;
        s.number_volumetric = 3.0 * s.number_areal / (2.0 * p.grain_radius);
        s.radius            = o.gf_radius_initial;
        s.volume            = grainFaceVolume(o, s.radius);
        s.area              = grainFaceArea(o, s.radius);
        s.vacancies = s.number_volumetric > 0.0 ? std::max(s.number_volumetric * s.volume / matrixVolume(p), 0.0) : 0.0;
        s.vacancies_per_bubble = s.number_volumetric > 0.0 ? s.vacancies / s.number_volumetric : 0.0;
        s.coverage             = s.number_areal * s.area;
        s.zeta                 = grainFaceZeta(std::max(s.coverage, 1e-12));
        s.equilibrium_pressure = gfEquilibrium(p, s.radius);
        return s;
    }
    double grainFaceLimitedGrowth(double nv, double growth, double eq, double dt)
    {
        const double x = nv + eq * dt, disc = x * x + 4.0 * std::max(growth, 0.0) * dt;
        return 0.5 * (x + std::sqrt(std::max(disc, 0.0)));
    }
    std::array<double, 3> solveGrainFace3x3(double v,
                                            double a,
                                            double n,
                                            double fc,
                                            double gas0,
                                            double gas1,
                                            double delta,
                                            double omega,
                                            double kappa,
                                            bool   saturated)
    {
        v                  = std::max(v, 1e-300);
        a                  = std::max(a, 1e-300);
        n                  = std::max(n, 1e-300);
        kappa              = std::max(kappa, 1e-300);
        const double beta  = (2.0 / 3.0) * a / v;
        const double gamma = saturated ? n / a : 6.0 * n * n / (3.0 + 4.0 * std::max(fc, 0.0));
        const double bn    = saturated ? 2.0 * n : n + gamma * a;
        const double alpha = saturated ? 0.0 : std::max(gas0, 0.0) / (n * n * kappa);
        const double bv    = saturated ? v + delta * omega + std::max(gas1 - gas0, 0.0) / (n * kappa)
                                       : v + delta * omega + std::max(gas1, 0.0) / (n * kappa);
        const double ba = a / 3.0, det = 1.0 - alpha * beta * gamma;
        if (!std::isfinite(det) || std::abs(det) < 1e-18)
            throw std::runtime_error("Singular UN grain-face 3x3");
        const double v1 = (bv - alpha * (bn - gamma * ba)) / det, a1 = ba + beta * v1, n1 = bn - gamma * a1;
        if (!std::isfinite(v1) || !std::isfinite(a1) || !std::isfinite(n1))
            throw std::runtime_error("Non-finite UN grain-face predictor");
        return {{v1, a1, n1}};
    }
    void advanceGrainFace(
        const Parameters& p, const PhysicsOptions& o, GrainFaceState& s, double incoming, double dt, double first)
    {
        incoming            = std::max(incoming, 0.0);
        dt                  = std::max(dt, 0.0);
        s.gas_in_step       = incoming;
        s.gas_released_step = 0.0;
        s.area_rate         = 0.0;
        s.number_rate       = 0.0;
        s.vacancy_rate      = 0.0;
        if (dt <= 0.0 || !o.intergranular_model)
            return;
        refreshGrainFace(p, o, s);
        const double q0 = std::max(s.gas, 0.0), rel0 = std::max(s.released, 0.0);
        const double n0 = std::max(s.number_areal, 1e-30), v0 = std::max(s.volume, 1e-300),
                     a0  = std::max(s.area, 1e-300);
        const double fc0 = std::max(s.coverage, 0.0), nv0 = std::max(s.vacancies_per_bubble, 0.0);
        const double kappa = 3.0 / (2.0 * p.grain_radius), qtrial = q0 + incoming, ng = qtrial / (kappa * n0);
        const bool   saturated = s.saturation_reached || fc0 >= o.gf_coverage_saturation * (1.0 - 1e-12);
        s.saturation_reached   = saturated;
        const double dv        = grainBoundaryDiffusivity(p, o, first);
        double       nv1       = nv0;
        if (dv > 0.0)
        {
            const double flow      = 2.0 * M_PI * dv * o.gf_delta_gb / std::max(s.zeta, 1e-300);
            const double growth    = flow * ng / std::max(matrixVolume(p), 1e-300),
                         eq        = -flow * gfEquilibrium(p, std::max(s.radius, 1e-300)) / (p.kB_J * p.temperature);
            const double candidate = grainFaceLimitedGrowth(nv0, growth, eq, dt);
            nv1                    = o.gf_vacancy_absorption_only ? std::max(candidate, nv0) : std::max(candidate, 0.0);
        }
        s.vacancy_rate       = (nv1 - nv0) / dt;
        const auto predictor = solveGrainFace3x3(
            v0, a0, n0, fc0, p.omega_fg * q0, p.omega_fg * qtrial, nv1 - nv0, matrixVolume(p), kappa, saturated);
        if (predictor[0] <= 0.0 || predictor[1] <= 0.0 || predictor[2] <= 0.0)
            throw std::runtime_error("Non-positive UN grain-face predictor");
        if (saturated)
        {
            const double volume   = p.omega_fg * ng + matrixVolume(p) * nv1,
                         area     = grainFaceArea(o, grainFaceRadius(o, volume));
            const double n1       = area >= a0 ? std::min(n0, o.gf_coverage_saturation / std::max(area, 1e-300)) : n0;
            const double survival = std::max(std::min(n1 / n0, 1.0), 0.0);
            s.gas                 = qtrial * survival;
            s.gas_released_step   = std::max(qtrial - s.gas, 0.0);
            s.released            = rel0 + s.gas_released_step;
            s.vacancies           = nv1 * (kappa * n1);
            s.number_areal        = n1;
            s.saturation_reached  = true;
            refreshGrainFace(p, o, s);
            if (area >= a0 && std::abs(s.coverage - o.gf_coverage_saturation) > 5e-10)
                throw std::runtime_error("UN grain-face saturation invariant lost");
        }
        else
        {
            s.gas          = qtrial;
            s.released     = rel0;
            s.number_areal = predictor[2];
            s.vacancies    = nv1 * (kappa * s.number_areal);
            refreshGrainFace(p, o, s);
            if (s.coverage >= o.gf_coverage_saturation)
            {
                const double before   = s.number_areal,
                             target   = std::min(before, o.gf_coverage_saturation / std::max(s.area, 1e-300));
                const double survival = std::max(std::min(target / before, 1.0), 0.0), qbefore = s.gas;
                s.gas *= survival;
                s.vacancies *= survival;
                s.number_areal       = target;
                s.gas_released_step  = std::max(qbefore - s.gas, 0.0);
                s.released           = rel0 + s.gas_released_step;
                s.saturation_reached = true;
                refreshGrainFace(p, o, s);
            }
        }
        s.area_rate   = (s.area - a0) / dt;
        s.number_rate = (s.number_areal - n0) / dt;
    }
}  // namespace un_l3
