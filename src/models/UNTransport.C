#include "UNLookupTables.h"
#include "UNTransport.h"
#include <algorithm>
#include <cmath>
#include <limits>
#include <stdexcept>

namespace un_l3
{
    double safeExp(double x)
    {
        return std::exp(std::max(-745.0, std::min(700.0, x)));
    }

    double lookupDiffusivity(double temperature, const std::vector<std::pair<double, double>>& table)
    {
        if (temperature <= 0.0 || table.size() < 2)
            throw std::invalid_argument("Invalid UN diffusivity lookup input");
        const double x = 1e4 / temperature;
        auto         it =
            std::lower_bound(table.begin(), table.end(), std::make_pair(x, -std::numeric_limits<double>::infinity()));
        std::size_t j = static_cast<std::size_t>(it - table.begin());
        if (j == 0)
            j = 1;
        if (j >= table.size())
            j = table.size() - 1;
        const auto   a = table[j - 1], b = table[j];
        const double y = a.first == b.first ? 0.5 * (a.second + b.second)
                                            : a.second + (x - a.first) * (b.second - a.second) / (b.first - a.first);
        return std::pow(10.0, y);
    }

    Diffusivity xeDiffusivity(const Parameters& p)
    {
        const double k = p.kB_eV * p.temperature, nan = std::numeric_limits<double>::quiet_NaN();
        double       d1 = 0.0, d2 = 0.0, d3 = 0.0, total = 0.0;
        if (p.xe_diffusivity_mode == "rizk2025_refit_plot")
        {
            d1 = 2.967341817979e-03 * std::exp(-5.013616576464 / k);
            d2 = std::sqrt(p.fission_rate) * 4.498475254045e-68 *
                 safeExp(-(-17.90671812685) / k - 0.9255235831189 / (k * k));
            d3    = 1.189275430019e-38 * p.fission_rate;
            total = p.Dg_D1_scale * d1 + p.D2_xe_scale * d2 + p.Dg_D3_scale * d3;
        }
        else if (p.xe_diffusivity_mode == "rizk2025_table_D1D3")
        {
            d1 = p.D10 * std::exp(-p.Q1 / k);
            d2 = std::sqrt(p.fission_rate) * p.A20_xe *
                 safeExp(-p.B21_xe / k - p.B22_xe / (k * k) - p.B23_xe / std::pow(k, 3));
            d3    = p.A30 * p.fission_rate;
            total = p.Dg_D1_scale * d1 + p.Dg_D3_scale * d3;
        }
        else if (p.xe_diffusivity_mode == "matthews2025_fig42_lookup")
        {
            d1 = d2 = nan;
            d3      = p.A30 * p.fission_rate;
            total   = lookupDiffusivity(p.temperature, xeLookupTable()) + p.Dg_D3_scale * d3;
        }
        else if (p.xe_diffusivity_mode == "rizk2023_table_raw")
        {
            d1 = 6.12e-5 * std::exp(-5.76 / k) + 7.54e-4 * std::exp(-5.76 / k);
            d2 =
                std::sqrt(p.fission_rate) * 3.40e-12 * safeExp(-27.67 / k + 5.28 / (k * k) - 0.302 / std::pow(k, 3)) +
                std::sqrt(p.fission_rate) * 1.45e-33 * safeExp(1.80 / k - 0.156 / (k * k)) +
                std::sqrt(p.fission_rate) * 2.06e-33 * safeExp(0.408 / k + 0.0798 / (k * k) - 0.00940 / std::pow(k, 3));
            d3    = 8.2e-42 * p.fission_rate;
            total = p.Dg_D1_scale * d1 + p.D2_xe_scale * d2 + p.Dg_D3_scale * d3;
        }
        else
            throw std::invalid_argument("Unknown UN Xe diffusivity mode");
        total *= p.Dg_scale * p.precursor_factor * p.Dg_extra_scale;
        return {total, d1, d2, d3, total * p.Dg_dislocation_scale};
    }

    Diffusivity vacancyDiffusivity(const Parameters& p)
    {
        const double k = p.kB_eV * p.temperature;
        double       d1, d2, total;
        if (p.vacancy_diffusivity_mode == "rizk2025_refit_full")
        {
            d1 = 1.122978768506e-02 * std::exp(-5.596873538604 / k);
            d2 = std::sqrt(p.fission_rate) * 7.805188680989e-28 *
                 safeExp(-0.9932675113163 / k - 0.02082395503235 / (k * k));
        }
        else if (p.vacancy_diffusivity_mode == "rizk2025_code_refit_old")
        {
            d1 = p.D10_vU * std::exp(-p.Q1_vU / k);
            d2 = std::sqrt(p.fission_rate) * p.A20_vU * safeExp(p.B21_vU / k + p.B22_vU / (k * k));
        }
        else if (p.vacancy_diffusivity_mode == "rizk2025_refit_A20_only")
        {
            d1 = 1.35e-2 * std::exp(-5.66 / k);
            d2 = std::sqrt(p.fission_rate) * 1.386341579723e-28 * safeExp(-0.62 / k - 0.04 / (k * k));
        }
        else if (p.vacancy_diffusivity_mode == "rizk2023_table_raw")
        {
            d1 = 2.11e-4 * std::exp(-6.04 / k);
            d2 = std::sqrt(p.fission_rate) * 8.12e-42 * safeExp(6.01 / k - 0.543 / (k * k) + 0.0124 / std::pow(k, 3));
        }
        else if (p.vacancy_diffusivity_mode == "matthews2025_DU_fig42_lookup")
        {
            d1    = 3.25e-4 * std::exp(-5.95 / k);
            d2    = std::numeric_limits<double>::quiet_NaN();
            total = lookupDiffusivity(p.temperature, vacancyLookupTable()) * p.Dv_scale;
            return {total, d1, d2, 0.0, total * p.Dv_dislocation_scale};
        }
        else
            throw std::invalid_argument("Unknown UN vacancy diffusivity mode");
        total = (p.Dv_D1_scale * d1 + p.Dv_D2_scale * d2) * p.Dv_scale;
        return {total, d1, d2, 0.0, total * p.Dv_dislocation_scale};
    }

    double grainBoundaryDiffusivity(const Parameters& p, const PhysicsOptions& o, double first)
    {
        if (o.gb_diffusivity_mode == "miller2025_LAUR2528152")
            return p.temperature <= 0.0
                       ? 0.0
                       : std::max(o.gb_arrhenius_D0 * std::exp(-o.gb_arrhenius_Q / (p.kB_eV * p.temperature)), 0.0);
        if (o.gb_diffusivity_mode != "rizk_legacy_1e6_Dv1")
            throw std::invalid_argument("Unknown UN grain-boundary diffusivity mode");
        if (p.vacancy_diffusivity_mode == "matthews2025_DU_fig42_lookup")
            first = p.temperature <= 0.0 ? 0.0 : 1.35e-2 * std::exp(-5.66 / (p.kB_eV * p.temperature));
        return std::max(o.gb_multiplier * std::max(first, 0.0), 0.0);
    }

    std::pair<double, double> resolutionRates(const Parameters& p, double rb, double rd)
    {
        const auto b0 = [](double r) { return 1e-25 * (2.64 - 2.02 * std::exp(-2.61e-9 / std::max(r, 1e-15))); };
        return {p.fission_rate * b0(rb + p.radius_in_lattice) * p.b_scale * p.b_bulk_scale,
                p.fission_rate * b0(rd + p.radius_in_lattice) * p.b_scale * p.b_dislocation_scale};
    }

    Trapping
    trappingRates(const Parameters& p, const PhysicsOptions& o, double dg, double rb, double nb, double rd, double nd)
    {
        const auto ws = [&](double r, double n)
        {
            if (n <= 0.0 || r <= 0.0)
                return 1.0;
            const double delta = std::pow(3.0 / (4.0 * M_PI * n), 1.0 / 3.0);
            const double x     = std::min(std::max(r / delta, 0.0), 1.0 - 1e-9);
            const double z     = 1.0 - 1.5 * x + 0.5 * std::pow(x, 3);
            const double f     = z <= 0.0 || !std::isfinite(z) ? o.ws_max_factor : (1.0 - std::pow(x, 3)) / z;
            return std::min(std::max(f, 0.0), o.ws_max_factor);
        };
        Trapping t;
        t.gamma       = 1.0 / std::sqrt(M_PI * p.rho_d);
        t.denominator = std::log(t.gamma / (p.Z_d * p.r_d)) - 3.0 / 5.0;
        if (t.denominator <= 0.0)
            throw std::domain_error("Invalid UN dislocation sink denominator");
        t.free_line   = std::max(p.rho_d - p.gd_line_alpha * 2.0 * rd * nd, 0.0);
        t.bubble_term = 4.0 * M_PI * dg * (rd + p.radius_in_lattice) * nd *
                        (o.ws_trapping_dislocation ? ws(rd + p.radius_in_lattice, nd) : 1.0);
        t.line_term   = (2.0 * M_PI * (dg * p.Dg_dislocation_scale) / t.denominator) * t.free_line;
        t.bulk        = p.gb_scale * (nb <= 0.0 ? 0.0
                                                : 4.0 * M_PI * dg * (rb + p.radius_in_lattice) * nb *
                                               (o.ws_trapping_bulk ? ws(rb + p.radius_in_lattice, nb) : 1.0));
        t.dislocation = p.gd_scale * (p.gd_bubble_scale * t.bubble_term + p.gd_line_scale * t.line_term);
        return t;
    }
    double productionRate(const Parameters& p)
    {
        return p.xe_yield * p.fission_rate;
    }
    double nucleationRate(const Parameters& p, double dg, double c)
    {
        return 8.0 * M_PI * p.f_n * dg * std::pow(p.omega_fg, 1.0 / 3.0) * std::pow(std::max(c, 0.0), 2);
    }
}  // namespace un_l3
