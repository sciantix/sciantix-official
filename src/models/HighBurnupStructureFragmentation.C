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
//  Authors: E. Cappellari                                                          //
//  author: E.Cappellari, POLIMI-CEA 2026                                           //
//////////////////////////////////////////////////////////////////////////////////////

// Fine fragmentation of the high-burnup structure (HBS), iHighBurnupStructureFragmentation = 1, 2, 3, 4
// (1 Jernkvist, 2 Kulacsy, 3 LEFM: utilities/HBFF/archive/README_options123.md; 4 rupture per pore class:
// utilities/HBFF/README.md).
//
// The model reads the HBS pore state of HighBurnupStructurePorosity (option 2 or 3), T and the hydrostatic pressure
// of input_history.txt (P_h = -Hydrostatic stress), and returns per step
//   D  fraction of the HBS phase whose pores are broken (pore-number weighted, drives the fragment size),
//   G  fraction of the pore gas released by the broken pores (gas weighted, drives the release),
//   d  mean fragment size.
// Option 4 also writes the state of its pore classes: the broken fraction of each class and the retained fraction of
// the second moment of the gas per pore.
//
// Tags: [P] paper, [R] reduction into the SCIANTIX state, [U] application to the HBS material,
//       [N] numerics, [?] unknown parameter, [E] deviation from or error in a source.
//
// References
//   J19  L.O. Jernkvist, Modelling of fine fragmentation and fission gas release of UO2 fuel in accident conditions,
//        EPJ Nucl. Sci. Technol. 5 (2019) 11, doi:10.1051/epjn/2019030
//   J20  L.O. Jernkvist, A review of analytical criteria for fission gas induced fragmentation of oxide fuel in
//        accident conditions, Prog. Nucl. Energy 119 (2020) 103188, doi:10.1016/j.pnucene.2019.103188
//   K15  K. Kulacsy, Mechanistic model for the fragmentation of the high-burnup structure during LOCA,
//        J. Nucl. Mater. 466 (2015) 409, doi:10.1016/j.jnucmat.2015.08.015
//   OHP  OperaHPC Deliverable D5.2 (2026), section 2.2 (NFIR annealing test, RVE with HBS pores)
//   KH20 G. Khvostov, Analytical criteria for fuel fragmentation and burst FGR during a LOCA, Nucl. Eng. Technol. 52
//        (2020) 2402 (fragment size from the specific surface of the cracks)
//   IR57 G.R. Irwin, J. Appl. Mech. 24 (1957) 361 (penny-shaped crack under internal pressure), as in J20 Eq. 8
//
// --------------------------------------------------------------------------------------------------
// Limitations
//   L1 [R]  the pressure is uniform over the pore classes in option 3; in option 4 it follows the closure
//           n_i ~ R_i^a, a gas density ~ R^(a - 3): a = 2 is a density ~ 1/R (for an ideal gas a pressure ~ 1/R,
//           the shape of the punching overpressure of K15 and J19), a = 3 a uniform pressure.
//   L2 [R]  options 1, 2, 3: the venting scales the gas by (1 - dG) and its variance by the square, as if every pore
//   lost the same fraction of its gas. Option 4 vents the broken classes only. L3 [E]  options 1, 3, 4 use the
//   hard-sphere EoS of the porosity model; option 2 uses van der Waals in place of
//           Ronchi. Ronchi (1981) gives the EoS as a table (T, V) -> P to be interpolated (Kulacsy 3.1.2), not a
//           closed form: pending digitisation of that table, van der Waals stays as the documented deviation.
//   L4 [?]  the fragments are equiaxed; HBS fragments are acicular (NEA/CSNI/R(2016)16, 3.1.3).
//   L5 [?]  no lower bound on the fragment size (a 40 um HBS fragment does not fragment further at 1300 C).
//   L6 [N]  D and G are cumulative maxima.
//   L7 [?]  option 2 starts the transient at the first step in which the fission rate falls, and keeps the state of the
//   step before as the reference (T_ss, P_h,ss); if the fission rate falls below 1 % of that value and later returns
//   above 50 % of it, the power-off was an outage and the reference state is tracked again. A power step-down that
//   never reaches 1 % is read as the transient start for the rest of the history. L8 [?]  the porosity model relaxes
//   the vented pores (capillary shrinkage; with partial venting the survivors
//           lose gas).
//   L9 [?]  the criteria act only for a mean pore radius of at least 0.1 um and a porosity below 0.35; the window
//           is on the mean pore, the classes of option 4 extend below 0.1 um.
//   L10 [?] the free parameter of option 3 (G_hbs) is fitted on one dataset (NFIR Kr-85 release, OHP Fig. 9, a
//           slow anneal), so the threshold at which it ruptures under a P_h transient is unvalidated. Options 1, 2
//           and 4 are not fitted.
//   L11 [R] the pore state that SCIANTIX builds (iHighBurnupStructureFormation = 4, porosity 3: about 10.5 %
//           porosity, 0.35 um radius, 73 MPa at the end of the NFIR base irradiation) differs from the measurement
//           (11 %, 0.88 um, gas density 1.7 times higher): the criteria act on the modelled state.
//   L12 [?] option 2 uses only xi, T, P_h of the SCIANTIX state: its classes are the fixed log-normal of K15.
//   L13 [?] the confining stress of the base irradiation is an input of the history (no PCMI model) and changes the
//           pore state through the equilibrium pressure of the porosity model.
//   L14 [?] the IFA-650 cases are one rim point with a transient taken from another test: no radial or axial
//           structure, no cladding strain, no real T and P_h traces.
//   L15 [?] no heating-rate dependence except the relaxation time of option 4; only the HBS is fragmented.
//   L16 [?] option 4: how gas and size vary together across the classes is not determined by the SCIANTIX moments
//           and sets which classes break: the pressure closure puts every class at the same fraction f of the way from
//           equilibrium to the punching limit c_dp / R, c_dp = G b (NEA), at every state (T, P_h), so that in a
//           transient the gas profile across the classes adapts at once to T and P_h; one pair of
//           bubbles in one population (Dowek 2021) has a gas far less dependent on R than any closure;
//           without input_hbs_shat.txt the stress concentration factor is the mean-field one of J19 (porosity only,
//           no dependence on the class and no scatter); with it, the s_hat of MMM is a surrogate of RVE runs at zero
//           macroscopic stress (P_h enters through the net pressure only), its inputs are clipped to the range of
//           the runs, and its sigma_R is calibrated on the NFIR anneal; the relaxation time tau = 2 s is borrowed from
//           the J19 interconnection and microcracking modes (J19 rupture is instantaneous); the pores do not interact
//           after a break; the classes keep their rank in the size distribution while the moments evolve, and the
//           unvented moments follow the retained ones in proportion between two steps.

#include "Simulation.h"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

namespace
{
    // s_hat of MMM for option 4 [P] [N]: the surrogate over the Latin hypercube of utilities/HBFF/mmm_surrogate.py
    // --fit, read from input_hbs_shat.txt in the folder of the case. Without that file option 4 keeps the mean field of
    // J19. In every class of pores ln s_hat ~ normal(mu, sig), with mu and ln sig = f(z), z_d = (x_d - mean_d) / std_d,
    // the inputs x_d (xi, s, eta_bar, u) clipped to the range of the runs:
    //    f(z) = beta_0 + sum_d beta_d z_d + scale * amplitude2 * sum_j alpha_j exp(-0.5 sum_d ((z_d - z_jd) /
    //    length_d)^2)
    // (linear trend + Gaussian process with an RBF kernel). The file carries sigma_R (Pa), which pairs with this s_hat.
    struct ShatFunction
    {
        std::vector<double> beta, length, points, alpha;  // points: one row of the standardised inputs per point
        double              scale = 0.0, amplitude2 = 0.0;

        double operator()(const std::vector<double>& z) const
        {
            const std::size_t d     = z.size();
            double            trend = beta[0], gp = 0.0;
            for (std::size_t k = 0; k < d; ++k)
                trend += beta[k + 1] * z[k];
            for (std::size_t j = 0; j < alpha.size(); ++j)
            {
                double r2 = 0.0;
                for (std::size_t k = 0; k < d; ++k)
                {
                    const double t = (z[k] - points[j * d + k]) / length[k];
                    r2 += t * t;
                }
                gp += alpha[j] * std::exp(-0.5 * r2);
            }
            return trend + scale * amplitude2 * gp;
        }
    };

    struct ShatSurrogate
    {
        bool                     active  = false;
        double                   sigma_r = 0.0;  // (Pa)
        std::vector<std::string> inputs;         // names, among xi, s, eta_bar, u
        std::vector<double>      mean, spread, lower, upper;
        ShatFunction             mu, ln_sig;

        // mu and sig of ln s_hat at the inputs x (in the order of `inputs`), clipped to the range of the runs if clip
        void predict(std::vector<double> x, bool clip, double& mu_out, double& sig_out) const
        {
            std::vector<double> z(x.size());
            for (std::size_t k = 0; k < x.size(); ++k)
            {
                if (clip)
                    x[k] = std::min(std::max(x[k], lower[k]), upper[k]);
                z[k] = (x[k] - mean[k]) / spread[k];
            }
            mu_out  = mu(z);
            sig_out = std::exp(ln_sig(z));
        }
    };

    // Read once, at the first call: input_hbs_shat.txt is the file mmm_surrogate.py writes (keywords and numbers; '#'
    // starts a comment). Its test points are evaluated here and the run stops if they do not give the mu and sig of the
    // file.
    const ShatSurrogate& shat_surrogate()
    {
        static ShatSurrogate surrogate;
        static bool          read = false;
        if (read)
            return surrogate;
        read = true;

        const std::string name = "input_hbs_shat.txt";
        std::ifstream     file(TestPath + name);
        if (!file)
            return surrogate;

        std::vector<std::string> tokens;
        std::string              line, token;
        while (std::getline(file, line))
        {
            std::istringstream words(line.substr(0, line.find('#')));
            while (words >> token)
                tokens.push_back(token);
        }
        std::size_t at   = 0;
        auto        fail = [&](const std::string& what)
        { ErrorMessages::Fatal(__FILE__, name + ": " + what + " (token " + std::to_string(at) + ")"); };
        auto word = [&](const std::string& expected)
        {
            if (at >= tokens.size() || tokens[at] != expected)
                fail("expected '" + expected + "'");
            ++at;
        };
        auto number = [&]()
        {
            if (at >= tokens.size())
                fail("the file ends too early");
            std::istringstream value_stream(tokens[at]);
            double             value = 0.0;
            if (!(value_stream >> value))
                fail("not a number: '" + tokens[at] + "'");
            ++at;
            return value;
        };
        auto numbers = [&](std::size_t count)
        {
            std::vector<double> values(count);
            for (double& value : values)
                value = number();
            return values;
        };

        word("sigma_R");
        surrogate.sigma_r = number();
        word("inputs");
        const std::size_t d = std::size_t(number());
        for (std::size_t k = 0; k < d; ++k)
        {
            if (at >= tokens.size())
                fail("the file ends too early");
            const std::string input = tokens[at++];
            if (input != "xi" && input != "s" && input != "eta_bar" && input != "u")
                fail("unknown input '" + input + "'");
            surrogate.inputs.push_back(input);
        }
        word("mean");
        surrogate.mean = numbers(d);
        word("std");
        surrogate.spread = numbers(d);
        word("lower");
        surrogate.lower = numbers(d);
        word("upper");
        surrogate.upper = numbers(d);
        for (const std::string output : {"mu", "lnsig"})
        {
            ShatFunction& f = output == "mu" ? surrogate.mu : surrogate.ln_sig;
            word("output");
            word(output);
            word("beta");
            f.beta = numbers(d + 1);
            word("scale");
            f.scale = number();
            word("amplitude2");
            f.amplitude2 = number();
            word("length");
            f.length = numbers(d);
            word("points");
            const std::size_t n_points = std::size_t(number());
            for (std::size_t j = 0; j < n_points; ++j)
            {
                const std::vector<double> row = numbers(d + 1);
                f.points.insert(f.points.end(), row.begin(), row.end() - 1);
                f.alpha.push_back(row.back());
            }
        }
        word("tests");
        const std::size_t n_tests = std::size_t(number());
        for (std::size_t j = 0; j < n_tests; ++j)
        {
            const std::vector<double> x   = numbers(d);
            const double              mu  = number();
            const double              sig = number();
            double                    mu_cpp, sig_cpp;
            surrogate.predict(x, false, mu_cpp, sig_cpp);
            if (std::abs(mu_cpp - mu) > 1.0e-9 * (1.0 + std::abs(mu)) || std::abs(sig_cpp / sig - 1.0) > 1.0e-9)
                fail("test point " + std::to_string(j) + " gives mu, sig = " + std::to_string(mu_cpp) + ", " +
                     std::to_string(sig_cpp) + " instead of " + std::to_string(mu) + ", " + std::to_string(sig));
        }
        if (at != tokens.size())
            fail("unexpected text after the test points");
        surrogate.active = true;
        return surrogate;
    }
}  // namespace

void Simulation::HighBurnupStructureFragmentation()
{
    const int option = int(input_variable["iHighBurnupStructureFragmentation"].getValue());
    if (!option)
        return;

    if (int(input_variable["iHighBurnupStructurePorosity"].getValue()) != 2 &&
        int(input_variable["iHighBurnupStructurePorosity"].getValue()) != 3)
        ErrorMessages::Fatal(__FILE__,
                             "iHighBurnupStructureFragmentation reads the pore state of "
                             "iHighBurnupStructurePorosity = 2 or 3; other values do not compute it.");

    // Model declaration
    Model model_;
    model_.setName("High-burnup structure fragmentation");

    Matrix fuel_(matrices["UO2HBS"]);

    // New restructured material is intact [R]: when the restructured volume fraction alpha grows, what is broken is
    // diluted by alpha_old / alpha.
    const double alpha_old = sciantix_variable["Restructured volume fraction"].getInitialValue();
    const double alpha     = sciantix_variable["Restructured volume fraction"].getFinalValue();
    const double dilution  = (alpha > alpha_old && alpha_old > 0.0) ? alpha_old / alpha : 1.0;

    // The gas of the pores that already vented was removed from the state, [R] L2
    double retention_old     = sciantix_variable["HBS gas retention fraction"].getInitialValue() > 0.0
                                   ? sciantix_variable["HBS gas retention fraction"].getInitialValue()
                                   : 1.0;
    double retention2_old    = sciantix_variable["HBS gas retention fraction - second moment"].getInitialValue() > 0.0
                                   ? sciantix_variable["HBS gas retention fraction - second moment"].getInitialValue()
                                   : 1.0;
    double vent_first_moment = 1.0, vent_second_moment = 1.0;

    // option 4: the vented shares of the moments, 1 - S and 1 - S_2, are diluted as the broken classes [R]
    if (option == 4 && dilution < 1.0)
    {
        retention_old  = 1.0 - (1.0 - retention_old) * dilution;
        retention2_old = 1.0 - (1.0 - retention2_old) * dilution;
    }

    const double T  = history_variable["Temperature"].getFinalValue();
    const double Ph = -history_variable["Hydrostatic stress"].getFinalValue() * 1.0e6;  // (Pa)
    const double xi = sciantix_variable["HBS porosity"].getFinalValue();
    const double R  = sciantix_variable["HBS pore radius"].getFinalValue();
    const double V  = sciantix_variable["HBS pore volume"].getFinalValue();
    const double n  = sciantix_variable["Xe atoms per HBS pore"].getFinalValue() / retention_old;
    const double varn =
        sciantix_variable["Xe atoms per HBS pore - variance"].getFinalValue() / (retention_old * retention_old);

    // Pore pressure [R], hard-sphere (Carnahan-Starling) EoS
    const double xe_diameter   = 4.45e-10 * (0.8542 - 0.03996 * std::log(T / 231.2));  // (m)
    const double xe_volume     = M_PI / 6.0 * std::pow(xe_diameter, 3.0);              // (m3)
    const double eta_cap       = 0.65;           // cap of the hard-sphere packing fraction
    auto         hard_sphere_Z = [](double eta)  // Z(eta)
    { return (1.0 + eta + eta * eta - eta * eta * eta) / std::pow(1.0 - eta, 3.0); };
    auto pore_pressure = [&](double atoms, double volume)
    {
        if (volume <= 0.0 || atoms <= 0.0)
            return 0.0;
        const double eta = std::min(atoms * xe_volume / volume, eta_cap);  // adimensional
        return atoms * boltzmann_constant * T * hard_sphere_Z(eta) / volume;
    };
    const double pg = pore_pressure(n, V);

    // last-cycle reference state of K15: it stops updating at the transient start [?] L7
    double T_ss  = sciantix_variable["HBFF reference temperature"].getInitialValue();
    double Ph_ss = sciantix_variable["HBFF reference hydrostatic pressure"].getInitialValue();
    // HBFF transient flag:
    //      0         = the reference state is tracked;
    //      F_ref > 0 = transient since the step in which the fission rate fell from F_ref;
    //      -F_ref    = the fission rate is also below 1 % of F_ref (power off).
    //                  If the power then returns above 50 % of F_ref the reference state is tracked again.
    // The fission rate carries a relative noise of about 1e-9 where the history changes T or P_h at constant
    // power: the 1 % and 50 % thresholds are not sensitive to it, the start test (any fall) is.
    double transient = sciantix_variable["HBFF transient flag"].getInitialValue();
    if (option == 2)
    {
        const double fission_rate     = history_variable["Fission rate"].getFinalValue();
        const double fission_rate_old = history_variable["Fission rate"].getInitialValue();

        if (transient > 0.0 && fission_rate < 0.01 * transient)
            transient = -transient;
        else if (transient < 0.0 && fission_rate >= -0.5 * transient)
            transient = 0.0;

        if (transient == 0.0)
        {
            const bool start = fission_rate < fission_rate_old;
            // first step in which the fission rate falls: the reference state is the last one at full power, whether
            // the history switches the fission rate off in one step or ramps it down together with the temperature
            if (start)
                transient = fission_rate_old;
            if (!start || T_ss == 0.0)
            {
                T_ss  = T;
                Ph_ss = Ph;
            }
        }
    }
    sciantix_variable["HBFF reference temperature"].setFinalValue(T_ss);
    sciantix_variable["HBFF reference hydrostatic pressure"].setFinalValue(Ph_ss);
    sciantix_variable["HBFF transient flag"].setFinalValue(transient);

    // [?] validity window of the criteria
    const double r_min_eval = 1.0e-7;  // (m) K15 covers 0.1 to 2 um;
    const double xi_max_eval =
        0.35;  // (/) MATPRO strength vanishes at xi = 0.38; HBS porosity saturates below 0.3 (J19)
    const bool state_valid = (R >= r_min_eval && xi > 0.0 && xi < xi_max_eval);

    // Parameter definition: each option resolves the instantaneous criterion into (D*, G*), the pore-number and
    // gas-weighted fractions broken this step (0 outside the validity window, or before option 2's transient starts).
    double d_star = 0.0, g_star = 0.0;

    std::string reference;

    switch (option)
    {
        case 1:
        {
            // Option 1: Jernkvist (2019) [P]
            // J19 Eq. 22 = J20 Eq. 7 with phi_2 -> phi_3.
            //
            // Force balance across the plane (Olander 1997, J20 Eq. 7) with the areal fraction replaced by the porosity
            // xi and the far-field stress by -P_h (J19 Eq. 22):
            //     P_cr = P_s + [ sigma_hbs^cr (1 - xi) + P_h ] / xi,
            //     sigma_hbs^cr = 21 MPa (J19 Table 4).
            // The rupture releases the whole population.
            // Rupture when p_g >= P_cr on the mean pore: D* = G* = 1.
            reference = ": Jernkvist (2019), EPJ Nucl. Sci. Technol. 5, 11, Eq. 22";

            if (state_valid)
            {
                // [P] J19 Table 4
                const double sigma_hbs_cr = 21.0e6;  // (Pa)
                const double p_cr         = 2.0 * fuel_.getSurfaceTension() / R + (sigma_hbs_cr * (1.0 - xi) + Ph) / xi;

                if (pg >= p_cr)
                {
                    d_star = 1.0;
                    g_star = 1.0;
                }
            }
            break;
        }

        case 2:
        {
            // Option 2: Kulacsy (2015) [P]
            // Before the transient every class of radius r is at the dislocation punching pressure (K15 Eq. 3, 12-14):
            //     p_0(r) = P_h,ss + 2 gamma_ss / r + G b / r
            //  T_ss and P_h,ss are the last-cycle state; the gas per class n_i follows from p_0 with the EoS at T_ss.
            //  Stress and strength (K15 Eq. 7, 11, 15):
            //     sigma_t = p_i - (3/2 P_h + 2 gamma(T, xi) / r),
            //     gamma(T, xi) = 0.41 (0.85 - 1.4e-4 T) (1 - xi)^4.025 N/m
            //     sigma_f(T, xi) = 170 MPa exp(-191.34 / min(T, 1000)) sqrt(1 - 2.62 xi) (MATPRO FFRACS)
            //  Classes are log-normal with median exp(-0.5) um and sigma_lnr = 0.356 (K15 Table 2).
            //  The classes below the smallest stable pore do not exist. With w_i the renormalised weights:
            //     D* = sum_i w_i [sigma_t,i > sigma_f],
            //     G* = sum_i w_i n_i [sigma_t,i > sigma_f] / sum_i w_i n_i.
            reference = ": Kulacsy (2015), J. Nucl. Mater. 466, 409";

            if (state_valid && transient != 0.0)
            {
                const double burgers = 0.39e-9;  // (m) K15, 3.2
                const double kul_m   = -0.5;     // K15 Table 2, r in um
                const double kul_s   = 0.356;    // K15 Table 2

                // [R] [N] radii (m) and number weights of the log-normal classes: 81 nodes at ln R = ln R_mean +/- 4 sigma
                const int    n_classes  = 81;
                const double class_span = 4.0;
                const double median     = std::exp(kul_m) * 1.0e-6;  // [P] r in um

                std::vector<double> radii, w;
                double              w_norm = 0.0;
                for (int i = 0; i < n_classes; ++i)
                {
                    const double x = -class_span + (2.0 * class_span) * i / (n_classes - 1);
                    radii.push_back(median * std::exp(kul_s * x));
                    w.push_back(std::exp(-0.5 * x * x));
                    w_norm += w.back();
                }
                for (double& weight : w)
                    weight /= w_norm;

                // base-irradiation state, no porosity dependence [P] K15 3.1.1.
                const double gamma_ss = 0.41 * (0.85 - 1.4e-4 * T_ss);
                const double e_ss     = 2.334e11 * (1.0 - 1.0915e-4 * T_ss);
                const double g_ss     = e_ss / (2.0 * (1.0 + fuel_.getPoissonRatio()));

                // [P] K15 Eq. 15, T capped at 1000 K (MATPRO)
                const double kulacsySigmaF_ss =
                    170.0e6 * std::exp(-191.34 / std::min(T_ss, 1000.0)) * std::sqrt(std::max(1.0 - 2.62 * xi, 0.0));

                // smallest stable pore: sigma_t = Gb/r - Ph/2 = sigma_f at T_ss [P] K15 4.1.1, Eq. 8
                const double r_min = g_ss * burgers / (kulacsySigmaF_ss + 0.5 * Ph_ss);

                double w_sum = 0.0;
                for (size_t i = 0; i < radii.size(); ++i)
                    if (radii[i] >= r_min)
                        w_sum += w[i];

                if (w_sum > 0.0)
                {
                    double w_broken = 0.0, gas_sum = 0.0, gas_broken = 0.0;
                    for (size_t i = 0; i < radii.size(); ++i)
                    {
                        if (radii[i] < r_min)
                            continue;
                        const double weight = w[i] / w_sum;
                        const double Vc     = 4.0 / 3.0 * M_PI * std::pow(radii[i], 3.0);
                        const double p0 =
                            Ph_ss + 2.0 * gamma_ss / radii[i] + g_ss * burgers / radii[i];  // [P] K15 Eq. 3

                        // During the transient the pores are rigid and p_i(T) follows the EoS.
                        // Van der Waals replaces the Ronchi EoS of K15 [E]:
                        //     p = n k_B T / (V - n b_w) - a_w n^2 / V^2
                        // K15, 3.1.2: "The gas content of the pores is calculated using Ronchi's equation of state for
                        // xenon [31]", given as a table (T, V) -> P to be interpolated; pending digitisation of that
                        // table, van der Waals implementation is temporary.
                        const double vdw_a = 1.17e-48;  // (J m3) per atom pair: a = 4.25 L^2 bar/mol2
                        const double vdw_b = 8.49e-29;  // (m3) per atom: b = 0.0511 L/mol

                        // [N] inverse of the van der Waals pressure at fixed V, T_ss, by bisection
                        double lo = 0.0, hi = 0.99 * Vc / vdw_b;
                        for (int it = 0; it < 100; ++it)
                        {
                            const double mid   = 0.5 * (lo + hi);
                            const double p_mid = (mid > 0.0) ? mid * boltzmann_constant * T_ss / (Vc - mid * vdw_b) -
                                                                   vdw_a * mid * mid / (Vc * Vc)
                                                             : 0.0;
                            if (p_mid < p0)
                                lo = mid;
                            else
                                hi = mid;
                        }
                        const double n_i = 0.5 * (lo + hi);
                        const double p_i = (n_i > 0.0) ? n_i * boltzmann_constant * T / (Vc - n_i * vdw_b) -
                                                             vdw_a * n_i * n_i / (Vc * Vc)
                                                       : 0.0;

                        // [P] K15 Eq. 11 (N/m)
                        const double kulacsyGamma = 0.41 * (0.85 - 1.4e-4 * T) * std::pow(1.0 - xi, 4.025);
                        // K15 Eq. 7, 1.0 factor on the net pressure; J20 gives 1/2 for a spherical pore [E]
                        // J20: "we note that Kulacsy used the rupture criterion in Eq. (5) in her recent study on rim
                        // zone fragmentation during LOCA (Kulacsy, 2015). She assumed that the overpressurized rim zone
                        // pores were spherical, but obviously used F1 = 1 instead of the correct value F1 = 1_2."
                        const double sigma_t = p_i - 2.0 * kulacsyGamma / radii[i] - 1.5 * Ph;
                        gas_sum += weight * n_i;

                        // [P] K15 Eq. 15, T capped at 1000 K (MATPRO)
                        const double kulacsySigmaF = 170.0e6 * std::exp(-191.34 / std::min(T, 1000.0)) *
                                                     std::sqrt(std::max(1.0 - 2.62 * xi, 0.0));
                        if (sigma_t > kulacsySigmaF)
                        {
                            w_broken += weight;
                            gas_broken += weight * n_i;
                        }
                    }
                    d_star = w_broken;
                    g_star = gas_broken / gas_sum;
                }
            }
            break;
        }

        case 3:
        {
            // Option 3: LEFM [P] penny crack, [R] capillarity, [?] interaction factor
            // Penny-shaped crack of radius R under the pore pressure (IR57, J20 Eq. 8):
            //     P_cr(R) = P_h + 2 gamma / R + (1 / (2 Y)) sqrt( pi E G_hbs / ((1 - nu^2) R) )
            // IR57/J20 Eq. 8 is derived for a flat penny-shaped crack (R1, R2 -> infinity), so it has no capillary
            // term. A real pore is closer to a filled cavity than to a flat crack, so the capillary pressure 2 gamma /
            // R is added;
            //  P_cr decreases with R: the pores with R >= R_c, P_cr(R_c) = p_g, break (bisection on R_c).
            // With z = ln(R_c / R_mean) / sigma_lnR:
            //   D* = (1/2) erfc(z / sqrt(2))
            //   G* = (1/2) erfc((z - 3 sigma_lnR) / sqrt(2))
            //   G* is the third moment of the log-normal: at uniform pressure the gas is proportional to R^3 [R].
            reference = ": LEFM, Jernkvist (2020), Prog. Nucl. Energy 119, 103188, Eq. 8";

            if (state_valid && pg > 0.0)
            {
                // G_hbs is fitted on the NFIR Kr-85 release (OHP Fig. 9, 29.8 % at 1200 C) [?]; with the bulk 2 J/m2 of
                // SetMatrix.C the criterion gives hundreds of MPa and cannot break the pores (J20: the local G is below
                // the bulk G).
                const double g_hbs = 0.055;  // (J/m2)

                // The interaction factor Y (J20 gives F3 in [2, 2.61] for a similar geometry dependence in the Harwell
                // criterion, Eq. 9) is left at 1, unfitted [?]:
                const double y_factor = 1.0;

                // [R] UO2HBS elastic modulus of SetMatrix.C (Pa), recomputed here from the current xi, T, Bu:
                // matrices["UO2HBS"].getElasticModulus() would stay frozen at the t = 0 state for the whole run [N].
                const double burnup = sciantix_variable["Burnup"].getFinalValue();
                const double E      = 2.237e11 * (1.0 - 2.6 * xi) * (1.0 - 1.394e-4 * (T - 273.0 - 20.0)) *
                                 (1.0 - 0.1506 * (1.0 - std::exp(-0.035 * burnup)));

                const double c =
                    0.5 * std::sqrt(M_PI * E * g_hbs / (1.0 - fuel_.getPoissonRatio() * fuel_.getPoissonRatio())) /
                    y_factor;
                const double target = pg - Ph;
                if (target > 0.0)
                {
                    double lo = 1.0e-12, hi = 1.0e-3;
                    if (c / std::sqrt(hi) + 2.0 * fuel_.getSurfaceTension() / hi < target)
                    {
                        for (int it = 0; it < 100;
                             ++it)  // [N] bisection on the decreasing function c/sqrt(R) + 2 gamma / R
                        {
                            const double mid = std::sqrt(lo * hi);
                            if (c / std::sqrt(mid) + 2.0 * fuel_.getSurfaceTension() / mid >= target)
                                lo = mid;
                            else
                                hi = mid;
                        }
                        const double r_c = std::sqrt(lo * hi);

                        //   Pore classes [R] [N]. SCIANTIX keeps two moments of n (mean n and Var(n)); the radius is
                        //   log-normal, R ~ n^(1/3):
                        //     CV_R = sqrt(Var(n)) / (3 n),   sigma_lnR = sqrt(ln(1 + CV_R^2)),
                        double sigma_lnR = 0.0;
                        if (n > 0.0 && varn > 0.0)
                        {
                            const double cv_r = std::sqrt(varn) / n / 3.0;
                            sigma_lnR         = std::sqrt(std::log(1.0 + cv_r * cv_r));
                        }

                        if (sigma_lnR <= 0.0)
                        {
                            if (R >= r_c)
                            {
                                d_star = 1.0;
                                g_star = 1.0;
                            }
                        }
                        else
                        {
                            const double z = (std::log(r_c) - std::log(R)) / sigma_lnR;
                            d_star         = 0.5 * std::erfc(z / std::sqrt(2.0));  // number fraction above R_c
                            g_star         = 0.5 * std::erfc((z - 3.0 * sigma_lnR) /
                                                     std::sqrt(2.0));  // third moment of the log-normal
                        }
                    }
                }
            }
            break;
        }

        case 4:
        {
            // Option 4: rupture per pore class, progressive [R] [P] [?] (utilities/HBFF/README.md, section 3). The
            // venting of the broken classes and the dilution of the retained moments are applied after the switch.
            const ShatSurrogate& mmm = shat_surrogate();
            reference =
                mmm.active
                    ? ": pore classes from the SCIANTIX moments; rupture per class with the stress concentration "
                      "factor of the micromechanics (MMM surrogate of input_hbs_shat.txt); progressive rupture as "
                      "GrainBoundaryMicroCracking"
                    : ": pore classes from the SCIANTIX moments; rupture per class with the mean-field stress "
                      "concentration factor of Jernkvist (2019), EPJ Nucl. Sci. Technol. 5, 11, Eq. 22; "
                      "progressive rupture as GrainBoundaryMicroCracking";

            // broken fraction beta_i of each class; new restructured material is intact [R]: beta_i x alpha_old / alpha
            std::vector<double> beta(HBFF_N_CLASSES);
            for (int i = 0; i < HBFF_N_CLASSES; ++i)
                beta[i] =
                    sciantix_variable["HBS broken fraction - class " + std::to_string(i)].getInitialValue() * dilution;

            if (state_valid && pg > 0.0)
            {
                // (Pa) [P] J19 Table 4, pairs with the mean-field s_hat; with the s_hat of MMM, the sigma_R of its file
                const double sigma_r     = mmm.active ? mmm.sigma_r : 21.0e6;
                const double xi_perc     = 0.29;  // (/) [P] J19, percolation of the HBS pores: every class open above
                const double tau_rupture = 2.0;   // (s) [?]
                // (N/m) [P] punching limit of the overpressure of a pore, c_dp / R with c_dp = G b (Greenwood et al.,
                // J. Nucl. Mater. 1 (1959) 305; J19 2.3.2): G of dense UO2 at T (Pu = 0, x = 0, porosity 0: the matrix
                // at the pore wall) from NEA/NSC/R(2024)1 Eq. 7.2; b = a / sqrt(2), the Burgers vector 1/2 <110> of the
                // fluorite, with a = 4 / sqrt(3) (r_U4+ + r_O2-) of UO2 (NEA/NSC/R(2024)1 2.2, Table 2.3). 30 N/m at
                // 903 K; J19 Table 4 takes 55 N/m, the fit of Gao et al. (2013) to HBS porosity data, about 2 G b.
                const double burgers = 4.0 / std::sqrt(3.0) * (0.9972e-10 + 1.372e-10) / std::sqrt(2.0);  // (m)
                const double c_dp    = 82.52e9 * (1.009549 - 1.182e-5 * T - 6.671e-8 * T * T) * burgers;
                const double class_span = 4.0;  // [N] nodes at ln R = ln R_median +/- 4 s
                const double gamma      = fuel_.getSurfaceTension();

                // [P] mean-field stress concentration factor per unit net pressure of J19 Eq. 22, the same for all the
                // classes (without input_hbs_shat.txt)
                const double s_hat = xi / (1.0 - xi);

                // SCIANTIX carries the mean pore volume V, the mean Xe atoms per pore n and Var(n).
                // Classes [N]: the radius is log-normal on 81 nodes x_i in [-4, 4]
                std::vector<double> x(HBFF_N_CLASSES), w(HBFF_N_CLASSES);
                double              w_norm = 0.0;
                for (int i = 0; i < HBFF_N_CLASSES; ++i)
                {
                    x[i] = -class_span + (2.0 * class_span) * i / (HBFF_N_CLASSES - 1);
                    w[i] = std::exp(-0.5 * x[i] * x[i]);  // number weights w_i ~ exp(-x_i^2 / 2)
                    w_norm += w[i];
                }
                for (double& weight : w)
                    weight /= w_norm;

                // The criterion sees the unvented moments [R]: n / S (n above) and (Var(n) + n^2) / S_2 - n^2, with S
                // and S_2 the retained shares of the first and second moment
                const double n_retained   = sciantix_variable["Xe atoms per HBS pore"].getFinalValue();
                const double var_retained = sciantix_variable["Xe atoms per HBS pore - variance"].getFinalValue();
                const double var_unvented =
                    std::max((var_retained + n_retained * n_retained) / retention2_old - n * n, 0.0);

                // Pressure closure [?] (README 3.2): at the current state (T, P_h) every class is at the same fraction f
                // of the way from the equilibrium P_h + 2 gamma / R_i (pore fed by vacancies) to the punching limit
                // P_h + (2 gamma + c_dp) / R_i (J19 Eq. 21):
                //     p_i = P_h + (2 gamma + f c_dp) / R_i,     n_i = EoS^-1(p_i, V_i, T),
                // on the radii of the log-normal
                //     R_i = R_V exp(s x_i) / (sum_j w_j exp(3 s x_j))^(1/3),   R_V = (3 V / 4 pi)^(1/3),
                // so that sum_i w_i V_i = V for any s. f and s are the roots of sum_i w_i n_i = n and
                // sum_i w_i (n_i - n)^2 = Var(n): the three moments hold on the classes. f is not bounded (f > 1: more
                // gas than punching allows; f = 0: equilibrium). The net pressure of a class is exactly f c_dp / R_i.
                const double kT    = boltzmann_constant * T;
                const double y_cap = eta_cap * hard_sphere_Z(eta_cap);

                // Xe atoms of a pore of volume `volume` at the pressure p and T, and dn/dp: the inverse of
                // pore_pressure, linear above the cap, 0 for p <= 0 [N]. y = eta Z(eta) is increasing and convex with
                // eta + 4 eta^2 <= eta Z(eta) (the virial coefficients of Z are positive): Newton from the root of
                // eta + 4 eta^2 = y, on the right of the root, converges monotonically
                const double z_cap = hard_sphere_Z(eta_cap);
                auto         gas_at    = [&](double p, double volume, double& dn_dp)
                {
                    dn_dp = 0.0;
                    if (p <= 0.0)
                        return 0.0;
                    const double y = p * xe_volume / kT;
                    if (y >= y_cap)
                    {
                        dn_dp = volume / (kT * z_cap);
                        return p * dn_dp;
                    }
                    double eta   = std::min((std::sqrt(1.0 + 16.0 * y) - 1.0) / 8.0, eta_cap);
                    double slope = 1.0;  // d(eta Z) / d eta
                    for (int it = 0; it < 50; ++it)
                    {
                        const double om  = 1.0 - eta;
                        const double om3 = om * om * om;
                        const double num = 1.0 + eta + eta * eta - eta * eta * eta;
                        const double z   = num / om3;
                        slope = z + eta * ((1.0 + 2.0 * eta - 3.0 * eta * eta) * om + 3.0 * num) / (om3 * om);
                        const double step = (eta * z - y) / slope;
                        eta -= step;
                        if (step <= 1.0e-15 * eta)
                            break;
                    }
                    dn_dp = volume / (kT * slope);
                    return eta * volume / xe_volume;
                };

                // root of an increasing function F with F(lo) < 0 <= F(hi) [N]: regula falsi with the Illinois
                // modification, until |F| <= 1e-13 or the bracket is at round-off
                auto increasing_root = [](auto&& F, double lo, double hi)
                {
                    double F_lo = F(lo), F_hi = F(hi), root = hi;
                    int    side = 0;
                    for (int it = 0; it < 200 && F_hi > 0.0; ++it)
                    {
                        root               = (lo * F_hi - hi * F_lo) / (F_hi - F_lo);
                        const double F_mid = F(root);
                        if (std::abs(F_mid) <= 1.0e-13 || hi - lo <= 1.0e-15 * (std::abs(lo) + std::abs(hi)))
                            break;
                        if (F_mid < 0.0)
                        {
                            lo   = root;
                            F_lo = F_mid;
                            if (side == -1)
                                F_hi *= 0.5;
                            side = -1;
                        }
                        else
                        {
                            hi   = root;
                            F_hi = F_mid;
                            if (side == 1)
                                F_lo *= 0.5;
                            side = 1;
                        }
                    }
                    return root;
                };

                std::vector<double> r(HBFF_N_CLASSES), v(HBFF_N_CLASSES), n_class(HBFF_N_CLASSES);
                double              r_median = 0.0;
                auto                set_radii = [&](double s)  // radii and volumes of the classes of width s
                {
                    double volume_norm = 0.0;
                    for (int i = 0; i < HBFF_N_CLASSES; ++i)
                        volume_norm += w[i] * std::exp(3.0 * s * x[i]);
                    r_median = std::cbrt(3.0 * V / (4.0 * M_PI) / volume_norm);
                    for (int i = 0; i < HBFF_N_CLASSES; ++i)
                    {
                        r[i] = r_median * std::exp(s * x[i]);
                        v[i] = 4.0 / 3.0 * M_PI * r[i] * r[i] * r[i];
                    }
                };
                // gas of the classes at the fraction f: returns F(f) = sum_i w_i n_i / n - 1, and dF/df in F_slope
                double F_slope = 0.0;
                auto   set_gas = [&](double f)
                {
                    double mean = 0.0, slope = 0.0, dn_dp = 0.0;
                    for (int i = 0; i < HBFF_N_CLASSES; ++i)
                    {
                        n_class[i] = gas_at(Ph + (2.0 * gamma + f * c_dp) / r[i], v[i], dn_dp);
                        mean += w[i] * n_class[i];
                        slope += w[i] * dn_dp * c_dp / r[i];
                    }
                    F_slope = slope / n;
                    return mean / n - 1.0;
                };
                // f of the mean gas on the current radii, n_class left at the root. F is increasing and concave in f:
                // Newton from the f of the previous width, kept inside the bracket (f_lo, f_hi) found on the way (f_lo:
                // below it every class has p <= 0 and no gas; f_hi: the smallest f seen with F > 0)
                double f_closure = 1.0;
                auto   solve_f   = [&]()
                {
                    double f_lo = 0.0, f_hi = HUGE_VAL;
                    for (int i = 0; i < HBFF_N_CLASSES; ++i)
                        f_lo = std::min(f_lo, -(Ph * r[i] + 2.0 * gamma) / c_dp);
                    double f = f_closure > f_lo ? f_closure : f_lo + 1.0;
                    double F = set_gas(f);
                    for (int it = 0; it < 200 && std::abs(F) > 1.0e-13; ++it)
                    {
                        if (F < 0.0)
                            f_lo = f;
                        else
                            f_hi = f;
                        if (f_hi < HUGE_VAL && f_hi - f_lo <= 1.0e-15 * (std::abs(f_lo) + std::abs(f_hi)))
                            break;
                        double f_new = F_slope > 0.0 ? f - F / F_slope : f_lo + 2.0 * (f - f_lo) + 1.0;
                        if (!(f_new > f_lo && f_new < f_hi))
                            f_new = f_hi < HUGE_VAL ? 0.5 * (f_lo + f_hi) : f_lo + 2.0 * (f - f_lo) + 1.0;
                        f = f_new;
                        F = set_gas(f);
                    }
                    f_closure = f;
                };

                // [N] s: the root of the relative excess of the variance, which grows from -1 at s = 0; the upper end
                // starts from the width of the continuous log-normal with n ~ R^3 and is doubled until it brackets the root
                const double cv2   = var_unvented / (n * n);
                double       sigma = 0.0;
                if (cv2 > 0.0)
                {
                    auto excess = [&](double s)
                    {
                        set_radii(s);
                        solve_f();
                        double m2 = 0.0;
                        for (int i = 0; i < HBFF_N_CLASSES; ++i)
                            m2 += w[i] * (n_class[i] - n) * (n_class[i] - n);
                        return m2 / (n * n) / cv2 - 1.0;
                    };
                    double s_hi = std::sqrt(std::log(1.0 + cv2)) / 3.0;
                    for (int it = 0; it < 20 && excess(s_hi) < 0.0; ++it)
                        s_hi *= 2.0;
                    sigma = increasing_root(excess, 0.0, s_hi);
                }
                set_radii(sigma);
                solve_f();

                // Progressive rupture [?]: each class relaxes towards b*_i with the time constant tau_rupture
                const double relax = 1.0 - std::exp(-physics_variable["Time step"].getFinalValue() / tau_rupture);

                // s_hat of MMM [P] [N] (with input_hbs_shat.txt): ln s_hat of the pores of a class ~ normal(mu, sig).
                // mu and sig come from the surrogate (shat_surrogate() above) at the 7 classes of an MMM run of this
                // state, where the surrogate was fitted, and are interpolated linearly in u = ln(R / R_V) on the
                // nodes, constant beyond the end classes.
                std::vector<double> mu_node(HBFF_N_CLASSES, 0.0), sig_node(HBFF_N_CLASSES, 1.0);
                if (mmm.active)
                {
                    const int           mmm_classes = 7;
                    const double        r_v         = std::cbrt(3.0 * V / (4.0 * M_PI));
                    const double        eta_bar     = n * xe_volume / V;
                    std::vector<double> u_class(mmm_classes), mu_class(mmm_classes), sig_class(mmm_classes);
                    for (int k = 0; k < mmm_classes; ++k)
                    {
                        // nodes of MMM class k: x in [k - 3.5, k - 2.5], half of a node on an inner edge; R of the
                        // class from its mean volume
                        const double lo = k - 3.5, hi = k - 2.5;  // edges in x; the end classes are open
                        double       weight = 0.0, volume = 0.0;
                        for (int i = 0; i < HBFF_N_CLASSES; ++i)
                        {
                            double share = 0.0;
                            if (std::abs(x[i] - lo) < 1.0e-6 && k > 0)
                                share = 0.5;
                            else if (std::abs(x[i] - hi) < 1.0e-6 && k < mmm_classes - 1)
                                share = 0.5;
                            else if ((k == 0 || x[i] > lo) && (k == mmm_classes - 1 || x[i] < hi))
                                share = 1.0;
                            weight += w[i] * share;
                            volume +=
                                w[i] * share * 4.0 / 3.0 * M_PI * std::pow(r_median * std::exp(sigma * x[i]), 3.0);
                        }
                        u_class[k] = std::log(std::cbrt(3.0 * volume / weight / (4.0 * M_PI)) / r_v);

                        // inputs of the surrogate by name, clipped to the range of the runs by predict()
                        std::vector<double> inputs;
                        for (const std::string& input : mmm.inputs)
                            inputs.push_back(input == "xi"        ? xi
                                             : input == "s"       ? sigma
                                             : input == "eta_bar" ? eta_bar
                                                                  : u_class[k]);
                        mmm.predict(inputs, true, mu_class[k], sig_class[k]);
                    }
                    for (int i = 0; i < HBFF_N_CLASSES; ++i)
                    {
                        const double u_i = std::log(r_median * std::exp(sigma * x[i]) / r_v);
                        int          k   = 0;
                        while (k < mmm_classes - 2 && u_i > u_class[k + 1])
                            ++k;
                        const double span = u_class[k + 1] - u_class[k];
                        const double f    = span > 0.0 ? std::min(std::max((u_i - u_class[k]) / span, 0.0), 1.0) : 0.5;
                        mu_node[i]        = mu_class[k] + f * (mu_class[k + 1] - mu_class[k]);
                        sig_node[i]       = sig_class[k] + f * (sig_class[k + 1] - sig_class[k]);
                    }
                }

                double gas_broken = 0.0, kept_old = 0.0, kept_new = 0.0, kept2_old = 0.0, kept2_new = 0.0;
                for (int i = 0; i < HBFF_N_CLASSES; ++i)
                {
                    const double r_i = r[i];
                    const double n_i = n_class[i];
                    const double p_i = pore_pressure(n_i, v[i]);  // [R] hard-sphere EoS on (n_i, V_i), at T

                    // Rupture of class i: first principal stress at the pore wall above the rupture stress (OHP 2.2.2),
                    //     s_hat (p_i - 2 gamma / R_i - P_h) >= sigma_R + P_h.
                    // b*_i, the share of the class that meets it: 1 for xi >= xi_perc [E] (J19 vents only the excess
                    // over c_dp / R); 0 or 1 with the mean field of J19; with the s_hat of MMM
                    //     b*_i = Phi((mu_i - ln t_i) / sig_i),   t_i = (sigma_R + P_h) / (p_i - 2 gamma / R_i - P_h),
                    // and 0 if the net pressure is not positive.
                    const double net    = p_i - 2.0 * gamma / r_i - Ph;
                    double       b_star = 0.0;
                    if (xi >= xi_perc)
                        b_star = 1.0;
                    else if (!mmm.active)
                        b_star = s_hat * net >= sigma_r + Ph ? 1.0 : 0.0;
                    else if (net > 0.0)
                        b_star = sigma_r + Ph <= 0.0 ? 1.0
                                                     : 0.5 * std::erfc(-(mu_node[i] - std::log((sigma_r + Ph) / net)) /
                                                                       (sig_node[i] * std::sqrt(2.0)));

                    // beta_i <- beta_i + max(b*_i - beta_i, 0) (1 - exp(-dt / tau)): a broken pore never heals.
                    // Retained first and second moment of n, sum_i w_i n_i (1 - beta_i) and
                    // sum_i w_i n_i^2 (1 - beta_i), before and after the rupture of this step
                    kept_old += w[i] * n_i * (1.0 - beta[i]);
                    kept2_old += w[i] * n_i * n_i * (1.0 - beta[i]);
                    beta[i] += std::max(b_star - beta[i], 0.0) * relax;
                    kept_new += w[i] * n_i * (1.0 - beta[i]);
                    kept2_new += w[i] * n_i * n_i * (1.0 - beta[i]);

                    // D = sum_i w_i beta_i,   G = sum_i w_i n_i beta_i / sum_i w_i n_i   (sum_i w_i n_i = n)
                    d_star += w[i] * beta[i];
                    gas_broken += w[i] * n_i * beta[i];
                }
                g_star = gas_broken / n;

                // Venting [R]: the gas leaves the broken classes only; the factors f_1, f_2 of the retained moments
                // are applied after the switch
                if (kept_old > 0.0)
                    vent_first_moment = kept_new / kept_old;
                if (kept2_old > 0.0)
                    vent_second_moment = kept2_new / kept2_old;
            }

            for (int i = 0; i < HBFF_N_CLASSES; ++i)
                sciantix_variable["HBS broken fraction - class " + std::to_string(i)].setFinalValue(beta[i]);
            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iHighBurnupStructureFragmentation", option);
            break;
    }

    std::vector<double> parameter;
    parameter.push_back(d_star);
    parameter.push_back(g_star);
    model_.setParameter(parameter);
    model_.setRef(reference);
    model.push(model_);

    // Model resolution
    //   Broken fractions [N] [U]. With alpha the restructured volume fraction and (D*, G*) the instantaneous result of
    //   the criterion:
    //     if alpha > alpha_old:  D <- D alpha_old / alpha,  G <- G alpha_old / alpha   (new restructured material is
    //     intact) D <- max(D, D*),  G <- max(G, G*)  (a broken pore never heals)
    double D = sciantix_variable["HBS fragmented fraction"].getInitialValue() * dilution;
    double G = sciantix_variable["HBS burst release fraction"].getInitialValue() * dilution;

    D                  = std::max(D, model["High-burnup structure fragmentation"].getParameter().at(0));
    const double G_new = std::max(G, model["High-burnup structure fragmentation"].getParameter().at(1));

    //   Venting [R]. A broken pore vents its gas and stays as a void. A = "Xe in HBS pores", B its variance,
    //   S = "HBS gas retention fraction". The gas leaves through "Xe released" by the mass balance of GasRelease.C.
    //   Options 1, 2, 3: when G grows from G_old to G_new, with f = max[(1 - G_new) / (1 - G_old), 1e-12 / S],
    //     A -> f A,  B -> f^2 B,  n -> f n,  Var(n) -> f^2 Var(n),  S -> f S,
    //   and the criteria are evaluated on the state before the venting, n / S and Var(n) / S^2.
    //   Option 4: the broken classes vent, with f_1 and f_2 the factors of the retained first and second moment of n,
    //     A -> f_1 A,  n -> f_1 n,  S -> f_1 S,  S_2 -> f_2 S_2,
    //     Var(n) -> f_2 (Var(n) + n^2) - (f_1 n)^2,  B -> N_p Var(n),
    //   on S and S_2 already diluted with the classes.
    // [N] the gas retention of the population never falls below this value
    const double retention_floor = 1.0e-12;
    double       retention       = retention_old;
    double       retention2      = retention2_old;
    if (option == 4)
    {
        if (vent_first_moment < 1.0)
        {
            const double f1         = std::max(vent_first_moment, retention_floor / retention_old);
            const double f2         = std::max(vent_second_moment, retention_floor / retention2_old);
            const double n_retained = sciantix_variable["Xe atoms per HBS pore"].getFinalValue();
            const double second_moment =
                (sciantix_variable["Xe atoms per HBS pore - variance"].getFinalValue() + n_retained * n_retained) * f2;
            const double variance = std::max(second_moment - (f1 * n_retained) * (f1 * n_retained), 0.0);
            retention *= f1;
            retention2 *= f2;
            sciantix_variable["Xe in HBS pores"].rescaleFinalValue(f1);
            sciantix_variable["Xe atoms per HBS pore"].rescaleFinalValue(f1);
            sciantix_variable["Xe atoms per HBS pore - variance"].setFinalValue(variance);
            sciantix_variable["Xe in HBS pores - variance"].setFinalValue(
                variance * sciantix_variable["HBS pore density"].getFinalValue());
        }
    }
    else if (G_new > G)
    {
        // the gas of the population and its variance scale with the survivors [R] L2
        double factor = (G < 1.0) ? (1.0 - G_new) / (1.0 - G) : 1.0;
        factor        = std::min(1.0, std::max(factor, retention_floor / retention_old));
        retention *= factor;
        sciantix_variable["Xe in HBS pores"].rescaleFinalValue(factor);
        sciantix_variable["Xe in HBS pores - variance"].rescaleFinalValue(factor * factor);
        sciantix_variable["Xe atoms per HBS pore"].rescaleFinalValue(factor);
        sciantix_variable["Xe atoms per HBS pore - variance"].rescaleFinalValue(factor * factor);
    }
    G = G_new;

    sciantix_variable["HBS fragmented fraction"].setFinalValue(D);
    sciantix_variable["HBS burst release fraction"].setFinalValue(G);
    sciantix_variable["HBS gas retention fraction"].setFinalValue(retention);
    sciantix_variable["HBS gas retention fraction - second moment"].setFinalValue(retention2);
    // the output is the pressure of the gas the pores hold after the venting; the criteria above used pg, before it
    sciantix_variable["HBS pore pressure"].setFinalValue(
        pore_pressure(sciantix_variable["Xe atoms per HBS pore"].getFinalValue(), V));

    // Fragment size [R] [U]
    // Cubic fragments bounded by the cracked faces of the Wigner-Seitz cells of the pores (specific surface of the
    // cracks, as in KH20 Eq. 2-4 for the vented pores of the grain face).
    const double n_pores = sciantix_variable["HBS pore density"].getFinalValue();
    double       d       = 0.0;
    if (D > 1.0e-9 && n_pores > 0.0)
    {
        // R_WS = (3 / (4 pi N_p))^(1/3)
        const double r_ws = std::pow(3.0 / (4.0 * M_PI * n_pores), 1.0 / 3.0);

        // A_f = 2 pi R_WS^2 [?] is the cracked area per broken pore: half of the surface of its Wigner-Seitz sphere,
        // each face being shared with the neighbouring cell. D -> 1 gives d = 2 R_WS, the cell diameter. d_max = 500 um
        // [U] (above, pellet cracks, NEA/CSNI/R(2016)16 section 3); d = 0 if D < 1e-9.
        const double a_f   = 2.0 * M_PI * r_ws * r_ws;  // (m2)
        const double d_max = 5.0e-4;                    // (m)

        // S_v = N_p D A_f,   d = min(3 / S_v, d_max)
        d = std::min(3.0 / (n_pores * D * a_f), d_max);
    }
    sciantix_variable["HBS fragment size"].setFinalValue(d);
}
