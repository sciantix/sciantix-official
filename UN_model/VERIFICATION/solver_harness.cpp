// Verification-only harness: the production Solver.C is linked unchanged.
#include "Solver.h"
#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <vector>

int main(int argc, char** argv)
{
    try
    {
        if (argc != 4) throw std::invalid_argument("harness config.txt output.bin timing.json");
        std::ifstream input(argv[1]);
        std::size_t n, stride;
        double dt, end, diffusion, radius, gb, gd, bb, bd;
        std::array<double,3> source, initial, means;
        input >> n >> dt >> end >> stride >> diffusion >> radius >> gb >> gd >> bb >> bd;
        for (double& value : source) input >> value;
        for (double& value : initial) input >> value;
        if (!input || n == 0 || n > 65535 || dt <= 0 || end <= 0 || stride == 0 || radius <= 0 ||
            diffusion < 0 || gb < 0 || gd < 0 || bb < 0 || bd < 0 || std::ceil(end / dt) > 5000000)
            throw std::invalid_argument("Invalid verification configuration or >5 million steps");
        std::uint16_t endian = 1;
        if (*reinterpret_cast<unsigned char*>(&endian) != 1 || sizeof(double) != 8)
            throw std::runtime_error("Output requires little-endian float64");
        const double projection = -2.0 * std::sqrt(2.0 / M_PI);
        std::array<std::vector<double>,3> modes;
        means.fill(0.0);
        for (std::size_t p = 0; p < 3; ++p)
        {
            modes[p].resize(n);
            for (std::size_t i = 0; i < n; ++i)
            {
                const double q = projection * std::pow(-1.0, static_cast<double>(i + 1)) / (i + 1);
                modes[p][i] = q * initial[p]; // Orthogonal projection, not mean renormalization.
                means[p] += q * modes[p][i] / ((4.0 / 3.0) * M_PI);
            }
        }
        std::ofstream output(argv[2], std::ios::binary);
        if (!output) throw std::runtime_error("Cannot open output");
        auto write = [&](double time) {
            output.write(reinterpret_cast<const char*>(&time), sizeof(double));
            output.write(reinterpret_cast<const char*>(means.data()), 3 * sizeof(double));
            for (const auto& population : modes)
                output.write(reinterpret_cast<const char*>(population.data()), n * sizeof(double));
        };
        const std::vector<double> parameters = {static_cast<double>(n), diffusion, radius, source[0],
            gb, gd, bb, bd, source[1], source[2]};
        Solver solver;
        double time = 0, elapsed = 0;
        std::size_t steps = 0, rows = 1;
        write(time);
        while (time < end)
        {
            const double h = std::min(dt, end - time);
            const auto start = std::chrono::steady_clock::now();
            solver.SpectralDiffusion3equationsExchange(means[0], means[1], means[2],
                modes[0].data(), modes[1].data(), modes[2].data(), parameters, h);
            elapsed += std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
            time += h;
            ++steps;
            if (steps % stride == 0 || time >= end) { write(time); ++rows; }
        }
        if (!output) throw std::runtime_error("Cannot write output");
        // Probe the actual initialization routine separately, without changing
        // the orthogonally projected initial data used in the benchmark.
        std::vector<double> seeded(n, 0.0);
        solver.modeInitialization(static_cast<int>(n), 1.0, seeded.data());
        double seeded_mean = 0;
        for (std::size_t i = 0; i < n; ++i)
            seeded_mean += projection * std::pow(-1.0, static_cast<double>(i + 1)) / (i + 1) * seeded[i] / ((4.0 / 3.0) * M_PI);
        std::ofstream timing(argv[3]);
        timing << std::setprecision(17) << "{\"solver_seconds\":" << elapsed
               << ",\"steps\":" << steps << ",\"rows\":" << rows
               << ",\"mode_initialization_mean\":" << seeded_mean << "}\n";
        return 0;
    }
    catch (const std::exception& error) { std::cerr << error.what() << '\n'; return 1; }
}
