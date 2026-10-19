// Test-only binary I/O driver. Links the actual SCIANTIX Solver.C; it contains
// no reimplementation of the 3x3 solve and computes no physical coefficients.
#include "Solver.h"
#include "UNIntraGranular.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

namespace
{
void read(std::ifstream& stream, double* data, std::size_t count)
{
    if (!stream.read(reinterpret_cast<char*>(data), count * sizeof(double)))
        throw std::runtime_error("Truncated fixture input");
}

void write(std::ofstream& stream, const double* data, std::size_t count)
{
    if (!stream.write(reinterpret_cast<const char*>(data), count * sizeof(double)))
        throw std::runtime_error("Cannot write fixture result");
}
}

int main(int argc, char** argv)
{
    try
    {
        if (argc != 5)
            throw std::invalid_argument("usage: un_l3_solver_fixture fixture_dir single|prefix|replay direct|state output.bin");
        const std::uint16_t endian = 1;
        if (*reinterpret_cast<const unsigned char*>(&endian) != 1 || sizeof(double) != 8)
            throw std::runtime_error("Fixture protocol requires little-endian float64");
        const std::string root(argv[1]), scenario(argv[2]), engine(argv[3]);
        if (scenario != "single" && scenario != "prefix" && scenario != "replay")
            throw std::invalid_argument("Unknown scenario");
        if (engine != "direct" && engine != "state")
            throw std::invalid_argument("Unknown engine");
        std::ifstream config(root + "/config.txt");
        std::size_t n_modes, n_steps, prefix_steps;
        if (!(config >> n_modes >> n_steps >> prefix_steps) || n_steps == 0 || prefix_steps == 0 || prefix_steps > n_steps)
            throw std::runtime_error("Invalid fixture dimensions");
        un_l3::State state(n_modes);
        Solver solver;
        std::ifstream coeff(root + "/coefficients.bin", std::ios::binary);
        std::ifstream input(root + "/input_modes.bin", std::ios::binary);
        std::ifstream interventions(root + "/interventions.bin", std::ios::binary);
        std::ofstream output(argv[4], std::ios::binary);
        if (!coeff || !input || !interventions || !output)
            throw std::runtime_error("Cannot open fixture files");
        const std::size_t count = scenario == "prefix" ? prefix_steps : n_steps;
        std::array<double, 10> c;
        std::vector<double> initial(3 * n_modes), delta(3 * n_modes);
        for (std::size_t step = 0; step < count; ++step)
        {
            read(coeff, c.data(), c.size());
            read(input, initial.data(), initial.size());
            read(interventions, delta.data(), delta.size());
            for (std::size_t population = 0; population < 3; ++population)
                for (std::size_t mode = 0; mode < n_modes; ++mode)
                {
                    const std::size_t offset = population * n_modes + mode;
                    if (scenario == "single" || step == 0)
                        state.modes[population][mode] = initial[offset];
                    else if (scenario == "replay")
                        // Frozen external intervention, not a capture law.
                        // Never reset/rebase a propagated C++ error to Python.
                        state.modes[population][mode] += delta[offset];
                }
            if (engine == "direct")
            {
                const std::vector<double> parameters = {
                    static_cast<double>(n_modes), c[1], c[2], c[3], c[4], c[5], c[6], c[7], c[8], c[9]};
                solver.SpectralDiffusion3equationsExchange(
                    state.averages[0], state.averages[1], state.averages[2],
                    state.modes[0].data(), state.modes[1].data(), state.modes[2].data(), parameters, c[0]);
            }
            else
            {
                const un_l3::ExchangeCoefficients parameters = {c[1], c[2], c[3], c[4], c[5], c[6], c[7], c[8], c[9]};
                un_l3::advanceExchange(solver, state, parameters, c[0]);
            }
            write(output, state.averages.data(), state.averages.size());
            for (const auto& population : state.modes)
                write(output, population.data(), population.size());
        }
        std::cout << count << " steps, " << n_modes << " modes, " << scenario << ", " << engine << "\n";
        return 0;
    }
    catch (const std::exception& error)
    {
        std::cerr << error.what() << "\n";
        return 1;
    }
}
