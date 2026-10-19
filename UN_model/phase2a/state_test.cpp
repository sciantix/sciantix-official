// State ownership/validation tests. Numerical Python equivalence is checked
// separately with frozen fixtures, not inferred from these lifecycle tests.
#include "Solver.h"
#include "UNIntraGranular.h"

#include <cmath>
#include <functional>
#include <iostream>
#include <limits>
#include <stdexcept>

namespace
{
void check(bool condition, const char* message)
{
    if (!condition)
        throw std::runtime_error(message);
}

void invalid(const std::function<void()>& operation)
{
    try { operation(); }
    catch (const std::invalid_argument&) { return; }
    throw std::runtime_error("Invalid input was not rejected");
}

void call(Solver& solver, un_l3::State& state, const un_l3::ExchangeCoefficients& coefficients)
{
    un_l3::advanceExchange(solver, state, coefficients, 10800.0);
}
}

int main()
{
    try
    {
        Solver solver;
        const un_l3::ExchangeCoefficients coefficients = {1e-19, 6e-6, 1e19, 1e-4, 2e-4, 1e-5, 2e-5, 0.0, 0.0};
        un_l3::State a(160), b(40);
        for (std::size_t population = 0; population < 3; ++population)
            for (std::size_t mode = 0; mode < a.numberOfModes(); ++mode)
                a.modes[population][mode] = (population + 1.0) * (mode % 2 ? -1.0 : 1.0) * 1e20 / (mode + 1.0);
        a.bulk.number_density = 1e20;
        a.bulk.radius = 1e-9;
        a.bulk.volume = 1e-27;
        a.bulk.vacancies = 2e22;
        a.dislocation.number_density = 2e19;
        a.dislocation.radius = 1e-8;
        a.dislocation.volume = 1e-24;
        a.dislocation.vacancies = 3e23;
        a.grain_face.number_areal = 1e13;
        a.grain_face.gas = 2e22;
        a.grain_face.released = 3e21;
        a.grain_face.vacancies = 4e24;
        a.grain_face.saturation_reached = true;
        a.bulk_first_gas_seed_done = true;
        a.dislocation_first_gas_seed_done = false;
        a.initial_gas = 5e20;
        a.generated = 6e25;
        a.gas_to_boundary = 7e22;
        a.time_s = 21600.0;
        un_l3::State uninterrupted = a;
        const un_l3::State metadata = a;
        const double* address = a.modes[2].data();
        for (int i = 0; i < 8; ++i)
        {
            call(solver, a, coefficients);
            call(solver, b, coefficients);  // another material point, different mode count
        }
        for (int i = 0; i < 8; ++i)
            call(solver, uninterrupted, coefficients);
        check(a.modes == uninterrupted.modes && a.averages == uninterrupted.averages,
              "Interleaved calls lost/corrupted the owned spectral state");
        check(a.modes[2].data() == address, "Exchange replaced/reallocated dislocation modes");
        check(a.bulk.number_density == metadata.bulk.number_density && a.bulk.radius == metadata.bulk.radius &&
              a.bulk.volume == metadata.bulk.volume && a.bulk.vacancies == metadata.bulk.vacancies &&
              a.dislocation.number_density == metadata.dislocation.number_density &&
              a.dislocation.radius == metadata.dislocation.radius && a.dislocation.volume == metadata.dislocation.volume &&
              a.dislocation.vacancies == metadata.dislocation.vacancies, "Exchange modified bubble microstructure");
        check(a.grain_face.number_areal == metadata.grain_face.number_areal &&
              a.grain_face.gas == metadata.grain_face.gas && a.grain_face.released == metadata.grain_face.released &&
              a.grain_face.vacancies == metadata.grain_face.vacancies && a.grain_face.saturation_reached,
              "Exchange modified grain-face inventories or saturation flag");
        check(a.bulk_first_gas_seed_done && !a.dislocation_first_gas_seed_done &&
              a.initial_gas == metadata.initial_gas && a.generated == metadata.generated &&
              a.gas_to_boundary == metadata.gas_to_boundary && a.time_s == metadata.time_s,
              "Exchange modified flags, cumulative inventories or time");
        a.dislocation_first_gas_seed_done = true;
        call(solver, a, coefficients);
        check(a.dislocation_first_gas_seed_done, "A later seed flag was lost across a call");
        un_l3::State copied = a;
        copied.modes[2][0] += 1e20;
        copied.bulk_first_gas_seed_done = false;
        check(copied.modes[2][0] != a.modes[2][0] && a.bulk_first_gas_seed_done, "State copy aliases source data");
        const auto zero_step_modes = a.modes;
        un_l3::advanceExchange(solver, a, coefficients, 0.0);
        check(a.modes == zero_step_modes, "Zero timestep changed modes");

        invalid([] { un_l3::State zero(0); });
        invalid([] { un_l3::State excessive(65536); });
        invalid([&] { un_l3::advanceExchange(solver, a, coefficients, -1.0); });
        auto bad = coefficients;
        bad.grain_radius = 0.0;
        invalid([&] { call(solver, a, bad); });
        bad = coefficients;
        bad.diffusivity = std::numeric_limits<double>::quiet_NaN();
        invalid([&] { call(solver, a, bad); });
        const auto before_invalid = a.modes;
        a.modes[1].pop_back();
        invalid([&] { call(solver, a, coefficients); });
        a.modes = before_invalid;
        a.modes[0][0] = std::numeric_limits<double>::infinity();
        invalid([&] { call(solver, a, coefficients); });
        std::cout << "PASS: owned/interleaved state, metadata/flags, independent copy, dt=0, validation\n";
        return 0;
    }
    catch (const std::exception& error)
    {
        std::cerr << error.what() << "\n";
        return 1;
    }
}
