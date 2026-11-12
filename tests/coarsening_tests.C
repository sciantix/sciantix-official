#include "IntraGranularBubbleBehavior.h"
#include <iostream>
#include <stdexcept>

using namespace intragranular_coarsening;

void check(bool condition, const char* message)
{
    if (!condition)
        throw std::runtime_error(message);
}

int main()
{
    // # COARSENING: initialization survives shutdown, losses and population extinction.
    auto seed = initializePopulation(0, false, 0);
    check(!seed.initialized && seed.number == 0, "No seeding before irradiation");
    seed = initializePopulation(seed.number, seed.initialized, 1e19);
    check(seed.initialized && seed.number == K * rho, "First irradiation seeds the population");
    seed = initializePopulation(seed.number / 2, seed.initialized, 1e19);
    check(seed.number == K * rho / 2, "No replenishment after population losses");
    seed = initializePopulation(seed.number, seed.initialized, 0);
    seed = initializePopulation(seed.number, seed.initialized, 1e19);
    check(seed.number == K * rho / 2, "No reseeding on irradiation resume");
    seed = initializePopulation(0, seed.initialized, 1e19);
    check(seed.number == 0 && seed.initialized, "No reseeding after extinction");

    check(numberCorrection(1) == 0 && numberCorrection(3) == .5, "Eq. (22) occupancy correction");
    check(resolution(100e-9, 1) < resolution(1e-9, 1), "Size-dependent resolution in nm");
    double vacancies = uptake(0, 1e6, 4e19, 2000, 0, 100);
    const double v_eq = bubbleVolume(1e6, vacancies, 2000);
    check(vacancies > 0 && std::abs(pressure(1e6, v_eq, 2000) / equilibrium(radius(v_eq), 0) - 1) < 1e-4,
          "Uptake stops at mechanical equilibrium");
    check(uptake(0, 1e6, 4e19, vacancy_activation_temperature - 1, 0, 100) == 0,
          "No vacancy uptake below the activation temperature");
    const double old = (volume(249.9e-9) - 1e6 * gasVolume(2000)) / atomic_volume;
    vacancies = uptake(old, 1e6, 1e18, 2000, 1e6, 100);
    check(radius(bubbleVolume(1e6, vacancies, 2000)) <= 250e-9 * (1 + 1e-12), "Absolute uptake limit");
    check(std::abs(gasVolume(2000) / (4 * pi / 6 * std::pow(hardSphereDiameter(2000), 3)) - 1) < 1e-12,
          "CS gas co-volume equals four hard-sphere volumes");
    std::cout << "Coarsening kernels passed\n";
}
