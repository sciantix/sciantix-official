#include "IntraGranularBubbleBehavior.h"
#include <stdexcept>

void require(bool value, const char* message)
{
    if (!value)
        throw std::runtime_error(message);
}

int main()
{
    using namespace intragranular_nucleation;
    require(mode() == "vesh_matrix_network", "Selected analytic model must be active");
    require(dislocation_density::correlation(0, 1100) == 0, "No density offset at zero burnup");
    require(dislocation_density::correlation(80, 900) > dislocation_density::correlation(80, 1400), "Thermal dependence of fitted density");
    require(densityIncrement(10, 10, 900, 1e19, 10) == 0, "No births from cooling alone");
    require(densityIncrement(10, 11, 1100, 0, 10) == 0, "No births during shutdown");
    require(densityIncrement(10, 11, 1100, 1e19, 0) == 0, "No births at zero duration");
    require(densityIncrement(11, 10, 1100, 1e19, 10) == 0, "No births for decreasing dose");
    require(densityIncrement(0, 1, 2000, 1e19, 10) > 0, "Selected analytic extrapolation is active");
    const double nv = mixedVacancies(4e19, 1e6, 2e19);
    require(std::abs((6e19 * nv) / (4e19 * 1e6) - 1) < 1e-14,
            "Birth mixing conserves vacancy inventory");
    state().rho_sites = 0;
    require(intragranular_coarsening::lineTrapping(1100, 1e-20) == 0, "No line capture at zero density");
    require(intragranular_coarsening::meanGasDiffusivity(1100, 1e-20) == 1e-20,
            "Hart mean reduces to bulk at zero density");
    const auto seed = intragranular_coarsening::initializePopulation(0, false, 1e19);
    require(seed.number == K * initial_density, "Independent initial bubble seed survives rho=0");
}
