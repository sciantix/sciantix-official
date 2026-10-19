#ifndef SCIANTIX_UN_L3_STATE_H
#define SCIANTIX_UN_L3_STATE_H

#include <array>
#include <cstddef>
#include <stdexcept>
#include <vector>

namespace un_l3
{
struct BubblePopulationState
{
    double number_density = 0.0;  // bubbles/m^3 fuel
    double radius = 0.0;          // m
    double volume = 0.0;          // m^3/bubble
    double vacancies = 0.0;       // total vacancies/m^3 fuel, not per bubble
};

struct GrainFaceState
{
    double number_areal = 0.0;  // bubbles/m^2 boundary
    double gas = 0.0;           // atoms/m^3 fuel retained on grain faces
    double released = 0.0;      // cumulative atoms/m^3 fuel
    double vacancies = 0.0;     // total vacancies/m^3 fuel
    bool saturation_reached = false;
    double number_volumetric = 0.0, atoms_per_bubble = 0.0, vacancies_per_bubble = 0.0;
    double volume = 0.0, radius = 0.0, area = 0.0, coverage = 0.0;
    double pressure = 0.0, equilibrium_pressure = 0.0, zeta = 0.0;
    double vacancy_rate = 0.0, area_rate = 0.0, number_rate = 0.0;
    double gas_in_step = 0.0, gas_released_step = 0.0;
};

// Explicit owner of UN data across calls. No singleton/global storage, no
// reconstruction of spectral profiles from averages, no restart API yet.
// The complete model owns this state as a member of Simulation's UN L3 path.
class State
{
  public:
    explicit State(std::size_t number_of_modes) : number_of_modes_(number_of_modes)
    {
        // The existing exchange solver indexes modes with unsigned short.
        if (number_of_modes == 0 || number_of_modes > 65535)
            throw std::invalid_argument("UN mode count must be in [1, 65535]");
        for (auto& population : modes)
            population.resize(number_of_modes, 0.0);
    }

    std::size_t numberOfModes() const { return number_of_modes_; }

    std::array<std::vector<double>, 3> modes;  // c, mb, md; signed coefficients
    std::array<double, 3> averages = {{0.0, 0.0, 0.0}};  // atoms/m^3 fuel
    BubblePopulationState bulk;
    BubblePopulationState dislocation;
    GrainFaceState grain_face;
    bool bulk_first_gas_seed_done = false;
    bool dislocation_first_gas_seed_done = false;
    double initial_gas = 0.0;       // atoms/m^3 fuel
    double generated = 0.0;         // cumulative atoms/m^3 fuel
    double gas_to_boundary = 0.0;   // grain-face retained + cumulative released
    double time_s = 0.0;

  private:
    std::size_t number_of_modes_;
};
}  // namespace un_l3

#endif
