#include "attributesbind.h"
#include "MainVariables.h"
#include "Simulation.h"

void init_attributes(py::module_ &m) {
    // Main variables
    m.attr("Time_step_number") = &Time_step_number;
    m.attr("Time_h") = &Time_h;
    m.attr("dTime_h") = &dTime_h;
    m.attr("Time_end_h") = &Time_end_h;
    // m.attr("Time_s") = &Time_s;
    // m.attr("Time_end_s") = &Time_end_s;
    m.attr("Number_of_time_steps_per_interval") = &Number_of_time_steps_per_interval;

    m.attr("Sciantix_options") = py::array_t<int>({40}, Sciantix_options);
    m.attr("Sciantix_history") = py::array_t<double>({20}, Sciantix_history);
    m.attr("Sciantix_variables") = py::array_t<double>({300}, Sciantix_variables);
    m.attr("Sciantix_scaling_factors") = py::array_t<double>({10}, Sciantix_scaling_factors);
    m.attr("Sciantix_diffusion_modes") = py::array_t<double>({1000}, Sciantix_diffusion_modes);

    m.attr("Input_history_points") = &Input_history_points;

    // Time input variables
    m.attr("Time_input") = py::cast(Time_input);
    m.attr("Temperature_input") = py::cast(Temperature_input);
    m.attr("Fissionrate_input") = py::cast(Fissionrate_input);
    m.attr("Hydrostaticstress_input") = py::cast(Hydrostaticstress_input);
    m.attr("Steampressure_input") = py::cast(Steampressure_input);

    // Access Simulation instance to get variable references
    Simulation* simInstance = Simulation::getInstance();
}
