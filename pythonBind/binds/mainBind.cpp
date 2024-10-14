#include <pybind11/pybind11.h>
#include <pybind11/numpy.h>
#include "mainBind.h"

namespace py = pybind11;

void initialize_simulation(
    py::array_t<int> Sciantix_options,
    py::array_t<double> Sciantix_history,
    py::array_t<double> Sciantix_variables,
    py::array_t<double> Sciantix_scaling_factors,
    py::array_t<double> Sciantix_diffusion_modes)
{
    // Convert numpy arrays to raw pointers
    auto Sciantix_options_ptr = Sciantix_options.mutable_data();
    auto Sciantix_history_ptr = Sciantix_history.mutable_data();
    auto Sciantix_variables_ptr = Sciantix_variables.mutable_data();
    auto Sciantix_scaling_factors_ptr = Sciantix_scaling_factors.mutable_data();
    auto Sciantix_diffusion_modes_ptr = Sciantix_diffusion_modes.mutable_data();

    // Get instance of Simulation and call setVariables
    Simulation* sim = Simulation::getInstance();
    sim->setVariables(Sciantix_options_ptr, Sciantix_history_ptr, Sciantix_variables_ptr, Sciantix_scaling_factors_ptr, Sciantix_diffusion_modes_ptr);
}

// Wrapper function to update the Simulation
void update_simulation(
    py::array_t<double> Sciantix_variables,
    py::array_t<double> Sciantix_diffusion_modes)
{
    // Convert numpy arrays to raw pointers
    auto Sciantix_variables_ptr = Sciantix_variables.mutable_data();
    auto Sciantix_diffusion_modes_ptr = Sciantix_diffusion_modes.mutable_data();

    // Get instance of Simulation and call update
    Simulation* sim = Simulation::getInstance();
    sim->update(Sciantix_variables_ptr, Sciantix_diffusion_modes_ptr);
}


void bind_get_history_variable(py::module_ &m);
void bind_get_variables(py::module_ &m);
void bind_get_diffusion_modes(py::module_ &m);
void bind_get_scaling_factors(py::module_ &m);
void bind_get_options(py::module_ &m);

// Define the binding for the main variables
void bind_main_variables(py::module_ &m) {
    m.attr("Sciantix_options") = py::cast(Sciantix_options, py::return_value_policy::reference);
    m.attr("Sciantix_variables") = py::cast(Sciantix_variables, py::return_value_policy::reference);
    m.attr("Sciantix_scaling_factors") = py::cast(Sciantix_scaling_factors, py::return_value_policy::reference);
    m.attr("Sciantix_diffusion_modes") = py::cast(Sciantix_diffusion_modes, py::return_value_policy::reference);
    m.attr("Sciantix_history") = py::cast(Sciantix_history, py::return_value_policy::reference);
    m.attr("Input_history_points") = py::cast(&Input_history_points, py::return_value_policy::reference);
    m.attr("Time_input") = py::cast(&Time_input, py::return_value_policy::reference);
    m.attr("Temperature_input") = py::cast(&Temperature_input, py::return_value_policy::reference);
    m.attr("Fissionrate_input") = py::cast(&Fissionrate_input, py::return_value_policy::reference);
    m.attr("Hydrostaticstress_input") = py::cast(&Hydrostaticstress_input, py::return_value_policy::reference);
    m.attr("Steampressure_input") = py::cast(&Steampressure_input, py::return_value_policy::reference);
    m.attr("Time_end_h") = py::cast(&Time_end_h, py::return_value_policy::reference);
    m.attr("Time_end_s") = py::cast(&Time_end_s, py::return_value_policy::reference);
}


void bind_input_reading(py::module_ &m)
{
    m.def(
        "InputReading", 
        [](py::array_t<int> Sciantix_options, 
           py::array_t<double> Sciantix_variables, 
           py::array_t<double> Sciantix_scaling_factors, 
           int Input_history_points, 
           py::array_t<double> Time_input, 
           py::array_t<double> Temperature_input, 
           py::array_t<double> Fissionrate_input, 
           py::array_t<double> Hydrostaticstress_input, 
           py::array_t<double> Steampressure_input, 
           double Time_end_h, 
           double Time_end_s)
        {
            // Get mutable data pointers
            auto Sciantix_options_ptr = Sciantix_options.mutable_data();
            auto Sciantix_variables_ptr = Sciantix_variables.mutable_data();
            auto Sciantix_scaling_factors_ptr = Sciantix_scaling_factors.mutable_data();

            // Convert numpy arrays to std::vector<double>
            std::vector<double> Time_input_vec(Time_input.size());
            std::memcpy(Time_input_vec.data(), Time_input.data(), Time_input.size() * sizeof(double));

            std::vector<double> Temperature_input_vec(Temperature_input.size());
            std::memcpy(Temperature_input_vec.data(), Temperature_input.data(), Temperature_input.size() * sizeof(double));

            std::vector<double> Fissionrate_input_vec(Fissionrate_input.size());
            std::memcpy(Fissionrate_input_vec.data(), Fissionrate_input.data(), Fissionrate_input.size() * sizeof(double));

            std::vector<double> Hydrostaticstress_input_vec(Hydrostaticstress_input.size());
            std::memcpy(Hydrostaticstress_input_vec.data(), Hydrostaticstress_input.data(), Hydrostaticstress_input.size() * sizeof(double));

            std::vector<double> Steampressure_input_vec(Steampressure_input.size());
            std::memcpy(Steampressure_input_vec.data(), Steampressure_input.data(), Steampressure_input.size() * sizeof(double));

            // Call the original C++ InputReading function
            InputReading(
                Sciantix_options_ptr, 
                Sciantix_variables_ptr, 
                Sciantix_scaling_factors_ptr, 
                Input_history_points, 
                Time_input_vec,  // Pass the vector
                Temperature_input_vec, 
                Fissionrate_input_vec, 
                Hydrostaticstress_input_vec, 
                Steampressure_input_vec, 
                Time_end_h, 
                Time_end_s
            );
        },
        py::arg("Sciantix_options").noconvert(), 
        py::arg("Sciantix_variables").noconvert(), 
        py::arg("Sciantix_scaling_factors").noconvert(), 
        py::arg("Input_history_points"), 
        py::arg("Time_input").noconvert(), 
        py::arg("Temperature_input").noconvert(), 
        py::arg("Fissionrate_input").noconvert(), 
        py::arg("Hydrostaticstress_input").noconvert(), 
        py::arg("Steampressure_input").noconvert(), 
        py::arg("Time_end_h"), 
        py::arg("Time_end_s")
    );
}


// Define the binding for the Initialization function
void bind_initialization(py::module_ &m)
{
    m.def(
        "Initialization", 
        [](py::array_t<double> Sciantix_history,
           py::array_t<double> Sciantix_variables, 
           py::array_t<double> Sciantix_diffusion_modes, 
           py::array_t<double> Temperature_input, 
           py::array_t<double> Fissionrate_input, 
           py::array_t<double> Hydrostaticstress_input, 
           py::array_t<double> Steampressure_input)
        {
            // Convert numpy arrays to raw pointers (for mutable arrays)
            auto Sciantix_history_ptr = Sciantix_history.mutable_data();
            auto Sciantix_variables_ptr = Sciantix_variables.mutable_data();
            auto Sciantix_diffusion_modes_ptr = Sciantix_diffusion_modes.mutable_data();

            // Convert numpy arrays to std::vector<double> (for input vectors)
            std::vector<double> Temperature_input_vec(Temperature_input.size());
            std::memcpy(Temperature_input_vec.data(), Temperature_input.mutable_data(), Temperature_input.size() * sizeof(double));

            std::vector<double> Fissionrate_input_vec(Fissionrate_input.size());
            std::memcpy(Fissionrate_input_vec.data(), Fissionrate_input.mutable_data(), Fissionrate_input.size() * sizeof(double));

            std::vector<double> Hydrostaticstress_input_vec(Hydrostaticstress_input.size());
            std::memcpy(Hydrostaticstress_input_vec.data(), Hydrostaticstress_input.mutable_data(), Hydrostaticstress_input.size() * sizeof(double));

            std::vector<double> Steampressure_input_vec(Steampressure_input.size());
            std::memcpy(Steampressure_input_vec.data(), Steampressure_input.mutable_data(), Steampressure_input.size() * sizeof(double));

            // Call the original C++ Initialization function
            Initialization(
                Sciantix_history_ptr, 
                Sciantix_variables_ptr, 
                Sciantix_diffusion_modes_ptr,  // Now passing diffusion_modes correctly
                Temperature_input_vec, 
                Fissionrate_input_vec, 
                Hydrostaticstress_input_vec, 
                Steampressure_input_vec
            );
        },
        py::arg("Sciantix_history"), 
        py::arg("Sciantix_variables"), 
        py::arg("Sciantix_diffusion_modes"),  // Added diffusion_modes correctly
        py::arg("Temperature_input"), 
        py::arg("Fissionrate_input"), 
        py::arg("Hydrostaticstress_input"), 
        py::arg("Steampressure_input")
    );
}

/**
 * This is the PYBIND11_MODULE for exporting C/C++ classes as a Python module.
 * @param sciantixModule The name of the module.
 * @param m The module reference.
 * @return This function returns the Python module.
 */
PYBIND11_MODULE(sciantixModule, m)
{
    bind_get_history_variable(m);
    bind_get_variables(m);
    bind_get_diffusion_modes(m);
    bind_get_scaling_factors(m);
    bind_get_options(m);

    // Bind the main variables
    bind_main_variables(m);

    // Bind the InputReading function
    bind_input_reading(m);

    // Bind the Initialization function
    bind_initialization(m);

    //--- imports all the attributes used --- // 
    init_attributes(m);
    
    //--- imports all the functions used --- //
    init_functions(m);

    //--- imports all the classes used --- //
    init_classes(m);

    m.def("initialize_simulation", &initialize_simulation);
    m.def("update_simulation", &update_simulation);

}
