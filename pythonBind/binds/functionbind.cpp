#include "functionbind.h"

// --- getters --- //
void bind_get_history_variable(py::module_ &m)
{
    m.def("getHistoryInArray_double", []() -> py::array_t<double> {
        // Assuming you have a method or a variable in the C++ code that returns the history array
        double* history_data = getSciantixHistoryArray();  // Function to get the history array pointer
        return py::array_t<double>({20}, history_data);  // Adjust size (20) based on the actual size of the array
    });
}

void bind_get_variables(py::module_ &m)
{
    m.def("getVariablesInArray_double", []() -> py::array_t<double> {
        double *Sciantix_variables = getSciantixVariablesArray();  // Fetch the pointer
        return py::array_t<double>({300}, Sciantix_variables);  // Adjust size (300)
    });
}

void bind_get_diffusion_modes(py::module_ &m)
{
    m.def("getDiffusionModesInArray_double", []() -> py::array_t<double> {
        double *Sciantix_diffusion_modes = getSciantixDiffusionModesArray();  // Fetch the pointer
        return py::array_t<double>({720}, Sciantix_diffusion_modes);  // Adjust size (720)
    });
}

void bind_get_scaling_factors(py::module_ &m)
{
    m.def("getScalingFactorsInArray_double", []() -> py::array_t<double> {
        double *Sciantix_scaling_factors = getSciantixScalingFactorsArray();  // Fetch the pointer
        return py::array_t<double>({10}, Sciantix_scaling_factors);  // Adjust size (10)
    });
}

void bind_get_options(py::module_ &m)
{
    m.def("getOptionsInArray_int", []() -> py::array_t<int> {
        int *Sciantix_options = getSciantixOptionsArray();  // Fetch the pointer
        return py::array_t<int>({40}, Sciantix_options);  // Adjust size (40)
    });
}

void UpdateVariablesConversion(py::array_t<double> Sciantix_variables, py::array_t<double> Sciantix_diffusion_modes)
{
    // Request buffer information for the numpy arrays
    auto variables_buf = Sciantix_variables.request();
    auto diffusion_modes_buf = Sciantix_diffusion_modes.request();

    // Ensure the input arrays are contiguous and have the correct data type
    if (variables_buf.ndim != 1 || diffusion_modes_buf.ndim != 1) {
        throw std::runtime_error("Number of dimensions must be one");
    }

    // Convert to raw pointers
    double *variables_ptr = static_cast<double *>(variables_buf.ptr);
    double *diffusion_modes_ptr = static_cast<double *>(diffusion_modes_buf.ptr);

    // Call the C++ function with the converted pointers
    //UpdateVariables(variables_ptr, diffusion_modes_ptr);
}


Matrix convertToMatrix(const pybind11::array_t<double>& array) {
    auto buf = array.unchecked<2>();
    size_t rows = buf.shape(0);
    size_t cols = buf.shape(1);
    Matrix mat;
    return mat;
}

void init_functions(py::module_ &m) {

    // Define the setTestPath function
    m.def("setTestPath", [](const std::string& path) {
        TestPath = path; // Set the global TestPath variable
    }, "Set the TestPath variable.");

    // Define the getTestPath function
    m.def("getTestPath", []() {
        return TestPath; // Return the current TestPath variable
    }, "Get the TestPath variable.");

    Simulation* sim_instance = Simulation::getInstance();

    m.def("setSystem", [sim_instance]() {  // Capture sim_instance by value
        // Access the singleton instance of Simulation
        if (sim_instance) {
            sim_instance->setSystem();  // Call the setSystem method on the singleton instance
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("setMatrix", [sim_instance]() {
        if (sim_instance) {
            sim_instance->setMatrix();
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("Burnup", [&sim_instance]() {sim_instance->Burnup();}, "Calculates the burnup.");


    m.def("EffectiveBurnup", [sim_instance]() {
        if (sim_instance) {
            sim_instance->EffectiveBurnup();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("GapPartialPressure", [sim_instance]() {
        if (sim_instance) {
            sim_instance->GapPartialPressure();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("UO2Thermochemistry", [sim_instance]() {
        if (sim_instance) {
            sim_instance->UO2Thermochemistry();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("StoichiometryDeviation", [sim_instance]() {
        if (sim_instance) {
            sim_instance->StoichiometryDeviation();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("HighBurnupStructureFormation", [sim_instance]() {
        if (sim_instance) {
            sim_instance->HighBurnupStructureFormation();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("HighBurnupStructurePorosity", [sim_instance]() {
        if (sim_instance) {
            sim_instance->HighBurnupStructurePorosity();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("GrainGrowth", [sim_instance]() {
        if (sim_instance) {
            sim_instance->GrainGrowth();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("GrainBoundarySweeping", [sim_instance]() {
        if (sim_instance) {
            sim_instance->GrainBoundarySweeping();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("GasProduction", [sim_instance]() {
        if (sim_instance) {
            sim_instance->GasProduction();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("IntraGranularBubbleBehavior", [sim_instance]() {
        if (sim_instance) {
            sim_instance->IntraGranularBubbleBehavior();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("GasDiffusion", [sim_instance]() {
        if (sim_instance) {
            sim_instance->GasDiffusion();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("GrainBoundaryMicroCracking", [sim_instance]() {
        if (sim_instance) {
            sim_instance->GrainBoundaryMicroCracking();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("GrainBoundaryVenting", [sim_instance]() {
        if (sim_instance) {
            sim_instance->GrainBoundaryVenting();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("InterGranularBubbleBehavior", [sim_instance]() {
        if (sim_instance) {
            sim_instance->InterGranularBubbleBehavior();  // Call the member function
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("update", [sim_instance](double* Sciantix_variables, double* Sciantix_diffusion_modes) {
        if (sim_instance) {
            sim_instance->update(Sciantix_variables, Sciantix_diffusion_modes);  // Call the member function with arguments
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("output", [sim_instance]() {
        if (sim_instance) {
            sim_instance->output();  // Call the output method on the singleton instance
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("Initialization", &Initialization);
    m.def("InputInterpolation", &InputInterpolation, 
        py::arg("x"),
        py::arg("xx"),
        py::arg("yy"),
        py::arg("n"));

    m.def("TimeStepCalculation", &TimeStepCalculation,
          py::arg("Input_history_points"),
          py::arg("Time_h"),
          py::arg("Time_input"),
          py::arg("Number_of_time_steps_per_interval"),
          "Calculates the time step in hours.");

    m.def("getSciantixOptionArray", &getSciantixOptionsArray);
    m.def("getSciantixHistoryArray", &getSciantixHistoryArray);
    m.def("getSciantixVariablesArray", &getSciantixVariablesArray);
    m.def("getSciantixScalingFactorsArray", &getSciantixScalingFactorsArray);
    m.def("getSciantixDiffusionModesArray", &getSciantixDiffusionModesArray);

}