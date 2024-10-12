#include "functionbind.h"

// --- getters --- //
py::array_t<double> getVariablesInArray_double() {
    double *Sciantix_variables = getSciantixVariablesArray();
    return py::array_t<double>({300}, Sciantix_variables);
}

py::array_t<double> getHistoryInArray_double() {
    double *Sciantix_history = getSciantixHistoryArray();
    return py::array_t<double>({20}, Sciantix_history);
}

py::array_t<double> getDiffusionModesInArray_double() {
    double *Sciantix_diffusion_modes = getSciantixDiffusionModesArray();
    return py::array_t<double>({720}, Sciantix_diffusion_modes);
}

py::array_t<double> getScalingFactorsInArray_double() {
    double *Sciantix_scaling_factors = getSciantixScalingFactorsArray();
    return py::array_t<double>({10}, Sciantix_scaling_factors);
}

py::array_t<int> getOptionsInArray_int() {
    int *Sciantix_options = getSciantixOptionsArray();
    return py::array_t<int>({40}, Sciantix_options);
}

// void SetVariablesConversion(py::array_t<int> Sciantix_options, 
//                             py::array_t<double> Sciantix_history, 
//                             py::array_t<double> Sciantix_variables, 
//                             py::array_t<double> Sciantix_scaling_factors, 
//                             py::array_t<double> Sciantix_diffusion_modes)
// {
//     // Ensure the input arrays are contiguous and have the correct data type
//     auto options_buf = Sciantix_options.request();
//     auto history_buf = Sciantix_history.request();
//     auto variables_buf = Sciantix_variables.request();
//     auto scaling_factors_buf = Sciantix_scaling_factors.request();
//     auto diffusion_modes_buf = Sciantix_diffusion_modes.request();

//     if (options_buf.ndim != 1 || history_buf.ndim != 1 || variables_buf.ndim != 1 ||
//         scaling_factors_buf.ndim != 1 || diffusion_modes_buf.ndim != 1) {
//         throw std::runtime_error("Number of dimensions must be one");
//     }

//     int *options_ptr = static_cast<int *>(options_buf.ptr);
//     double *history_ptr = static_cast<double *>(history_buf.ptr);
//     double *variables_ptr = static_cast<double *>(variables_buf.ptr);
//     double *scaling_factors_ptr = static_cast<double *>(scaling_factors_buf.ptr);
//     double *diffusion_modes_ptr = static_cast<double *>(diffusion_modes_buf.ptr);

//     // Call the C++ function with the converted pointers
//     SetVariables(options_ptr, history_ptr, variables_ptr, scaling_factors_ptr, diffusion_modes_ptr);
// }

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

    // m.def("SetVariables", &SetVariablesConversion,
    //         py::arg("Sciantix_options"),
    //         py::arg("Sciantix_history"),
    //         py::arg("Sciantix_variables"),
    //         py::arg("Sciantix_scaling_factors"),
    //         py::arg("Sciantix_diffusion_modes"));

    m.def("setMatrix", [sim_instance]() {
        if (sim_instance) {
            sim_instance->setMatrix();
        } else {
            throw std::runtime_error("Failed to get Simulation instance.");
        }
    });

    m.def("setGas", [sim_instance]() {
        Simulation* sim_instance = Simulation::getInstance();
        if (sim_instance) {
            sim_instance->setGas();
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

    m.def("TimeStepCalculation", &TimeStepCalculation);
    m.def("getSciantixOptionArray", &getSciantixOptionsArray);
    m.def("getSciantixHistoryArray", &getSciantixHistoryArray);
    m.def("getSciantixVariablesArray", &getSciantixVariablesArray);
    m.def("getSciantixScalingFactorsArray", &getSciantixScalingFactorsArray);
    m.def("getSciantixDiffusionModesArray", &getSciantixDiffusionModesArray);

    // m.def("getSciantixTimeStepNumber", &getSciantixTimeStepNumber);
    // m.def("getSciantixTimeH", &getSciantixTimeH);
    // m.def("getSciantixDTimeH", &getSciantixDTimeH);
    // m.def("getSciantixTimeEndH", &getSciantixTimeEndH);
    // m.def("getSciantixTimeS", &getSciantixTimeS);
    // m.def("getSciantixInputHistoryPoints", &getSciantixInputHistoryPoints);
    // m.def("getSciantixTimeInput", &getSciantixTimeInput);
    // m.def("getSciantixTemperatureInput", &getSciantixTemperatureInput);
    // m.def("getSciantixFissionrateInput", &getSciantixFissionrateInput);
    // m.def("getSciantixHydrostaticstressInput", &getSciantixHydrostaticstressInput);
    // m.def("getSciantixSteampressureInput", &getSciantixSteampressureInput);

    // m.def("getHistoryVariable", [sim_instance]() {
    //     if (sim_instance) {
    //         // Access the history_variable
    //         SciantixArray<SciantixVariable>& history_var = sim_instance->getHistoryVariable();
            
    //         // Create a vector to hold the data
    //         std::vector<double> history_data;

    //         // Assuming SciantixArray has an operator[] to access elements
    //         for (size_t i = 0; i < history_var.size(); ++i) {
    //             // Assuming SciantixVariable has a way to get the double value
    //             history_data.push_back(history_var[i].getValue());
    //         }
            
    //         // Return as a NumPy array
    //         return py::array_t<double>(history_data.size(), history_data.data());
    //     } else {
    //         throw std::runtime_error("Failed to get Simulation instance.");
    //     }
    // }, "Get the history variable.");

}