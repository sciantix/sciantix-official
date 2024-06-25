#include "functionbind.h"

// --- all the clears ---//
void clearHistoryVariable(){
    history_variable.clear();
};
void clearSciantixVariable(){
    sciantix_variable.clear();
};
void clearSystem(){
    sciantix_system.clear();
};
void clearPhysicsVariable(){
    physics_variable.clear();
};
void clearModel(){
    model.clear();
};
void clearMaterial(){
    material.clear();
};
void clearGas(){
    gas.clear();
};
void clearMatrix(){
    matrix.clear();
};

// --- getters --- //
py::array_t<double> getVariablesInArray_double()
{
    double *Sciantix_variables = getSciantixVariables();
    return py::array_t<double>({300}, Sciantix_variables);
}
py::array_t<double> getHistoryInArray_double()
{
    double *Sciantix_history = getSciantixHistory();
    return py::array_t<double>({20}, Sciantix_history);
}
py::array_t<double> getDiffusionModesInArray_double()
{
    double *Sciantix_diffusion_modes = getSciantixDiffusionModes();
    return py::array_t<double>({1000}, Sciantix_diffusion_modes);
}
py::array_t<double> getScalingFactorsInArray_double()
{
    double *Sciantix_scaling_factors = getSciantixScalingFactors();
    return py::array_t<double>({10}, Sciantix_scaling_factors);
}
py::array_t<int> getOptionsInArray_int()
{
    int *Sciantix_options = getSciantixOptions();
    return py::array_t<int>({40}, Sciantix_options);
}

int getSciantixInputHistoryPointsConversion()
{
    return getSciantixInputHistoryPoints();
}

long long int getSciantixTimeStepNumberConversion()
{
    return getSciantixTimeStepNumber();
}

double getSciantixTimeHConversion()
{
    return getSciantixTimeH();
}
double getSciantixDTimeHConversion()
{
    return getSciantixDTimeH();
}
double getSciantixTimeEndHConversion()
{
    return getSciantixTimeEndH();
}
double getSciantixTimeSConversion()
{
    return getSciantixTimeS();
}

std::vector<double> &getSciantixTimeInputConversion()
{
    return getSciantixTimeInput();
}
std::vector<double> &getSciantixTemperatureInputConversion()
{
    return getSciantixTemperatureInput();
}
std::vector<double> &getSciantixFissionrateInputConversion()
{
    return getSciantixFissionrateInput();
}
std::vector<double> &getSciantixHydrostaticstressInputConversion()
{
    return getSciantixHydrostaticstressInput();
}
std::vector<double> &getSciantixSteampressureInputConversion()
{
    return getSciantixSteampressureInput();
}

std::vector<HistoryVariable> &getHistoryVariableConv()
{
    return getHistoryVariable();
}

std::vector<SciantixVariable> &getSciantixVariableConv()
{
    return getSciantixVariable();
}

std::vector<System> &getSystemConv()
{
    return getSystem();
}

std::vector<PhysicsVariable> &getPhysicsVariableConv()
{
    return getPhysicsVariable();
}

std::vector<Model> &getModelConv()
{
    return getModel();
}

std::vector<Material> &getMaterialConv()
{
    return getMaterial();
}

std::vector<Gas> &getGasConv()
{
    return getGas();
}

std::vector<Matrix> &getMatrixConv()
{
    return getMatrix();
}


void SetVariablesConversion(py::array_t<int> Sciantix_options, 
                            py::array_t<double> Sciantix_history, 
                            py::array_t<double> Sciantix_variables, 
                            py::array_t<double> Sciantix_scaling_factors, 
                            py::array_t<double> Sciantix_diffusion_modes)
{
    // Ensure the input arrays are contiguous and have the correct data type
    auto options_buf = Sciantix_options.request();
    auto history_buf = Sciantix_history.request();
    auto variables_buf = Sciantix_variables.request();
    auto scaling_factors_buf = Sciantix_scaling_factors.request();
    auto diffusion_modes_buf = Sciantix_diffusion_modes.request();

    if (options_buf.ndim != 1 || history_buf.ndim != 1 || variables_buf.ndim != 1 ||
        scaling_factors_buf.ndim != 1 || diffusion_modes_buf.ndim != 1) {
        throw std::runtime_error("Number of dimensions must be one");
    }

    int *options_ptr = static_cast<int *>(options_buf.ptr);
    double *history_ptr = static_cast<double *>(history_buf.ptr);
    double *variables_ptr = static_cast<double *>(variables_buf.ptr);
    double *scaling_factors_ptr = static_cast<double *>(scaling_factors_buf.ptr);
    double *diffusion_modes_ptr = static_cast<double *>(diffusion_modes_buf.ptr);

    // Call the C++ function with the converted pointers
    SetVariables(options_ptr, history_ptr, variables_ptr, scaling_factors_ptr, diffusion_modes_ptr);
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


void init_functions(py::module_ &m) {

    m.def("SetVariables", &SetVariablesConversion,
            py::arg("Sciantix_options"),
            py::arg("Sciantix_history"),
            py::arg("Sciantix_variables"),
            py::arg("Sciantix_scaling_factors"),
            py::arg("Sciantix_diffusion_modes"));
    
    m.def("getVariablesInArray_double", &getVariablesInArray_double);
    m.def("getHistoryInArray_double", &getHistoryInArray_double);
    m.def("getDiffusionModesInArray_double", &getDiffusionModesInArray_double);
    m.def("getScalingFactorsInArray_double", &getScalingFactorsInArray_double);
    m.def("getOptionsInArray_int", &getOptionsInArray_int);
    m.def("getSciantixInputHistoryPointsConversion", &getSciantixInputHistoryPointsConversion);
    m.def("getSciantixTimeStepNumberConversion", &getSciantixTimeStepNumberConversion);
    m.def("getSciantixTimeHConversion", &getSciantixTimeHConversion);
    m.def("getSciantixDTimeHConversion", &getSciantixDTimeHConversion);
    m.def("getSciantixTimeEndHConversion", &getSciantixTimeEndHConversion);
    m.def("getSciantixTimeSConversion", &getSciantixTimeSConversion);
    m.def("getSciantixTimeInputConversion", &getSciantixTimeInputConversion, py::return_value_policy::reference_internal);
    m.def("getSciantixTemperatureInputConversion", &getSciantixTemperatureInputConversion, py::return_value_policy::reference_internal);
    m.def("getSciantixFissionrateInputConversion", &getSciantixFissionrateInputConversion, py::return_value_policy::reference_internal);
    m.def("getSciantixHydrostaticstressInputConversion", &getSciantixHydrostaticstressInputConversion, py::return_value_policy::reference_internal);
    m.def("getSciantixSteampressureInputConversion", &getSciantixSteampressureInputConversion, py::return_value_policy::reference_internal);
    m.def("setHistory", &setSciantixHistory, py::arg("index"), py::arg("value"));
    m.def("setHistoryFull", &setSciantixHist, py::arg("sciantix"));
    m.def("setSciantixDTimeH", &setSciantixDTimeH, py::arg("value"));
    m.def("setSciantixTimeStepNumber", &setSciantixTimeStepNumber, py::arg("value"));
    m.def("setSciantixTimeH", &setSciantixTimeH, py::arg("value"));
    m.def("setSciantixTimeS", &setSciantixTimeS, py::arg("value"));

    m.def("clearHistoryVariable", &clearHistoryVariable );
    m.def("clearSciantixVariable", &clearSciantixVariable );
    m.def("clearSystem", &clearSystem );
    m.def("clearPhysicsVariable", &clearPhysicsVariable );
    m.def("clearModel", &clearModel );
    m.def("clearMaterial", &clearMaterial );
    m.def("clearGas", &clearGas );
    m.def("clearMatrix", &clearMatrix );


    m.def("SetGas", &SetGas);
    m.def("SetMatrix", &SetMatrix);
    m.def("SetSystem", &SetSystem);
    m.def("Burnup", &Burnup);
    m.def("EffectiveBurnup", &EffectiveBurnup);
    m.def("EnvironmentComposition", &EnvironmentComposition);
    m.def("UO2Thermochemistry", &UO2Thermochemistry);
    m.def("StoichiometryDeviation", &StoichiometryDeviation);
    m.def("HighBurnupStructureFormation", &HighBurnupStructureFormation);
    m.def("HighBurnupStructurePorosity", &HighBurnupStructurePorosity);
    m.def("GrainGrowth", &GrainGrowth);
    m.def("GrainBoundarySweeping", &GrainBoundarySweeping);
    m.def("GasProduction", &GasProduction);
    m.def("IntraGranularBubbleEvolution", &IntraGranularBubbleEvolution);
    m.def("GasDiffusion", &GasDiffusion);
    m.def("GrainBoundaryMicroCracking", &GrainBoundaryMicroCracking);
    m.def("GrainBoundaryVenting", &GrainBoundaryVenting);
    m.def("InterGranularBubbleEvolution", &InterGranularBubbleEvolution);
    m.def("FiguresOfMerit", &FiguresOfMerit);
    m.def("UpdateVariables", &UpdateVariables);
        // py::arg("Sciantix_variables"),
        // py::arg("Sciantix_diffusion_modes"));
    m.def("Output", &Output);
    m.def("MapModel", &MapModel);
    m.def("InputReading", &InputReading);
    m.def("Initialization", &Initialization);
    m.def("InputInterpolation", &InputInterpolation, 
        py::arg("x"),
        py::arg("xx"),
        py::arg("yy"),
        py::arg("n"));

    m.def("TimeStepCalculation", &TimeStepCalculation);
    m.def("getSciantixOption", &getSciantixOptions);
    m.def("getSciantixHistory", &getSciantixHistory);
    m.def("getSciantixVariables", &getSciantixVariables);
    m.def("getSciantixScalingFactors", &getSciantixScalingFactors);
    m.def("getSciantixDiffusionModes", &getSciantixDiffusionModes);

    m.def("getSciantixTimeStepNumber", &getSciantixTimeStepNumber);
    m.def("getSciantixTimeH", &getSciantixTimeH);
    m.def("getSciantixDTimeH", &getSciantixDTimeH);
    m.def("getSciantixTimeEndH", &getSciantixTimeEndH);
    m.def("getSciantixTimeS", &getSciantixTimeS);
    m.def("getSciantixInputHistoryPoints", &getSciantixInputHistoryPoints);
    m.def("getSciantixTimeInput", &getSciantixTimeInput);
    m.def("getSciantixTemperatureInput", &getSciantixTemperatureInput);
    m.def("getSciantixFissionrateInput", &getSciantixFissionrateInput);
    m.def("getSciantixHydrostaticstressInput", &getSciantixHydrostaticstressInput);
    m.def("getSciantixSteampressureInput", &getSciantixSteampressureInput);

    m.def("getHistoryVariable", &getHistoryVariableConv);
    m.def("getSciantixVariable", &getSciantixVariableConv);
    m.def("getSystem", &getSystemConv);
    m.def("getPhysicsVariable", &getPhysicsVariableConv);
    m.def("getModel", &getModelConv);
    m.def("getMaterial", &getMaterialConv);
    m.def("getGas", &getGasConv);
    m.def("getMatrix", &getMatrixConv);    
}