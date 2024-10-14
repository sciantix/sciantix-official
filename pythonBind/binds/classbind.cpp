#include "classbind.h"

void init_classes(py::module_ &m)
{
    // --- all the classes : --- //
    py::class_<SciantixVariable>(m, "SciantixVariable")
        .def(py::init<std::string, std::string, double, double, bool>(), 
             "Create a SciantixVariable",
             py::arg("name"), py::arg("uom"), py::arg("initial_value"), 
             py::arg("final_value"), py::arg("output"))
        .def("rescaleInitialValue", &SciantixVariable::rescaleInitialValue)
        .def("rescaleFinalValue", &SciantixVariable::rescaleFinalValue)
        .def("addValue", &SciantixVariable::addValue)
        .def("setUOM", &SciantixVariable::setUOM)
        .def("getUOM", &SciantixVariable::getUOM)
        .def("setConstant", &SciantixVariable::setConstant)
        .def("resetValue", &SciantixVariable::resetValue)
        .def("setFinalValue", &SciantixVariable::setFinalValue)
        .def("setInitialValue", &SciantixVariable::setInitialValue)
        .def("getFinalValue", &SciantixVariable::getFinalValue)
        .def("getInitialValue", &SciantixVariable::getInitialValue)
        .def("getIncrement", &SciantixVariable::getIncrement)
        .def("setOutput", &SciantixVariable::setOutput)
        .def("getOutput", &SciantixVariable::getOutput);

    // Bind SciantixArray
    py::class_<SciantixArray<SciantixVariable>>(m, "SciantixArray")
        .def(py::init<>())
        .def("push", &SciantixArray<SciantixVariable>::push)
        .def("clear", &SciantixArray<SciantixVariable>::clear)
        .def("empty", &SciantixArray<SciantixVariable>::empty)
        .def("__getitem__", [](SciantixArray<SciantixVariable>& array, const std::string& name) {
            return array[name]; // Use the name to get the variable
        })
        .def("__getitem__", [](SciantixArray<SciantixVariable>& array, int index) {
            return array[index]; // Use the index to get the variable
        })
        .def("isElementPresent", &SciantixArray<SciantixVariable>::isElementPresent)
        .def("begin", [](SciantixArray<SciantixVariable>& array) {
            return array.begin(); // Adjust if the return type is a complex iterator
        });

    py::class_<Material>(m, "Material")
        .def(py::init<>());
    
    py::class_<Matrix, Material>(m, "Matrix")
        .def("setTheoreticalDensity", &Matrix::setTheoreticalDensity)
        .def("getTheoreticalDensity", &Matrix::getTheoreticalDensity)
        .def("setLatticeParameter", &Matrix::setLatticeParameter)
        .def("getLatticeParameter", &Matrix::getLatticeParameter)
        .def("setSurfaceTension", &Matrix::setSurfaceTension)
        .def("getSurfaceTension", &Matrix::getSurfaceTension)
        .def("setSchottkyVolume", &Matrix::setSchottkyVolume)
        .def("getSchottkyVolume", &Matrix::getSchottkyVolume)
        .def("setOctahedralInterstitialSite", &Matrix::setOctahedralInterstitialSite)
        .def("getOctahedralInterstitialSite", &Matrix::getOctahedralInterstitialSite)
        .def("setGrainBoundaryMobility", &Matrix::setGrainBoundaryMobility)
        .def("getGrainBoundaryMobility", &Matrix::getGrainBoundaryMobility)
        .def("setFissionFragmentRange", &Matrix::setFissionFragmentRange)
        .def("getFissionFragmentRange", &Matrix::getFissionFragmentRange)
        .def("setFissionFragmentInfluenceRadius", &Matrix::setFissionFragmentInfluenceRadius)
        .def("getFissionFragmentInfluenceRadius", &Matrix::getFissionFragmentInfluenceRadius)
        .def("setSemidihedralAngle", &Matrix::setSemidihedralAngle)
        .def("getSemidihedralAngle", &Matrix::getSemidihedralAngle)
        .def("setGrainBoundaryThickness", &Matrix::setGrainBoundaryThickness)
        .def("getGrainBoundaryThickness", &Matrix::getGrainBoundaryThickness)
        .def("setGrainBoundaryVacancyDiffusivity", &Matrix::setGrainBoundaryVacancyDiffusivity)
        .def("getGrainBoundaryVacancyDiffusivity", &Matrix::getGrainBoundaryVacancyDiffusivity)
        .def("setLenticularShapeFactor", &Matrix::setLenticularShapeFactor)
        .def("getLenticularShapeFactor", &Matrix::getLenticularShapeFactor)
        .def("setNucleationRate", &Matrix::setNucleationRate)
        .def("getNucleationRate", &Matrix::getNucleationRate)
        .def("setPoreNucleationRate", &Matrix::setPoreNucleationRate)
        .def("getPoreNucleationRate", &Matrix::getPoreNucleationRate)
        .def("setPoreResolutionRate", &Matrix::setPoreResolutionRate)
        .def("getPoreResolutionRate", &Matrix::getPoreResolutionRate)
        .def("setPoreTrappingRate", &Matrix::setPoreTrappingRate)
        .def("getPoreTrappingRate", &Matrix::getPoreTrappingRate)
        .def("setGrainRadius", &Matrix::setGrainRadius)
        .def("getGrainRadius", &Matrix::getGrainRadius)
        .def("setHealingTemperatureThreshold", &Matrix::setHealingTemperatureThreshold)
        .def("getHealingTemperatureThreshold", &Matrix::getHealingTemperatureThreshold);

    py::class_<Gas>(m, "Gas")
        .def(py::init<>())
        .def("getAtomicNumber", &Gas::getAtomicNumber)
        .def("setAtomicNumber", &Gas::setAtomicNumber, py::arg("y"))
        .def("getMassNumber", &Gas::getMassNumber)
        .def("setMassNumber", &Gas::setMassNumber, py::arg("y"))
        .def("getVanDerWaalsVolume", &Gas::getVanDerWaalsVolume)
        .def("setVanDerWaalsVolume", &Gas::setVanDerWaalsVolume, py::arg("y"))
        .def("getDecayRate", &Gas::getDecayRate)
        .def("setDecayRate", &Gas::setDecayRate, py::arg("l"))
        .def("getPrecursorFactor", &Gas::getPrecursorFactor)
        .def("setPrecursorFactor", &Gas::setPrecursorFactor, py::arg("h"));

    py::class_<System, Material>(m, "System")
        .def(py::init<>())
        .def("setRestructuredMatrix", &System::setRestructuredMatrix)
        .def("getRestructuredMatrix", &System::getRestructuredMatrix)
        .def("setYield", &System::setYield)
        .def("getYield", &System::getYield)
        .def("setRadiusInLattice", &System::setRadiusInLattice)
        .def("getRadiusInLattice", &System::getRadiusInLattice)
        .def("getGas", &System::getGas)
        .def("getGasName", &System::getGasName)
        .def("setMatrix", &System::setMatrix)
        .def("getMatrix", &System::getMatrix)
        .def("getMatrixName", &System::getMatrixName)
        .def("getVolumeInLattice", &System::getVolumeInLattice)
        .def("setVolumeInLattice", &System::setVolumeInLattice)
        .def("setBubbleDiffusivity", &System::setBubbleDiffusivity)
        .def("getBubbleDiffusivity", &System::getBubbleDiffusivity)
        .def("setHeliumDiffusivity", &System::setHeliumDiffusivity)
        .def("getHeliumDiffusivity", &System::getHeliumDiffusivity)
        .def("setFissionGasDiffusivity", &System::setFissionGasDiffusivity)
        .def("getFissionGasDiffusivity", &System::getFissionGasDiffusivity)
        .def("setHenryConstant", &System::setHenryConstant)
        .def("getHenryConstant", &System::getHenryConstant)
        .def("setResolutionRate", &System::setResolutionRate)
        .def("getResolutionRate", &System::getResolutionRate)
        .def("setTrappingRate", &System::setTrappingRate)
        .def("getTrappingRate", &System::getTrappingRate)
        .def("setNucleationRate", &System::setNucleationRate)
        .def("getNucleationRate", &System::getNucleationRate)
        .def("setPoreNucleationRate", &System::setPoreNucleationRate)
        .def("getPoreNucleationRate", &System::getPoreNucleationRate)
        .def("setProductionRate", &System::setProductionRate)
        .def("getProductionRate", &System::getProductionRate);

    py::class_<Variable>(m, "Variable")
        .def(py::init<>())
        .def("setName", &Variable::setName)
        .def("getName", &Variable::getName);

    py::class_<InputVariable, Variable>(m, "InputVariable")
        .def(py::init<>())
        .def("setValue", &InputVariable::setValue, py::arg("v"))
        .def("getValue", &InputVariable::getValue);

    py::class_<Solver, InputVariable>(m, "Solver")
        .def(py::init<>())
        .def("Integrator", &Solver::Integrator, py::arg("initial_value"), py::arg("parameter"), py::arg("increment"))
        .def("LimitedGrowth", &Solver::LimitedGrowth, py::arg("initial_value"), py::arg("parameter"), py::arg("increment"))
        .def("Decay", &Solver::Decay, py::arg("initial_condition"), py::arg("decay_rate"), py::arg("source_term"), py::arg("increment"))
        .def("BinaryInteraction", &Solver::BinaryInteraction, py::arg("initial_condition"), py::arg("interaction_coefficient"), py::arg("increment"))
        // .def("BinaryInteractionVerification", &Solver::BinaryInteractionVerification, py::arg("initial_condition"), py::arg("interaction_coefficient"), py::arg("increment"), py::arg("mode"))
        .def("SpectralDiffusion", &Solver::SpectralDiffusion, py::arg("initial_condition"), py::arg("parameter"), py::arg("increment"))
        .def("dotProduct1D", &Solver::dotProduct1D, py::arg("u"), py::arg("v"), py::arg("n"))
        .def("dotProduct2D", &Solver::dotProduct2D, py::arg("A"), py::arg("v"), py::arg("n_rows"), py::arg("n_col"), py::arg("result"))
        .def("SpectralDiffusion2equations", &Solver::SpectralDiffusion2equations, py::arg("gas_1"), py::arg("gas_2"), py::arg("initial_condition_gas_1"), py::arg("initial_condition_gas_2"), py::arg("parameter"), py::arg("increment"))
        .def("SpectralDiffusion3equations", &Solver::SpectralDiffusion3equations, py::arg("gas_1"), py::arg("gas_2"), py::arg("gas_3"), py::arg("initial_condition_gas_1"), py::arg("initial_condition_gas_2"), py::arg("initial_condition_gas_3"), py::arg("parameter"), py::arg("increment"))
        .def("Laplace2x2", &Solver::Laplace2x2, py::arg("A"), py::arg("b"))
        .def("Laplace3x3", &Solver::Laplace3x3, py::arg("A"), py::arg("b"))
        .def("det", &Solver::det, py::arg("N"), py::arg("A"))
        .def("Laplace", &Solver::Laplace, py::arg("N"), py::arg("A"), py::arg("b"))
        .def("QuarticEquation", &Solver::QuarticEquation, py::arg("parameter"))
        .def("modeInitialization", &Solver::modeInitialization, py::arg("n_modes"), py::arg("mode_initial_condition"), py::arg("diffusion_modes"))
        .def("NewtonBlackburn", &Solver::NewtonBlackburn, py::arg("parameter"))
        .def("NewtonLangmuirBasedModel", &Solver::NewtonLangmuirBasedModel, py::arg("initial_value"), py::arg("parameter"), py::arg("increment"));

    py::class_<Simulation>(m, "Simulation")
        .def_static("getInstance", &Simulation::getInstance, "Gets the singleton instance of Simulation")
        
        // Existing bindings
        .def("Burnup", &Simulation::Burnup, "Computes the fuel burnup from the local power density.")
        .def("EffectiveBurnup", &Simulation::EffectiveBurnup, "Computes the effective burnup of the fuel.")
        .def("GasProduction", &Simulation::GasProduction, "Computes the gas produced from the production rate.")
        .def("GasDecay", &Simulation::GasDecay, "Computes the decay of gases.")
        .def("GasDiffusion", &Simulation::GasDiffusion, "Computes gas diffusion based on selected models and conditions.")
        .def("GrainGrowth", &Simulation::GrainGrowth, "Calculates grain growth in materials.")
        .def("IntraGranularBubbleBehavior", &Simulation::IntraGranularBubbleBehavior, "Simulates the behavior of intragranular bubbles.")
        .def("InterGranularBubbleBehavior", &Simulation::InterGranularBubbleBehavior, "Simulates the behavior of intergranular bubbles.")
        .def("GrainBoundarySweeping", &Simulation::GrainBoundarySweeping, "Processes grain boundary sweeping.")
        .def("GrainBoundaryMicroCracking", &Simulation::GrainBoundaryMicroCracking, "Handles grain boundary microcracking induced by temperature changes.")
        .def("GrainBoundaryVenting", &Simulation::GrainBoundaryVenting, "Processes grain boundary venting.")
        .def("HighBurnupStructureFormation", &Simulation::HighBurnupStructureFormation, "Simulates the formation of high burnup structures.")
        .def("HighBurnupStructurePorosity", &Simulation::HighBurnupStructurePorosity, "Calculates the porosity of high burnup structures.")
        .def("StoichiometryDeviation", &Simulation::StoichiometryDeviation, "Manages stoichiometry deviations in materials.")
        .def("UO2Thermochemistry", &Simulation::UO2Thermochemistry, "Manages UO2 thermochemical processes.")
        
        // Methods related to diffusion modes
        .def("getDiffusionModes", &Simulation::getDiffusionModes, py::arg("gas_name"), "Returns a pointer to the array of diffusion modes for a specified gas.")
        .def("getDiffusionModesSolution", &Simulation::getDiffusionModesSolution, py::arg("gas_name"), "Returns a pointer to the array of solution diffusion modes for a specified gas.")
        .def("getDiffusionModesBubbles", &Simulation::getDiffusionModesBubbles, py::arg("gas_name"), "Returns a pointer to the array of bubble diffusion modes for a specified gas.")

        // Methods for getting various variables
        .def("getHistoryVariable", &Simulation::getHistoryVariable, py::return_value_policy::reference)
        .def("getSciantixVariable", &Simulation::getSciantixVariable, py::return_value_policy::reference)
        .def("getPhysicsVariable", &Simulation::getPhysicsVariable, py::return_value_policy::reference)
        .def("getModel", &Simulation::getModel, py::return_value_policy::reference)
        .def("getSciantixSystem", &Simulation::getSciantixSystem, py::return_value_policy::reference)
        .def("getMatrices", &Simulation::getMatrices, py::return_value_policy::reference)
        .def("getGas", &Simulation::getGas, py::return_value_policy::reference)

        // Additional methods to bind
        .def("setVariables", &Simulation::setVariables, "Sets the variables for the simulation.")
        .def("setGas", &Simulation::setGas, "Sets the gas properties for the simulation.")
        .def("setMatrix", &Simulation::setMatrix, "Sets the matrix for the simulation.")
        .def("setSystem", &Simulation::setSystem, "Sets the system for the simulation.")
        .def("update", &Simulation::update, "Updates the simulation with the latest variables.")
        .def("output", &Simulation::output, "Print the simulation results.");
}

