
#include "classbind.h"

void init_classes(py::module_ &m) {
    
    // --- all the classes : --- //
    py::class_<Entity>(m, "Entity")
        .def(py::init<>())
        .def("getReference", &Entity::getRef)
        .def("getName", &Entity::getName)
        .def("setReference", &Entity::setRef, py::arg("n"))
        .def("setName", &Entity::setName, py::arg("n"));

    py::class_<Material, Entity>(m, "Material")
        .def(py::init<>());

    py::class_<Matrix, Material>(m, "Matrix")
        .def(py::init<>())
        .def("setTheoreticalDensity", &Matrix::setTheoreticalDensity, py::arg("m"))
        .def("getTheoreticalDensity", &Matrix::getTheoreticalDensity)
        .def("setLatticeParameter", &Matrix::setLatticeParameter, py::arg("m"))
        .def("getLatticeParameter", &Matrix::getLatticeParameter)
        .def("setSurfaceTension", &Matrix::setSurfaceTension, py::arg("r"))
        .def("getSurfaceTension", &Matrix::getSurfaceTension)
        .def("setSchottkyVolume", &Matrix::setSchottkyVolume, py::arg("v"))
        .def("getSchottkyVolume", &Matrix::getSchottkyVolume)
        .def("setOIS", &Matrix::setOIS, py::arg("v"))
        .def("getOIS", &Matrix::getOIS)
        .def("setGrainBoundaryMobility", &Matrix::setGrainBoundaryMobility)
        .def("getGrainBoundaryMobility", &Matrix::getGrainBoundaryMobility)
        .def("setFFrange", &Matrix::setFFrange, py::arg("r"))
        .def("getFFrange", &Matrix::getFFrange)
        .def("setFFinfluenceRadius", &Matrix::setFFinfluenceRadius, py::arg("r"))
        .def("getFFinfluenceRadius", &Matrix::getFFinfluenceRadius)
        .def("setSemidihedralAngle", &Matrix::setSemidihedralAngle, py::arg("sda"))
        .def("getSemidihedralAngle", &Matrix::getSemidihedralAngle)
        .def("setGrainBoundaryThickness", &Matrix::setGrainBoundaryThickness, py::arg("gbt"))
        .def("getGrainBoundaryThickness", &Matrix::getGrainBoundaryThickness)
        .def("setGrainBoundaryVacancyDiffusivity", &Matrix::setGrainBoundaryVacancyDiffusivity, py::arg("inpput_value"))
        .def("getGrainBoundaryVacancyDiffusivity", &Matrix::getGrainBoundaryVacancyDiffusivity)
        .def("setLenticularShapeFactor", &Matrix::setLenticularShapeFactor, py::arg("lsf"))
        .def("getLenticularShapeFactor", &Matrix::getLenticularShapeFactor)
        .def("setNucleationRate", &Matrix::setNucleationRate, py::arg("n"))
        .def("getNucleationRate", &Matrix::getNucleationRate)
        .def("setPoreNucleationRate", &Matrix::setPoreNucleationRate)
        .def("getPoreNucleationRate", &Matrix::getPoreNucleationRate)
        .def("setPoreResolutionRate", &Matrix::setPoreResolutionRate)
        .def("getPoreResolutionRate", &Matrix::getPoreResolutionRate)
        .def("setPoreTrappingRate", &Matrix::setPoreTrappingRate)
        .def("getPoreTrappingRate", &Matrix::getPoreTrappingRate)
        .def("setGrainRadius", &Matrix::setGrainRadius, py::arg("gr"))
        .def("getGrainRadius", &Matrix::getGrainRadius)
        .def("setHealingTemperatureThreshold", &Matrix::setHealingTemperatureThreshold, py::arg("t"))
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

    py::class_<System, Matrix, Gas>(m, "System")
        .def(py::init<>())
        .def("setRestructuredMatrix", &System::setRestructuredMatrix, py::arg("value"))
        .def("getRestructuredMatrix", &System::getRestructuredMatrix)
        .def("setYield", &System::setYield, py::arg("yield"))
        .def("getYield", &System::getYield)
        .def("setRadiusInLattice", &System::setRadiusInLattice, py::arg("radius"))
        .def("getRadiusInLattice", &System::getRadiusInLattice)
        .def("setGasName", &System::setGasName, py::arg("name"))
        .def("getGasName", &System::getGasName)
        .def("setMatrixName", &System::setMatrixName, py::arg("name"))
        .def("getMatrixName", &System::getMatrixName)
        .def("getVolumeInLattice", &System::getVolumeInLattice)
        .def("setVolumeInLattice", &System::setVolumeInLattice, py::arg("volume"))
        .def("setBubbleDiffusivity", &System::setBubbleDiffusivity, py::arg("input_value"))
        .def("getBubbleDiffusivity", &System::getBubbleDiffusivity)
        .def("setHeliumDiffusivity", &System::setHeliumDiffusivity, py::arg("input_value"))
        .def("getHeliumDiffusivity", &System::getHeliumDiffusivity)
        .def("setFissionGasDiffusivity", &System::setFissionGasDiffusivity, py::arg("input_value"))
        .def("getFissionGasDiffusivity", &System::getFissionGasDiffusivity)
        .def("setHenryConstant", &System::setHenryConstant, py::arg("constant"))
        .def("getHenryConstant", &System::getHenryConstant)
        .def("setResolutionRate", &System::setResolutionRate, py::arg("input_value"))
        .def("getResolutionRate", &System::getResolutionRate)
        .def("setTrappingRate", &System::setTrappingRate, py::arg("input_value"))
        .def("getTrappingRate", &System::getTrappingRate)
        .def("setNucleationRate", &System::setNucleationRate, py::arg("input_value"))
        .def("getNucleationRate", &System::getNucleationRate)
        .def("setPoreNucleationRate", &System::setPoreNucleationRate, py::arg("rate"))
        .def("getPoreNucleationRate", &System::getPoreNucleationRate)
        .def("setProductionRate", &System::setProductionRate, py::arg("input_value"))
        .def("getProductionRate", &System::getProductionRate);

    py::class_<Variable, Entity>(m, "Variable")
        .def(py::init<>());

    py::class_<PhysicsVariable, Variable>(m, "PhysicsVariable")
        .def(py::init<>())
        .def("rescaleInitialValue", &PhysicsVariable::rescaleInitialValue, py::arg("factor"))
        .def("rescaleFinalValue", &PhysicsVariable::rescaleFinalValue, py::arg("factor"))
        .def("add_value", &PhysicsVariable::addValue, py::arg("v"))
        .def("set_uom", &PhysicsVariable::setUOM, py::arg("s"))
        .def("get_uom", &PhysicsVariable::getUOM)
        .def("set_constant", &PhysicsVariable::setConstant)
        .def("reset_value", &PhysicsVariable::resetValue)
        .def("set_final_value", &PhysicsVariable::setFinalValue, py::arg("FinalValue"))
        .def("get_final_value", &PhysicsVariable::getFinalValue)
        .def("set_initial_value", &PhysicsVariable::setInitialValue, py::arg("InitialValue"))
        .def("get_initial_value", &PhysicsVariable::getInitialValue)
        .def("get_increment", &PhysicsVariable::getIncrement)
        .def("set_output", &PhysicsVariable::setOutput, py::arg("io"))
        .def("get_output", &PhysicsVariable::getOutput);

    py::class_<HistoryVariable, PhysicsVariable>(m, "HistoryVariable")
        .def(py::init<>());

    py::class_<SciantixVariable, PhysicsVariable>(m, "SciantixVariable")
        .def(py::init<>());

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
        .def("BinaryInteractionVerification", &Solver::BinaryInteractionVerification, py::arg("initial_condition"), py::arg("interaction_coefficient"), py::arg("increment"), py::arg("mode"))
        .def("SpectralDiffusion", [](Solver& self, py::array_t<double> initial_condition,
                                 py::list parameter,
                                 double increment) {
            // Validate input types
            auto initial_condition_buf = initial_condition.request();
            if (initial_condition_buf.ndim != 1) {
                throw std::runtime_error("initial_condition must be 1-dimensional");
            }
            
            // Convert parameter list to std::vector<double>
            std::vector<double> parameter_vec;
            for (py::handle item : parameter) {
                parameter_vec.push_back(item.cast<double>());
            }
            
            // Get pointer to initial_condition data
            double *initial_condition_ptr = static_cast<double *>(initial_condition_buf.ptr);

            // Call C++ function
            return self.SpectralDiffusion(initial_condition_ptr, parameter_vec, increment);},
        py::arg("initial_condition"), py::arg("parameter"), py::arg("increment"))
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

    py::class_<Model, InputVariable, SciantixVariable, HistoryVariable, System>(m, "Model")
        .def(py::init<>())
        .def("setParameter", &Model::setParameter,
            py::arg("p"))
        .def("getParameter", &Model::getParameter);

    py::class_<Simulation, Model, Solver>(m, "Simulation")
        .def(py::init<>())
        .def("Burnup", &Simulation::Burnup, "Computes the fuel burnup from the local power density.")
        .def("EffectiveBurnup", &Simulation::EffectiveBurnup, "Computes the effective burnup of the fuel.")
        .def("GasProduction", &Simulation::GasProduction, "Computes the gas produced from the production rate.")
        .def("GasDecay", &Simulation::GasDecay, "Computes the decay of gases.")
        .def("GasDiffusion", &Simulation::GasDiffusion, "Computes gas diffusion based on selected models and conditions.")
        .def("GrainGrowth", &Simulation::GrainGrowth, "Calculates grain growth in materials.")
        .def("IntraGranularBubbleBehaviour", &Simulation::IntraGranularBubbleBehaviour, "Simulates the behavior of intragranular bubbles.")
        .def("InterGranularBubbleBehaviour", &Simulation::InterGranularBubbleBehaviour, "Simulates the behavior of intergranular bubbles.")
        .def("GrainBoundarySweeping", &Simulation::GrainBoundarySweeping, "Processes grain boundary sweeping.")
        .def("GrainBoundaryMicroCracking", &Simulation::GrainBoundaryMicroCracking, "Handles grain boundary microcracking induced by temperature changes.")
        .def("GrainBoundaryVenting", &Simulation::GrainBoundaryVenting, "Processes grain boundary venting.")
        .def("HighBurnupStructureFormation", &Simulation::HighBurnupStructureFormation, "Simulates the formation of high burnup structures.")
        .def("HighBurnupStructurePorosity", &Simulation::HighBurnupStructurePorosity, "Calculates the porosity of high burnup structures.")
        .def("StoichiometryDeviation", &Simulation::StoichiometryDeviation, "Manages stoichiometry deviations in materials.")
        .def("UO2Thermochemistry", &Simulation::UO2Thermochemistry, "Manages UO2 thermochemical processes.")
        .def("getDiffusionModes", &Simulation::getDiffusionModes, py::arg("gas_name"), "Returns a pointer to the array of diffusion modes for a specified gas.")
        .def("getDiffusionModesSolution", &Simulation::getDiffusionModesSolution, py::arg("gas_name"), "Returns a pointer to the array of solution diffusion modes for a specified gas.")
        .def("getDiffusionModesBubbles", &Simulation::getDiffusionModesBubbles, py::arg("gas_name"), "Returns a pointer to the array of bubble diffusion modes for a specified gas.");
    
}
