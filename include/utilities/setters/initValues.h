#include <vector>
#include "PhysicsVariable.h"
#include "MainVariables.h"



std::vector<std::string> getInputVariableNames()
{
    std::vector<std::string> names = {
        "iGrainGrowth", "iFGDiffusionCoefficient", "iDiffusionSolver", "iIntraGranularBubbleEvolution", 
        "iResolutionRate", "iTrappingRate", "iNucleationRate", "iOutput", 
        "iGrainBoundaryVacancyDiffusivity", "iGrainBoundaryBehaviour", "iGrainBoundaryMicroCracking", 
        "iFuelMatrix", "iGrainBoundaryVenting", "iRadioactiveFissionGas", "iHelium", 
        "iHeDiffusivity", "iGrainBoundarySweeping", "iHighBurnupStructureFormation", 
        "iHighBurnupStructurePorosity", "iHeliumProductionRate", "iStoichiometryDeviation", "iBubbleDiffusivity"
    };

    return names;
}



std::vector<PhysicsVariable> initHistoryVariableValues(
    double Sciantix_history[],
    bool toOutputStoichiometryDeviation
)
{
    std::vector<PhysicsVariable> history_variable = {
        PhysicsVariable("Time", "(h)", Sciantix_history[7], Sciantix_history[7], 1),
        PhysicsVariable("Time step number", "(/)", Sciantix_history[8], Sciantix_history[8], 0),
        PhysicsVariable("Temperature", "(K)", Sciantix_history[0] * Sciantix_scaling_factors[4], Sciantix_history[1] * Sciantix_scaling_factors[4], 1),
        PhysicsVariable("Fission rate", "(fiss / m3 s)", Sciantix_history[2] * Sciantix_scaling_factors[5], Sciantix_history[3] * Sciantix_scaling_factors[5], 1),
        PhysicsVariable("Hydrostatic stress", "(MPa)", Sciantix_history[4], Sciantix_history[5], 1),
        PhysicsVariable("Steam pressure", "(atm)", Sciantix_history[9], Sciantix_history[10], toOutputStoichiometryDeviation)
    };

    return history_variable;
}


std::vector<PhysicsVariable> initSciantixVariableValues(
    double Sciantix_variables[], 
    bool toOutputRadioactiveFG,
    bool toOutputVenting,
    bool toOutputHelium,
    bool toOutputCracking,
    bool toOutputFracture,
    bool toOutputGrainBoundary,
    bool toOutputHighBurnupStructure,
    bool toOutputStoichiometryDeviation
)
{
    std::vector<PhysicsVariable> init_sciantix_variable = {
        PhysicsVariable("Grain radius", "(m)", Sciantix_variables[0], Sciantix_variables[0], 1),

        PhysicsVariable("Xe produced", "(at/m3)", Sciantix_variables[1], Sciantix_variables[1], 1),
        PhysicsVariable("Xe produced in HBS", "(at/m3)", Sciantix_variables[100], Sciantix_variables[100], toOutputHighBurnupStructure),
        PhysicsVariable("Xe in grain", "(at/m3)", Sciantix_variables[2], Sciantix_variables[2], 1),
        PhysicsVariable("Xe in grain HBS", "(at/m3)", Sciantix_variables[92], Sciantix_variables[92], toOutputHighBurnupStructure),
        PhysicsVariable("Xe in intragranular solution", "(at/m3)", Sciantix_variables[3], Sciantix_variables[3], 1),
        PhysicsVariable("Xe in intragranular bubbles", "(at/m3)", Sciantix_variables[4], Sciantix_variables[4], 1),
        PhysicsVariable("Xe at grain boundary", "(at/m3)", Sciantix_variables[5], Sciantix_variables[5], 1),
        PhysicsVariable("Xe released", "(at/m3)", Sciantix_variables[6], Sciantix_variables[6], 1),
        PhysicsVariable("Xe decayed", "(at/m3)", 0.0, 0.0, 0),

        PhysicsVariable("Kr produced", "(at/m3)", Sciantix_variables[7], Sciantix_variables[7], 1),
        PhysicsVariable("Kr in grain", "(at/m3)", Sciantix_variables[8], Sciantix_variables[8], 1),
        PhysicsVariable("Kr in intragranular solution", "(at/m3)", Sciantix_variables[9], Sciantix_variables[9], 1),
        PhysicsVariable("Kr in intragranular bubbles", "(at/m3)", Sciantix_variables[10], Sciantix_variables[10], 1),
        PhysicsVariable("Kr at grain boundary", "(at/m3)", Sciantix_variables[11], Sciantix_variables[11], 1),
        PhysicsVariable("Kr released", "(at/m3)", Sciantix_variables[12], Sciantix_variables[12], 1),
        PhysicsVariable("Kr decayed", "(at/m3)", 0.0, 0.0, 0),

        PhysicsVariable("Fission gas release", "(/)", 0.0, 0.0, 1),

        PhysicsVariable("He produced", "(at/m3)", Sciantix_variables[13], Sciantix_variables[13], toOutputHelium),
        PhysicsVariable("He in grain", "(at/m3)", Sciantix_variables[14], Sciantix_variables[14], toOutputHelium),
        PhysicsVariable("He in intragranular solution", "(at/m3)", Sciantix_variables[15], Sciantix_variables[15], toOutputHelium),
        PhysicsVariable("He in intragranular bubbles", "(at/m3)", Sciantix_variables[16], Sciantix_variables[16], toOutputHelium),
        PhysicsVariable("He at grain boundary", "(at/m3)", Sciantix_variables[17], Sciantix_variables[71], toOutputHelium),
        PhysicsVariable("He released", "(at/m3)", Sciantix_variables[18], Sciantix_variables[18], toOutputHelium),
        PhysicsVariable("He decayed", "(at/m3)", 0.0, 0.0, 0),
        PhysicsVariable("He fractional released", "(/)", 0.0, 0.0, toOutputHelium),
        PhysicsVariable("He release rate", "(at/m3 s)", 0.0, 0.0, toOutputHelium),

        PhysicsVariable("Xe133 produced", "(at/m3)", Sciantix_variables[48], Sciantix_variables[48], toOutputRadioactiveFG),
        PhysicsVariable("Xe133 in grain", "(at/m3)", Sciantix_variables[49], Sciantix_variables[49], toOutputRadioactiveFG),
        PhysicsVariable("Xe133 in intragranular solution", "(at/m3)", Sciantix_variables[50], Sciantix_variables[50], toOutputRadioactiveFG),
        PhysicsVariable("Xe133 in intragranular bubbles", "(at/m3)", Sciantix_variables[51], Sciantix_variables[51], toOutputRadioactiveFG),
        PhysicsVariable("Xe133 decayed", "(at/m3)", Sciantix_variables[51], Sciantix_variables[51], 0),
        PhysicsVariable("Xe133 at grain boundary", "(at/m3)", Sciantix_variables[53], Sciantix_variables[53], toOutputRadioactiveFG),
        PhysicsVariable("Xe133 released", "(at/m3)", Sciantix_variables[54], Sciantix_variables[54], toOutputRadioactiveFG),
        PhysicsVariable("Xe133 R/B", "(/)", 0.0, 0.0, toOutputRadioactiveFG),

        PhysicsVariable("Kr85m produced", "(at/m3)", Sciantix_variables[57], Sciantix_variables[57], toOutputRadioactiveFG),
        PhysicsVariable("Kr85m in grain", "(at/m3)", Sciantix_variables[58], Sciantix_variables[58], toOutputRadioactiveFG),
        PhysicsVariable("Kr85m in intragranular solution", "(at/m3)", Sciantix_variables[59], Sciantix_variables[59], toOutputRadioactiveFG),
        PhysicsVariable("Kr85m in intragranular bubbles", "(at/m3)", Sciantix_variables[60], Sciantix_variables[60], toOutputRadioactiveFG),
        PhysicsVariable("Kr85m decayed", "(at/m3)", Sciantix_variables[61], Sciantix_variables[61], 0),
        PhysicsVariable("Kr85m at grain boundary", "(at/m3)", Sciantix_variables[64], Sciantix_variables[64], toOutputRadioactiveFG),
        PhysicsVariable("Kr85m released", "(at/m3)", Sciantix_variables[65], Sciantix_variables[65], toOutputRadioactiveFG),
        PhysicsVariable("Kr85m R/B", "(/)", 0.0, 0.0, toOutputRadioactiveFG),       



        PhysicsVariable("Intragranular bubble concentration", "(bub/m3)", Sciantix_variables[19], Sciantix_variables[19], 1),
        PhysicsVariable("Intragranular bubble radius", "(m)", Sciantix_variables[20], Sciantix_variables[20], 1),
        PhysicsVariable("Intragranular bubble volume", "(m3)", 0.0, 0.0, 0),

        PhysicsVariable("Intragranular Xe atoms per bubble", "(at/bub)", Sciantix_variables[21], Sciantix_variables[21], 0),
        PhysicsVariable("Intragranular Kr atoms per bubble", "(at/bub)", Sciantix_variables[22], Sciantix_variables[22], 0),
        PhysicsVariable("Intragranular He atoms per bubble", "(at/bub)", Sciantix_variables[23], Sciantix_variables[23], 0),
        PhysicsVariable("Intragranular atoms per bubble", "(at/bub)", Sciantix_variables[21] + Sciantix_variables[22] + Sciantix_variables[23], Sciantix_variables[21] + Sciantix_variables[22] + Sciantix_variables[23], 0),

        PhysicsVariable("Intragranular gas bubble swelling", "(/)", Sciantix_variables[24], Sciantix_variables[24], 1),
        PhysicsVariable("Intragranular gas solution swelling", "(/)", Sciantix_variables[68], Sciantix_variables[68], toOutputHighBurnupStructure),
        PhysicsVariable("Intergranular bubble concentration", "(bub/m2)", Sciantix_variables[25], Sciantix_variables[25], toOutputGrainBoundary),

        PhysicsVariable("Intergranular Xe atoms per bubble", "(at/bub)", Sciantix_variables[26], Sciantix_variables[26], 0),
        PhysicsVariable("Intergranular Kr atoms per bubble", "(at/bub)", Sciantix_variables[27], Sciantix_variables[27], 0),
        PhysicsVariable("Intergranular He atoms per bubble", "(at/bub)", Sciantix_variables[28], Sciantix_variables[28], 0),
        PhysicsVariable("Intergranular atoms per bubble", "(at/bub)", Sciantix_variables[29], Sciantix_variables[29], toOutputGrainBoundary),
        PhysicsVariable("Intergranular vacancies per bubble", "(vac/bub)", Sciantix_variables[30], Sciantix_variables[30], toOutputGrainBoundary),

        PhysicsVariable("Intergranular bubble pressure", "(MPa)", 0.0, 0.0, toOutputFracture),
        PhysicsVariable("Critical intergranular bubble pressure", "(MPa)", 0.0, 0.0, toOutputFracture),
        PhysicsVariable("Intergranular bubble radius", "(m)", Sciantix_variables[31], Sciantix_variables[31], toOutputGrainBoundary),
        PhysicsVariable("Intergranular bubble area", "(m2)", Sciantix_variables[32], Sciantix_variables[32], toOutputGrainBoundary),
        PhysicsVariable("Intergranular bubble volume", "(m3)", Sciantix_variables[33], Sciantix_variables[33], toOutputGrainBoundary),

        PhysicsVariable("Intergranular fractional coverage", "(/)", Sciantix_variables[34], Sciantix_variables[34], toOutputGrainBoundary), 
        PhysicsVariable("Intergranular saturation fractional coverage", "(/)", Sciantix_variables[35], Sciantix_variables[35], toOutputGrainBoundary),
        PhysicsVariable("Intergranular gas swelling", "(/)", Sciantix_variables[36], Sciantix_variables[36], toOutputGrainBoundary),
        PhysicsVariable("Intergranular fractional intactness", "(/)", Sciantix_variables[37], Sciantix_variables[37], toOutputCracking),

        PhysicsVariable("Burnup", "(MWd/kgUO2)", Sciantix_variables[38], Sciantix_variables[38], 1),
        PhysicsVariable("FIMA", "(%)", Sciantix_variables[69], Sciantix_variables[69], toOutputHighBurnupStructure),
        PhysicsVariable("Effective burnup", "(MWd/kgUO2)", Sciantix_variables[39], Sciantix_variables[39], toOutputHighBurnupStructure),
        PhysicsVariable("Irradiation time", "(h)", Sciantix_variables[65], Sciantix_variables[65], 0),
        PhysicsVariable("Fuel density", "(kg/m3)", Sciantix_variables[40], Sciantix_variables[40], 0),

        PhysicsVariable("U", "(at/m3)", Sciantix_variables[41] + Sciantix_variables[42] + Sciantix_variables[43] + Sciantix_variables[44] + Sciantix_variables[45], Sciantix_variables[41] + Sciantix_variables[42] + Sciantix_variables[43] + Sciantix_variables[44] + Sciantix_variables[45], 0),
        PhysicsVariable("U234", "(at/m3)", Sciantix_variables[41], Sciantix_variables[41], 0),
        PhysicsVariable("U235", "(at/m3)", Sciantix_variables[42], Sciantix_variables[42], 1),
        PhysicsVariable("U236", "(at/m3)", Sciantix_variables[43], Sciantix_variables[43], 0),
        PhysicsVariable("U237", "(at/m3)", Sciantix_variables[44], Sciantix_variables[44], 0),
        PhysicsVariable("U238", "(at/m3)", Sciantix_variables[45], Sciantix_variables[45], 1),
        PhysicsVariable("U", "(at/m3)", Sciantix_variables[41] + Sciantix_variables[42] + Sciantix_variables[43] + Sciantix_variables[44] + Sciantix_variables[45], Sciantix_variables[41] + Sciantix_variables[42] + Sciantix_variables[43] + Sciantix_variables[44] + Sciantix_variables[45], 0),

        PhysicsVariable("Intergranular vented fraction", "(/)", Sciantix_variables[46], Sciantix_variables[46], toOutputVenting),
        PhysicsVariable("Intergranular venting probability", "(/)", Sciantix_variables[47], Sciantix_variables[47], toOutputVenting),

        PhysicsVariable("Restructured volume fraction", "(/)", Sciantix_variables[55], Sciantix_variables[55], toOutputHighBurnupStructure),
        PhysicsVariable("Intragranular similarity ratio", "(/)", Sciantix_variables[64], Sciantix_variables[64], 0),
        PhysicsVariable("Specific power", "(MW/kg)", 0.0, 0.0, 0),
        PhysicsVariable("Gap oxygen partial pressure", "(MPa)", 0.0, 0.0, toOutputStoichiometryDeviation),
        PhysicsVariable("Stoichiometry deviation", "(/)", Sciantix_variables[66], Sciantix_variables[66], toOutputStoichiometryDeviation),
        PhysicsVariable("Equilibrium stoichiometry deviation", "(/)", 0.0, 0.0, toOutputStoichiometryDeviation),

        PhysicsVariable("Fuel oxygen partial pressure", "(MPa)", Sciantix_variables[67], Sciantix_variables[67], toOutputStoichiometryDeviation),
        PhysicsVariable("Fuel oxygen potential", "(KJ/mol)", 0.0, 0.0, toOutputStoichiometryDeviation),
        PhysicsVariable("Specific power", "(MW/kg)", 0.0, 0.0, 0),

        // HBS-related variables
        PhysicsVariable("HBS porosity", "(/)", Sciantix_variables[56], Sciantix_variables[56], toOutputHighBurnupStructure),
        PhysicsVariable("HBS pore density", "(pores/m3)", Sciantix_variables[80], Sciantix_variables[80], toOutputHighBurnupStructure),
        PhysicsVariable("HBS pore volume", "(m3)", Sciantix_variables[81], Sciantix_variables[81], toOutputHighBurnupStructure),
        PhysicsVariable("HBS pore radius", "(m)", Sciantix_variables[82], Sciantix_variables[82], toOutputHighBurnupStructure),
        PhysicsVariable("Xe in HBS pores", "(at/m3)", Sciantix_variables[83], Sciantix_variables[83], toOutputHighBurnupStructure),
        PhysicsVariable("Xe in HBS pores - variance", "(at^2/m3)", Sciantix_variables[85], Sciantix_variables[85], toOutputHighBurnupStructure),
        PhysicsVariable("Xe atoms per HBS pore", "(at/pore)", Sciantix_variables[86], Sciantix_variables[86], toOutputHighBurnupStructure),
        PhysicsVariable("Xe atoms per HBS pore - variance", "(at^2/pore)", Sciantix_variables[88], Sciantix_variables[88], toOutputHighBurnupStructure)
    };

    return init_sciantix_variable;
}



std::vector<std::string> getScalingFactorsNames()
{
    std::vector<std::string> names = {
        "Resolution rate", "Trapping rate", "Nucleation rate", "Diffusivity",
        "Temperature", "Fission rate", "Cent parameter", "Helium production rate", "Dummy"
    };

    return names;
}