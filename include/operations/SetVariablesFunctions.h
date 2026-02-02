//////////////////////////////////////////////////////////////////////////////////////
//       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
//      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
//     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
//      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
//  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
//  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
//                                                                                  //
//  Originally developed by D. Pizzocri & T. Barani                                 //
//                                                                                  //
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SET_VARIABLES_FUNCTIONS_H
#define SET_VARIABLES_FUNCTIONS_H

#include <vector>
#include "SciantixVariable.h"
#include "ThermochemistryVariable.h"

/**
 * @brief Retrieves the list of input variable names.
 * @return A vector of strings containing the names of input variables.
 * 
 * @author F. Bastien
 * @author G. Zullo
 * 
 */

std::vector<std::string> getInputVariableNames()
{
    std::vector<std::string> names =
    {
        "iGrainGrowth",
        "iFissionGasDiffusivity",
        "iDiffusionSolver",
        "iIntraGranularBubbleBehavior", 
        "iResolutionRate",
        "iTrappingRate",
        "iNucleationRate",
        "iOutput", 
        "iGrainBoundaryVacancyDiffusivity",
        "iGrainBoundaryBehaviour",
        "iGrainBoundaryMicroCracking", 
        "iFuelMatrix",
        "iGrainBoundaryVenting",
        "iRadioactiveFissionGas",
        "iHelium", 
        "iHeDiffusivity",
        "iGrainBoundarySweeping",
        "iHighBurnupStructureFormation", 
        "iHighBurnupStructurePorosity",
        "iHeliumProductionRate",
        "iStoichiometryDeviation",
        "iBubbleDiffusivity",
        "iChromiumSolubility",
        "iDensification",
        "iReleaseMode",
        "iThermochimica"
    };

    return names;
}

/**
 * @brief Initializes the history variables.
 * @param Sciantix_history An array of history variables.
 * @param toOutputStoichiometryDeviation A flag indicating whether to consider additional history variables (currently, steam pressure).
 * @param Sciantix_scaling_factors An array of scaling factors.
 * @return A vector of SciantixVariable objects initialized with the given history variables.
 */
std::vector<SciantixVariable> initializeHistoryVariable(
    double Sciantix_history[],
    double Sciantix_scaling_factors[],
    bool toOutput
)
{
    std::vector<SciantixVariable> history_variable =
    {
        SciantixVariable("Time", "(h)", Sciantix_history[7], Sciantix_history[7], 1),
        SciantixVariable("Time step number", "(/)", Sciantix_history[8], Sciantix_history[8], 0),
        SciantixVariable("Temperature", "(K)", Sciantix_history[0] * Sciantix_scaling_factors[4], Sciantix_history[1] * Sciantix_scaling_factors[4], 1),
        SciantixVariable("Fission rate", "(fiss / m3 s)", Sciantix_history[2] * Sciantix_scaling_factors[5], Sciantix_history[3] * Sciantix_scaling_factors[5], 1),
        SciantixVariable("Hydrostatic stress", "(MPa)", Sciantix_history[4], Sciantix_history[5], 1),
        SciantixVariable("Steam pressure", "(atm)", Sciantix_history[9], Sciantix_history[10], toOutput),
        SciantixVariable("THERMOCHIMICA pressure", "(Pa)", Sciantix_history[11], Sciantix_history[12], 1)
    };

    return history_variable;
}

/**
 * @brief Initializes the Sciantix variables with the provided values and flags.
 * @param Sciantix_variables An array of Sciantix variable values.
 * @param toOutputRadioactiveFG Flag for outputting radioactive fission gas information.
 * @param toOutputVenting Flag for outputting venting information.
 * @param toOutputHelium Flag for outputting helium information.
 * @param toOutputCracking Flag for outputting cracking information.
 * @param toOutputGrainBoundary Flag for outputting grain boundary information.
 * @param toOutputHighBurnupStructure Flag for outputting high burnup structure information.
 * @param toOutputStoichiometryDeviation Flag for outputting stoichiometry deviation information.
 * @param toOutputThermochimica Flag for outputting thermochimica results.
 * @return A vector of SciantixVariable objects initialized with the given values and flags.
 */
std::vector<SciantixVariable> initializeSciantixVariable(
    double Sciantix_variables[], 
    bool toOutputRadioactiveFG,
    bool toOutputVenting,
    bool toOutputHelium,
    bool toOutputCracking,
    bool toOutputGrainBoundary,
    bool toOutputHighBurnupStructure,
    bool toOutputStoichiometryDeviation,
    bool toOutputChromiumContent,
    bool toOutputThermochimica
)
{
    std::vector<SciantixVariable> init_sciantix_variable =
    {
        SciantixVariable("Grain radius", "(m)", Sciantix_variables[0], Sciantix_variables[0], 1),

        SciantixVariable("Xe produced", "(at/m3)", Sciantix_variables[1], Sciantix_variables[1], 1),
        SciantixVariable("Xe produced in HBS", "(at/m3)", Sciantix_variables[100], Sciantix_variables[100], toOutputHighBurnupStructure),
        SciantixVariable("Xe in grain", "(at/m3)", Sciantix_variables[2], Sciantix_variables[2], 1),
        SciantixVariable("Xe in grain HBS", "(at/m3)", Sciantix_variables[92], Sciantix_variables[92], toOutputHighBurnupStructure),
        SciantixVariable("Xe in intragranular solution", "(at/m3)", Sciantix_variables[3], Sciantix_variables[3], 1),
        SciantixVariable("Xe in intragranular bubbles", "(at/m3)", Sciantix_variables[4], Sciantix_variables[4], 1),
        SciantixVariable("Xe at grain boundary", "(at/m3)", Sciantix_variables[5], Sciantix_variables[5], 1),
        SciantixVariable("Xe released", "(at/m3)", Sciantix_variables[6], Sciantix_variables[6], 1),
        SciantixVariable("Xe decayed", "(at/m3)", 0.0, 0.0, 0),

        SciantixVariable("Kr produced", "(at/m3)", Sciantix_variables[7], Sciantix_variables[7], 1),
        SciantixVariable("Kr in grain", "(at/m3)", Sciantix_variables[8], Sciantix_variables[8], 1),
        SciantixVariable("Kr in intragranular solution", "(at/m3)", Sciantix_variables[9], Sciantix_variables[9], 1),
        SciantixVariable("Kr in intragranular bubbles", "(at/m3)", Sciantix_variables[10], Sciantix_variables[10], 1),
        SciantixVariable("Kr at grain boundary", "(at/m3)", Sciantix_variables[11], Sciantix_variables[11], 1),
        SciantixVariable("Kr released", "(at/m3)", Sciantix_variables[12], Sciantix_variables[12], 1),
        SciantixVariable("Kr decayed", "(at/m3)", 0.0, 0.0, 0),

        SciantixVariable("Fission gas release", "(/)", 0.0, 0.0, 1),

        SciantixVariable("He produced", "(at/m3)", Sciantix_variables[13], Sciantix_variables[13], toOutputHelium),
        SciantixVariable("He in grain", "(at/m3)", Sciantix_variables[14], Sciantix_variables[14], toOutputHelium),
        SciantixVariable("He in intragranular solution", "(at/m3)", Sciantix_variables[15], Sciantix_variables[15], toOutputHelium),
        SciantixVariable("He in intragranular bubbles", "(at/m3)", Sciantix_variables[16], Sciantix_variables[16], toOutputHelium),
        SciantixVariable("He at grain boundary", "(at/m3)", Sciantix_variables[17], Sciantix_variables[71], toOutputHelium),
        SciantixVariable("He released", "(at/m3)", Sciantix_variables[18], Sciantix_variables[18], toOutputHelium),
        SciantixVariable("He decayed", "(at/m3)", 0.0, 0.0, 0),
        SciantixVariable("He fractional release", "(/)", 0.0, 0.0, toOutputHelium),
        SciantixVariable("He release rate", "(at/m3 s)", 0.0, 0.0, toOutputHelium),

        SciantixVariable("Xe133 produced", "(at/m3)", Sciantix_variables[48], Sciantix_variables[48], toOutputRadioactiveFG),
        SciantixVariable("Xe133 in grain", "(at/m3)", Sciantix_variables[49], Sciantix_variables[49], toOutputRadioactiveFG),
        SciantixVariable("Xe133 in intragranular solution", "(at/m3)", Sciantix_variables[50], Sciantix_variables[50], toOutputRadioactiveFG),
        SciantixVariable("Xe133 in intragranular bubbles", "(at/m3)", Sciantix_variables[51], Sciantix_variables[51], toOutputRadioactiveFG),
        SciantixVariable("Xe133 decayed", "(at/m3)", Sciantix_variables[52], Sciantix_variables[52], toOutputRadioactiveFG),
        SciantixVariable("Xe133 at grain boundary", "(at/m3)", Sciantix_variables[53], Sciantix_variables[53], toOutputRadioactiveFG),
        SciantixVariable("Xe133 released", "(at/m3)", Sciantix_variables[54], Sciantix_variables[54], toOutputRadioactiveFG),
        SciantixVariable("Xe133 R/B", "(/)", 0.0, 0.0, toOutputRadioactiveFG),

        SciantixVariable("Kr85m produced", "(at/m3)", Sciantix_variables[57], Sciantix_variables[57], toOutputRadioactiveFG),
        SciantixVariable("Kr85m in grain", "(at/m3)", Sciantix_variables[58], Sciantix_variables[58], toOutputRadioactiveFG),
        SciantixVariable("Kr85m in intragranular solution", "(at/m3)", Sciantix_variables[59], Sciantix_variables[59], toOutputRadioactiveFG),
        SciantixVariable("Kr85m in intragranular bubbles", "(at/m3)", Sciantix_variables[60], Sciantix_variables[60], toOutputRadioactiveFG),
        SciantixVariable("Kr85m decayed", "(at/m3)", Sciantix_variables[61], Sciantix_variables[61], toOutputRadioactiveFG),
        SciantixVariable("Kr85m at grain boundary", "(at/m3)", Sciantix_variables[62], Sciantix_variables[62], toOutputRadioactiveFG),
        SciantixVariable("Kr85m released", "(at/m3)", Sciantix_variables[63], Sciantix_variables[63], toOutputRadioactiveFG),
        SciantixVariable("Kr85m R/B", "(/)", 0.0, 0.0, toOutputRadioactiveFG),       

        SciantixVariable("Cs produced", "(at/m3)", Sciantix_variables[101], Sciantix_variables[101], 1),
        SciantixVariable("Cs in grain", "(at/m3)", Sciantix_variables[102], Sciantix_variables[102], 1),
        SciantixVariable("Cs in intragranular solution", "(at/m3)", Sciantix_variables[103], Sciantix_variables[103], 1),
        SciantixVariable("Cs in intragranular bubbles", "(at/m3)", Sciantix_variables[104], Sciantix_variables[104], 1),
        SciantixVariable("Cs at grain boundary", "(at/m3)", Sciantix_variables[105], Sciantix_variables[105], 1),
        SciantixVariable("Cs released", "(at/m3)", Sciantix_variables[106], Sciantix_variables[106], 1),
        SciantixVariable("Cs decayed", "(at/m3)", 0.0, 0.0, 0),
        SciantixVariable("Cs reacted - IG", "(at/m3)", Sciantix_variables[110], Sciantix_variables[110], 1),
        SciantixVariable("Cs reacted - GB", "(at/m3)", Sciantix_variables[107], Sciantix_variables[107], 1),
        SciantixVariable("Intragranular Cs atoms per bubble", "(at/bub)", Sciantix_variables[108], Sciantix_variables[108], 0),
        SciantixVariable("Intergranular Cs atoms per bubble", "(at/bub)", Sciantix_variables[109], Sciantix_variables[109], 0),

        SciantixVariable("I produced", "(at/m3)", Sciantix_variables[111], Sciantix_variables[111], 1),
        SciantixVariable("I in grain", "(at/m3)", Sciantix_variables[112], Sciantix_variables[112], 1),
        SciantixVariable("I in intragranular solution", "(at/m3)", Sciantix_variables[113], Sciantix_variables[113], 1),
        SciantixVariable("I in intragranular bubbles", "(at/m3)", Sciantix_variables[114], Sciantix_variables[114], 1),
        SciantixVariable("I at grain boundary", "(at/m3)", Sciantix_variables[115], Sciantix_variables[115], 1),
        SciantixVariable("I released", "(at/m3)", Sciantix_variables[116], Sciantix_variables[116], 1),
        SciantixVariable("I decayed", "(at/m3)", 0.0, 0.0, 0),
        SciantixVariable("I reacted - IG", "(at/m3)", Sciantix_variables[120], Sciantix_variables[120], 1),
        SciantixVariable("I reacted - GB", "(at/m3)", Sciantix_variables[117], Sciantix_variables[117], 1),
        SciantixVariable("Intragranular I atoms per bubble", "(at/bub)", Sciantix_variables[118], Sciantix_variables[118], 0),
        SciantixVariable("Intergranular I atoms per bubble", "(at/bub)", Sciantix_variables[119], Sciantix_variables[119], 0),

        SciantixVariable("Te produced", "(at/m3)", Sciantix_variables[121], Sciantix_variables[121], 1),
        SciantixVariable("Te in grain", "(at/m3)", Sciantix_variables[122], Sciantix_variables[122], 1),
        SciantixVariable("Te in intragranular solution", "(at/m3)", Sciantix_variables[123], Sciantix_variables[123], 1),
        SciantixVariable("Te in intragranular bubbles", "(at/m3)", Sciantix_variables[124], Sciantix_variables[124], 1),
        SciantixVariable("Te at grain boundary", "(at/m3)", Sciantix_variables[125], Sciantix_variables[125], 1),
        SciantixVariable("Te released", "(at/m3)", Sciantix_variables[126], Sciantix_variables[126], 1),
        SciantixVariable("Te decayed", "(at/m3)", 0.0, 0.0, 0),
        SciantixVariable("Te reacted - IG", "(at/m3)", Sciantix_variables[130], Sciantix_variables[130], 1),
        SciantixVariable("Te reacted - GB", "(at/m3)", Sciantix_variables[127], Sciantix_variables[127], 1),
        SciantixVariable("Intragranular Te atoms per bubble", "(at/bub)", Sciantix_variables[128], Sciantix_variables[128], 0),
        SciantixVariable("Intergranular Te atoms per bubble", "(at/bub)", Sciantix_variables[129], Sciantix_variables[129], 0),

        SciantixVariable("Intragranular bubble concentration", "(bub/m3)", Sciantix_variables[19], Sciantix_variables[19], 1),
        SciantixVariable("Intragranular bubble radius", "(m)", Sciantix_variables[20], Sciantix_variables[20], 1),
        SciantixVariable("Intragranular bubble volume", "(m3)", 0.0, 0.0, 0),

        SciantixVariable("Intragranular Xe atoms per bubble", "(at/bub)", Sciantix_variables[21], Sciantix_variables[21], 0),
        SciantixVariable("Intragranular Kr atoms per bubble", "(at/bub)", Sciantix_variables[22], Sciantix_variables[22], 0),
        SciantixVariable("Intragranular He atoms per bubble", "(at/bub)", Sciantix_variables[23], Sciantix_variables[23], 0),
        SciantixVariable("Intragranular atoms per bubble", "(at/bub)", Sciantix_variables[21] + Sciantix_variables[22] + Sciantix_variables[23], Sciantix_variables[21] + Sciantix_variables[22] + Sciantix_variables[23], 0),

        SciantixVariable("Intragranular gas bubble swelling", "(/)", Sciantix_variables[24], Sciantix_variables[24], 1),
        SciantixVariable("Intragranular gas solution swelling", "(/)", Sciantix_variables[68], Sciantix_variables[68], toOutputHighBurnupStructure),
        SciantixVariable("Intergranular bubble concentration", "(bub/m2)", Sciantix_variables[25], Sciantix_variables[25], 1),

        SciantixVariable("Intergranular Xe atoms per bubble", "(at/bub)", Sciantix_variables[26], Sciantix_variables[26], 0),
        SciantixVariable("Intergranular Kr atoms per bubble", "(at/bub)", Sciantix_variables[27], Sciantix_variables[27], 0),
        SciantixVariable("Intergranular He atoms per bubble", "(at/bub)", Sciantix_variables[28], Sciantix_variables[28], 0),
        SciantixVariable("Intergranular atoms per bubble", "(at/bub)", Sciantix_variables[29], Sciantix_variables[29], 1),
        SciantixVariable("Intergranular vacancies per bubble", "(vac/bub)", Sciantix_variables[30], Sciantix_variables[30], 1),

        SciantixVariable("Intergranular bubble pressure", "(MPa)", 0.0, 0.0, 0),
        SciantixVariable("Critical intergranular bubble pressure", "(MPa)", 0.0, 0.0, 0),
        SciantixVariable("Intergranular bubble radius", "(m)", Sciantix_variables[31], Sciantix_variables[31], 1),
        SciantixVariable("Intergranular bubble area", "(m2)", Sciantix_variables[32], Sciantix_variables[32], 1),
        SciantixVariable("Intergranular bubble volume", "(m3)", Sciantix_variables[33], Sciantix_variables[33], 1),

        SciantixVariable("Intergranular fractional coverage", "(/)", Sciantix_variables[34], Sciantix_variables[34], 1), 
        SciantixVariable("Intergranular saturation fractional coverage", "(/)", Sciantix_variables[35], Sciantix_variables[35], 1),
        SciantixVariable("Intergranular gas swelling", "(/)", Sciantix_variables[36], Sciantix_variables[36], 1),
        SciantixVariable("Intergranular fractional intactness", "(/)", Sciantix_variables[37], Sciantix_variables[37], 1),

        SciantixVariable("Burnup", "(MWd/kgMOX)", Sciantix_variables[38], Sciantix_variables[38], 1),
        SciantixVariable("FIMA", "(%)", Sciantix_variables[69], Sciantix_variables[69], toOutputHighBurnupStructure || toOutputChromiumContent),
        SciantixVariable("Effective burnup", "(MWd/kgMOX)", Sciantix_variables[39], Sciantix_variables[39], toOutputHighBurnupStructure),
        SciantixVariable("Irradiation time", "(h)", Sciantix_variables[65], Sciantix_variables[65], 0),
        SciantixVariable("Fuel density", "(kg/m3)", Sciantix_variables[40], Sciantix_variables[40], 1),

        SciantixVariable("U", "(at/m3)", Sciantix_variables[41] + Sciantix_variables[42] + Sciantix_variables[43] + Sciantix_variables[44] + Sciantix_variables[45], Sciantix_variables[41] + Sciantix_variables[42] + Sciantix_variables[43] + Sciantix_variables[44] + Sciantix_variables[45], 0),
        SciantixVariable("U234", "(at/m3)", Sciantix_variables[41], Sciantix_variables[41], 0),
        SciantixVariable("U235", "(at/m3)", Sciantix_variables[42], Sciantix_variables[42], 1),
        SciantixVariable("U236", "(at/m3)", Sciantix_variables[43], Sciantix_variables[43], 0),
        SciantixVariable("U237", "(at/m3)", Sciantix_variables[44], Sciantix_variables[44], 0),
        SciantixVariable("U238", "(at/m3)", Sciantix_variables[45], Sciantix_variables[45], 1),

        // MOX Pu composition and ratio
        SciantixVariable("Pu", "(at/m3)", Sciantix_variables[171] + Sciantix_variables[172] + Sciantix_variables[173] + Sciantix_variables[174] + Sciantix_variables[175], Sciantix_variables[171] + Sciantix_variables[172] + Sciantix_variables[173] + Sciantix_variables[174] + Sciantix_variables[175], 0),
        SciantixVariable("Pu238", "(at/m3)", Sciantix_variables[171], Sciantix_variables[171], 1),
        SciantixVariable("Pu239", "(at/m3)", Sciantix_variables[172], Sciantix_variables[172], 1),
        SciantixVariable("Pu240", "(at/m3)", Sciantix_variables[173], Sciantix_variables[173], 1),
        SciantixVariable("Pu241", "(at/m3)", Sciantix_variables[174], Sciantix_variables[174], 1),
        SciantixVariable("Pu242", "(at/m3)", Sciantix_variables[175], Sciantix_variables[175], 1),
        SciantixVariable("q", "(-)", Sciantix_variables[177], Sciantix_variables[177], 1),


        SciantixVariable("Intergranular vented fraction", "(/)", Sciantix_variables[46], Sciantix_variables[46], toOutputVenting),
        SciantixVariable("Intergranular venting probability", "(/)", Sciantix_variables[47], Sciantix_variables[47], toOutputVenting),
        
        SciantixVariable("Porosity", "(/)", Sciantix_variables[70], Sciantix_variables[70], toOutputVenting),
        SciantixVariable("Fabrication porosity", "(/)", Sciantix_variables[71], Sciantix_variables[71], toOutputVenting),
        SciantixVariable("Open porosity", "(/)", Sciantix_variables[72], Sciantix_variables[72], toOutputVenting),
        SciantixVariable("Residual porosity", "(/)", Sciantix_variables[73], Sciantix_variables[73], toOutputVenting),
        SciantixVariable("Densification factor", "(/)", Sciantix_variables[74], Sciantix_variables[74], toOutputVenting),

        SciantixVariable("Restructured volume fraction", "(/)", Sciantix_variables[55], Sciantix_variables[55], toOutputHighBurnupStructure),
        SciantixVariable("Intragranular similarity ratio", "(/)", Sciantix_variables[64], Sciantix_variables[64], 0),
        SciantixVariable("Gap oxygen partial pressure", "(MPa)", 0.0, 0.0, toOutputStoichiometryDeviation),
        SciantixVariable("Stoichiometry deviation", "(/)", Sciantix_variables[66], Sciantix_variables[66], 1),
        SciantixVariable("Equilibrium stoichiometry deviation", "(/)", 0.0, 0.0, toOutputStoichiometryDeviation),

        SciantixVariable("Fuel oxygen partial pressure", "(MPa)", Sciantix_variables[67], Sciantix_variables[67], toOutputStoichiometryDeviation),
        SciantixVariable("Fuel oxygen potential", "(KJ/mol)", 0.0, 0.0, toOutputStoichiometryDeviation),
        SciantixVariable("Specific power", "(MW/kg)", 0.0, 0.0, 0),

        SciantixVariable("HBS porosity", "(/)", Sciantix_variables[56], Sciantix_variables[56], toOutputHighBurnupStructure),
        SciantixVariable("HBS pore density", "(pores/m3)", Sciantix_variables[80], Sciantix_variables[80], toOutputHighBurnupStructure),
        SciantixVariable("HBS pore volume", "(m3)", Sciantix_variables[81], Sciantix_variables[81], toOutputHighBurnupStructure),
        SciantixVariable("HBS pore radius", "(m)", Sciantix_variables[82], Sciantix_variables[82], toOutputHighBurnupStructure),
        SciantixVariable("Xe in HBS pores", "(at/m3)", Sciantix_variables[83], Sciantix_variables[83], toOutputHighBurnupStructure),
        SciantixVariable("Xe in HBS pores - variance", "(at^2/m3)", Sciantix_variables[85], Sciantix_variables[85], toOutputHighBurnupStructure),
        SciantixVariable("Xe atoms per HBS pore", "(at/pore)", Sciantix_variables[86], Sciantix_variables[86], toOutputHighBurnupStructure),
        SciantixVariable("Xe atoms per HBS pore - variance", "(at^2/pore)", Sciantix_variables[88], Sciantix_variables[88], toOutputHighBurnupStructure),

        SciantixVariable("Chromium content", "(µg/g)", Sciantix_variables[150], Sciantix_variables[150], toOutputChromiumContent),
        SciantixVariable("Lattice parameter", "(m)", Sciantix_variables[151], Sciantix_variables[151], 0),
        SciantixVariable("Theoretical density", "(kg/m3)", Sciantix_variables[152], Sciantix_variables[152], 0),
        SciantixVariable("Chromium solubility", "(% weight/UO2)", Sciantix_variables[153], Sciantix_variables[153], toOutputChromiumContent),
        SciantixVariable("Chromia solubility", "(% weight/UO2)", Sciantix_variables[154], Sciantix_variables[154], toOutputChromiumContent),
        SciantixVariable("Chromium solution", "(at/m3)", Sciantix_variables[155], Sciantix_variables[155], toOutputChromiumContent),
        SciantixVariable("Chromium precipitate", "(at/m3)", Sciantix_variables[156], Sciantix_variables[156], toOutputChromiumContent),
        SciantixVariable("Chromia solution", "(at/m3)", Sciantix_variables[157], Sciantix_variables[157], toOutputChromiumContent),
        SciantixVariable("Chromia precipitate", "(at/m3)", Sciantix_variables[158], Sciantix_variables[158], toOutputChromiumContent),

        SciantixVariable("Diffusion coefficient", "(m2/s)", Sciantix_variables[160], Sciantix_variables[160], 0),
        SciantixVariable("Uranium content", "(mol/m3)", Sciantix_variables[161], Sciantix_variables[161], 1),
        SciantixVariable("Oxygen content", "(mol/m3)", Sciantix_variables[162], Sciantix_variables[162], 1),
        SciantixVariable("Plutonium content", "(mol/m3)", Sciantix_variables[163], Sciantix_variables[163], 1),
        
        SciantixVariable("Initial grain radius", "(mol)", Sciantix_variables[170], Sciantix_variables[170], 0),

    };

    return init_sciantix_variable;
}
    
std::vector<ThermochemistryVariable> initializeThermochemistryVariable(
    double Sciantix_thermochemistry[]
)
{   
    std::vector<ThermochemistryVariable> init_thermochemistry_variable;

    std::vector<std::string> locations;
    
    locations.push_back("in grain");
    locations.push_back("at grain boundary");
    locations.push_back("in the gap");
    
    // Read from JSON file the compounds of interest
    std::string jsonPath = "./input_thermochemistry.json";

    std::ifstream jsonFile(jsonPath);
    if (!jsonFile) {
        std::cerr << "Error: Cannot open thermochemistry input: " << jsonPath << std::endl;
    }

    Json::Value root;
    jsonFile >> root;
    root = root["Compounds"];

    // Counter to index Sciantix_thermochemistry
    int index = 0;

    for (const auto& type : root.getMemberNames())
    {
        for (const auto& phase : root[type].getMemberNames())
        {
            for (const auto& compound : root[type][phase].getMemberNames())
            {
                std::map<std::string, int> stoichiometry;
                for (const auto& element : root[type][phase][compound].getMemberNames()) {
                    stoichiometry[element] = root[type][phase][compound][element].asInt();
                } 

                auto locations_to_use = (type == "matrix") ? std::vector<std::string>{"matrix"} : locations;

                for (const auto& location : locations_to_use)
                {
                    double final_value = 0;
                    if (type == "matrix") final_value = Sciantix_thermochemistry[index];
                    
                    init_thermochemistry_variable.emplace_back(
                        ThermochemistryVariable(
                            compound + " (" + phase + ", " + location + ")", // name
                            "(mol/m3)",                           // unit
                            Sciantix_thermochemistry[index],     // initial_value
                            final_value,     // final_value
                            phase,                               // phase
                            location,                            // location
                            stoichiometry,                       // stoichiometry
                            1                                   // output flag
                        )
                    );
                    ++index;
                }
            }
        }
    }

    return init_thermochemistry_variable;
}


/**
 * @brief Retrieves the list of scaling factors names.
 * @return A vector of strings containing the names of scaling factors.
 */
std::vector<std::string> getScalingFactorsNames()
{
    std::vector<std::string> names =
    {
        "Resolution rate",
        "Trapping rate",
        "Nucleation rate",
        "Diffusivity",
        "Diffusivity2",
        "Temperature",
        "Fission rate",
        "Cent parameter",
        "Helium production rate",
        "Dummy"
    };

    return names;
}

#endif