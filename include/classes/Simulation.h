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

#ifndef SIMULATION_H
#define SIMULATION_H

#include <cmath>
#include <vector>
#include "Solver.h"
#include "Model.h"
#include "Matrix.h"
#include "System.h"
#include "SciantixArray.h"
#include "SciantixVariable.h"

/**
 * @class Simulation
 * @brief Derived class that orchestrates the overall simulation process combining various models and solvers.
 *
 * The Simulation class inherits from Solver and Model classes to utilize their functionalities
 * for simulating complex interactions and processes based on the provided models.
 * 
 * @author G. Zullo
 * @author F. Bastien
 * 
 */
class Simulation
{
private:
    SciantixArray<SciantixVariable> sciantix_variable;
    SciantixArray<SciantixVariable> history_variable;
    SciantixArray<SciantixVariable> physics_variable;
    
    SciantixArray<Model> model;
    SciantixArray<System> sciantix_system;
    SciantixArray<Matrix> matrices;
    SciantixArray<Gas> gas;

    SciantixArray<InputVariable> input_variable;
    SciantixArray<InputVariable> scaling_factors;

    int n_modes;
    std::vector<double> modes_initial_conditions;

    Solver solver;

    static Simulation* instance;

    /**
     * @brief Default constructor for the Simulation class
     */
    Simulation() {
        n_modes = 40;
        modes_initial_conditions.resize(1120);
    }

public:

    /**
     * @brief Destructor for the Simulation class
     */
    ~Simulation() {}

    static Simulation* getInstance();

    void setVariables(
        int Sciantix_options[], 
        double Sciantix_history[], 
        double Sciantix_variables[], 
        double Sciantix_scaling_factors[], 
        double Sciantix_diffusion_modes[]
    );

    void setGas();
    void setMatrix();
    void setSystem();

    void initialize(
        int Sciantix_options[], 
        double Sciantix_history[], 
        double Sciantix_variables[], 
        double Sciantix_scaling_factors[], 
        double Sciantix_diffusion_modes[]
    );

    void execute();

    void update(double Sciantix_variables[], double Sciantix_diffusion_modes[]);

    void output();

    /**
     * @brief Burnup() calculates the local burnup from fission rate and fuel density.
     *
     * @details
     * This function calculates the local burnup in MWd/kgUO2 from fission rate and fuel density.
     * In addition, the function calculates also the irradiation time and the burnup in FIMA.
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void Burnup();

    /**
     * @brief EffectiveBurnup () calculates the local effective burnup of the fuel.
     *
     * @details
     * The local effective burnup is calculated if the local temperature is lower than a temperature threshold, 
     * 
     * @ref G. Khvostov et al., WRFPM-2005, Kyoto, Japan, 2005.
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void EffectiveBurnup();

    /**
     * @brief GasProduction computes the gas produced from the production rate.
     *
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void GasProduction();

    /**
     * @brief Calculates the decayed amount of radioactive gases.
     * 
     * @author G. Zullo
     * 
     */
    void GasDecay();

    /**
     * @brief Handles the intragranular gas diffusion problem.
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void GasDiffusion();

    /**
     * @brief Grain growth based on specific model parameters affecting the system's materials.
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void GrainGrowth();
    
    /**
     * @brief Evolution models for intra-granular bubbles in fuel grains based on the value of iIntraGranularBubbleBehavior.
     * 
     * - **iIntraGranularBubbleBehavior == 1**:  
     *   The evolution of small intra-granular bubbles in fuel grains is controlled by bubble nucleation, gas atom trapping, 
     *   and irradiation-induced gas atom re-solution back into the lattice.  
     *   Description of the model in @ref Pizzocri et al., JNM, 502 (2018) 323-330.
     *   @param[out] intragranular_bubble_concentration The concentration of intra-granular bubbles.
     *   @param[out] intragranular_bubble_radius The radius of intra-granular bubbles.
     * 
     * - **iIntraGranularBubbleBehavior == 2**:  
     *   The evolution of intra-granular bubbles is modeled by temperature-driven correlations.  
     *   Description of the model in @ref White, Tucker, Journal of Nuclear Materials, 118 (1983), 1-38.
     *   @param[in] local_fuel_temperature The local temperature of the fuel affecting bubble evolution.
     * 
     * - **iIntraGranularBubbleBehavior == 3**:  
     *   The evolution of intra-granular bubble concentration, radius, and atoms per bubble is described through 
     *   the similarity ratio, based on the evolution of intra-granular gas concentration in bubbles.
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void IntraGranularBubbleBehavior();

    /**
     * @brief The model considers one-off nucleation, growth of lenticular bubbles by vacancy absorption and coalescence of bubbles.
     *  
     * @ref White, JNM, 325 (2004) 61-77
     * @ref Pastore, NED, (2013)
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void InterGranularBubbleBehavior();

    /**
     * @brief Calculates the sweeping of grain boundaries, particularly how it affects intragranular gas concentrations.
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void GrainBoundarySweeping();

    /**
     * @brief Calculates the fraction of cracked/healed grain-boundary faces after temperature transients,
     * and the corresponding fission gas released.
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void GrainBoundaryMicroCracking();

    /**
     * @brief Calculates the amount of gas released due to venting processes.
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void GrainBoundaryVenting();

    /**
     * @brief Calculates the formation of high burnup structures within the nuclear fuel.
     * 
     * @author G. Zullo
     * @author A. Magni
     * @author E. Redaelli
     * 
     */
    void HighBurnupStructureFormation();

    /**
     * @brief Models the porosity changes within high burnup structures, influencing material properties.
     * 
     * @author A. Magni
     * @author E. Redaelli
     * @author G. Zullo
     * 
     */
    void HighBurnupStructurePorosity();

    /**
     * @brief Evaluates the deviation in stoichiometry within the nuclear material and its effects.
     * 
     * @author G. Petrosillo
     * @author G. Zullo
     * 
     */
    void StoichiometryDeviation();


    /**
     * @brief GapPartialPressure() Calculates the oxygen partial pressure in the gap based on a thermo-chemical equilibrium model.
     * 
     * @author G. Petrosillo
     * @author G. Zullo
     *
     * @details
     * The equilibrium constant is calculated using the law of mass action for water vapor decomposition.
     * The gap oxygen partial pressure is then calculated using this equilibrium constant and the steam pressure.
     * The final value is set in the `Gap oxygen partial pressure` variable.
     * 
     * @ref Morel et al., CEA, Report NT/DTP/SECC no. DR94-55 (1994)
     * @ref Lewis et al. JNM 227 (1995) 83-109, D.R. Olander, Nucl. Technol. 74 (1986) 215.
     * 
     */
    void GapPartialPressure();

    /**
     * @brief Conducts detailed thermochemical calculations for UO2 under specific conditions.
     * 
     * @author G. Petrosillo
     * @author G. Zullo
     * 
     */
    void UO2Thermochemistry();

	/**
	 * @brief This function defines the Sciantix model *ChromiumSolubility*.
	 * 
     * @details
	 * The model ChromiumSolubility is used to evaluate the chromium solubility accordingly to the temperature and the oxygen content.
	 * Then the number of oxide chromium atoms is evaluated, accordingly to the oxygen content of the system. 
	 * 
	 * Solubility of Chromium is evaluated as log10(y_Cr) = p*log10(P_O2) + V + U/T
	 * Coefficients U and V came from experimental fitting, in this way the Gibbs Potential is approximated as a function of 1/T
	 * Metallic chromium shows two different phases with threshold temperature of 1651 °C
	 * CrO is expected to be negligible, accordingly to Cr - O phase diagram
	 * The exchange between Cr-metal phase and Cr-oxide phase is described accordingly to the empirical expression:
	 * oxide_fraction  = 1 - exp(C1 * T - C1*C2)
	 * 
     * @author G. Nicodemo
     * 
     * @ref <a href="https://www.sciencedirect.com/science/article/pii/S0022311524004033" target="_blank">Nicodemo G. et al (2024). Journal of Nuclear Materials, 601, 155301.</a>
	 */

    void ChromiumSolubility();

    /**
	 * @brief The model Microstructure is used to evaluate the lattice parameter and the theoretical density, accordingly to chromium content.
	 * 
     * @author G. Nicodemo
     * 
     * @ref <a href="https://www.sciencedirect.com/science/article/pii/S0022311512000943" target="_blank">T. Cardinaels et al (2012), Journal of Nuclear Materials, 424 252-260.</a>
	 */

    void Microstructure();

    /**
     * @brief This method returns a pointer to the array of diffusion modes corresponding to the specified gas.
     * @param gas_name The name of the gas for which diffusion modes are required.
     * @return A pointer to the array of diffusion modes for the specified gas, or nullptr for invalid gas names.
     */
    double *getDiffusionModes(std::string gas_name)
    {
        if (gas_name == "Xe")
            return &modes_initial_conditions[0];
        else if (gas_name == "Kr")
            return &modes_initial_conditions[3 * 40];
        else if (gas_name == "He")
            return &modes_initial_conditions[6 * 40];
        else if (gas_name == "Xe133")
            return &modes_initial_conditions[9 * 40];

        else if (gas_name == "Kr85m")
            return &modes_initial_conditions[12 * 40];

        else if (gas_name == "Xe in HBS")
            return &modes_initial_conditions[15 * 40];

        else if (gas_name == "Cs")
            return &modes_initial_conditions[18 * 40];
            
        else if (gas_name == "I")
            return &modes_initial_conditions[21 * 40];
            
        else if (gas_name == "CsI")
            return &modes_initial_conditions[24 * 40];

        else
        {
            std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModes." << std::endl;
            return nullptr;
        }
    }

    /**
     * @brief Retrieves diffusion modes related to solutions for a specified gas.
     * @param gas_name Name of the gas.
     * @return Pointer to the array of diffusion modes for solutions, or nullptr for invalid gas names.
     */
    double *getDiffusionModesSolution(std::string gas_name)
    {
        if (gas_name == "Xe")
            return &modes_initial_conditions[1 * 40];

        else if (gas_name == "Kr")
            return &modes_initial_conditions[4 * 40];

        else if (gas_name == "He")
            return &modes_initial_conditions[7 * 40];

        else if (gas_name == "Xe133")
            return &modes_initial_conditions[10 * 40];

        else if (gas_name == "Kr85m")
            return &modes_initial_conditions[13 * 40];

        else if (gas_name == "Xe in HBS")
            return &modes_initial_conditions[16 * 40];

        else if (gas_name == "Cs")
            return &modes_initial_conditions[19 * 40];
            
        else if (gas_name == "I")
            return &modes_initial_conditions[22 * 40];
            
        else if (gas_name == "CsI")
            return &modes_initial_conditions[25 * 40];
        else
        {
            std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModesSolution." << std::endl;
            return nullptr;
        }
    }

    /**
     * @brief Retrieves diffusion modes related to bubbles for a specified gas.
     * @param gas_name Name of the gas.
     * @return Pointer to the array of diffusion modes for bubbles, or nullptr for invalid gas names.
     */
    double *getDiffusionModesBubbles(std::string gas_name)
    {
        if (gas_name == "Xe")
            return &modes_initial_conditions[2 * 40];

        else if (gas_name == "Kr")
            return &modes_initial_conditions[5 * 40];

        else if (gas_name == "He")
            return &modes_initial_conditions[8 * 40];

        else if (gas_name == "Xe133")
            return &modes_initial_conditions[11 * 40];

        else if (gas_name == "Kr85m")
            return &modes_initial_conditions[14 * 40];
        
        else if (gas_name == "Xe in HBS")
            return &modes_initial_conditions[17 * 40];

        else if (gas_name == "Cs")
            return &modes_initial_conditions[20 * 40];
            
        else if (gas_name == "I")
            return &modes_initial_conditions[23 * 40];
            
        else if (gas_name == "CsI")
            return &modes_initial_conditions[26 * 40];
        else
        {
            std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModesBubbles." << std::endl;
            return nullptr;
        }
    }
};

#endif
