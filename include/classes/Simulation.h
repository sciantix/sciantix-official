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
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SIMULATION_H
#define SIMULATION_H

#include <cmath>
#include <vector>
#include "Solver.h"
#include "Model.h"
#include "MapModel.h"
#include "MapSciantixVariable.h"
#include "MapPhysicsVariable.h"
#include "HistoryVariableDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "ModelDeclaration.h"
#include "SolverDeclaration.h"
#include "PhysicsVariableDeclaration.h"
#include "GasDeclaration.h"
#include "MapGas.h"
#include "SciantixDiffusionModeDeclaration.h"
#include "SystemDeclaration.h"
#include "MapSystem.h"
#include "MatrixDeclaration.h"
#include "MapMatrix.h"
#include "ConstantNumbers.h"

/**
 * @class Simulation
 * @brief Derived class that orchestrates the overall simulation process combining various models and solvers.
 *
 * The Simulation class inherits from Solver and Model classes to utilize their functionalities
 * for simulating complex interactions and processes based on the provided models.
 * 
 * @author G. Zullo
 * 
 */

class Simulation : public Solver, public Model
{
    public:

    /**
     * @brief Burnup() calculates the local burnup from fission rate and fuel density.
     *
     * \details
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
     * \details
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
     * @author G. Zullo
     * 
     */
    void GrainGrowth();
    
    /**
     * @brief Evolution models for intra-granular bubbles in fuel grains based on the value of iIntraGranularBubbleEvolution.
     * 
     * - **iIntraGranularBubbleEvolution == 1**:  
     *   The evolution of small intra-granular bubbles in fuel grains is controlled by bubble nucleation, gas atom trapping, 
     *   and irradiation-induced gas atom re-solution back into the lattice.  
     *   Description of the model in @ref Pizzocri et al., JNM, 502 (2018) 323-330.
     *   @param[out] intragranular_bubble_concentration The concentration of intra-granular bubbles.
     *   @param[out] intragranular_bubble_radius The radius of intra-granular bubbles.
     * 
     * - **iIntraGranularBubbleEvolution == 2**:  
     *   The evolution of intra-granular bubbles is modeled by temperature-driven correlations.  
     *   Description of the model in @ref White, Tucker, Journal of Nuclear Materials, 118 (1983), 1-38.
     *   @param[in] local_fuel_temperature The local temperature of the fuel affecting bubble evolution.
     * 
     * - **iIntraGranularBubbleEvolution == 3**:  
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
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void InterGranularBubbleBehavior();

    /**
     * @brief Calculates the sweeping of grain boundaries, particularly how it affects gas concentrations.
     * Sweeping of the intra-granular gas concentrations.
     * 
     * @author D. Pizzocri
     * @author T. Barani
     * @author G. Zullo
     * 
     */
    void GrainBoundarySweeping();

    /**
     * @brief GrainBoundaryMicroCracking is method of simulation which executes
     * the SCIANTIX simulation for the grain-boundary micro-cracking induced by a temperature difference.
     * This method calls the related model "Grain-boundary micro-cracking", takes the model parameters and solve the model ODEs.
     */
    void GrainBoundaryMicroCracking();

    /**
     * @brief Handles the venting processes at grain boundaries, potentially releasing gases.
     */
    void GrainBoundaryVenting();

    /**
     * @brief Simulates the formation of high burnup structures within the nuclear fuel.
     * 
     * @author A. Magni
     * @author E. Redaelli
     * @author G. Zullo
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
     * \details
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

    Simulation() {}
    ~Simulation() {}
};

#endif
