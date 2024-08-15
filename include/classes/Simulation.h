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
     */
    void EffectiveBurnup();

    /**
     * @brief GasProduction computes the gas produced from the production rate.
     *
     */
    void GasProduction();

    /**
     * @brief Calculates the decayed amount of radioactive gases.
     */
    void GasDecay();

    /**
     * @brief Handles the diffusion of gas within the system, considering different states like in-grain or within bubbles.
     */
    void GasDiffusion();

    /**
     * @brief Grain growth based on specific model parameters affecting the system's materials.
     */
    void GrainGrowth();
    
    /**
     * @brief Calculates the behavior of intragranular bubbles, including their formation and changes over time.
     * This method computes concentration and average size of intragranular gas bubbles.
     */
    void IntraGranularBubbleBehavior();

    /**
     * @brief Simulates intergranular bubble dynamics, including growth and interaction with vacancies.
     */
    void InterGranularBubbleBehavior();

    /**
     * @brief Calculates the sweeping of grain boundaries, particularly how it affects gas concentrations.
     * Sweeping of the intra-granular gas concentrations.
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
     */
    void HighBurnupStructureFormation();

    /**
     * @brief Models the porosity changes within high burnup structures, influencing material properties.
     */
    void HighBurnupStructurePorosity();

    /**
     * @brief Evaluates the deviation in stoichiometry within the nuclear material and its effects.
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
     */
    void GapPartialPressure();

    /**
     * @brief Conducts detailed thermochemical calculations for UO2 under specific conditions.
     */
    void UO2Thermochemistry();

    Simulation() {}
    ~Simulation() {}
};

#endif
