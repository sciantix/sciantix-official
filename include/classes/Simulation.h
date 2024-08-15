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

    void Burnup();

    void EffectiveBurnup();

    void GasProduction();

    void GasDecay();

    void GasDiffusion();

    void GrainGrowth();

    void IntraGranularBubbleBehavior();

    void InterGranularBubbleBehavior();

    void GrainBoundarySweeping();

    void GrainBoundaryMicroCracking();

    void GrainBoundaryVenting();

    void HighBurnupStructureFormation();

    void HighBurnupStructurePorosity();

    void EnvironmentComposition();

    void StoichiometryDeviation();

    void UO2Thermochemistry();

    Simulation() {}
    ~Simulation() {}
};

#endif
