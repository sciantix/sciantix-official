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
#include "UO2Thermochemistry.h"

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

    void GasDiffusion()
    {
        for (auto& system : sciantix_system)
        {
            switch (int(input_variable[iv["iDiffusionSolver"]].getValue()))
            {
                case 1:
                {
                    if (system.getRestructuredMatrix() == 0)
                    {
                        sciantix_variable[sv[system.getGasName() + " in grain"]].setFinalValue(
                            solver.SpectralDiffusion(
                                getDiffusionModes(system.getGasName()),
                                model[sm["Gas diffusion - " + system.getName()]].getParameter(),
                                physics_variable[pv["Time step"]].getFinalValue()
                            )
                        );

                        double equilibrium_fraction(1.0);
                        if ((system.getResolutionRate() + system.getTrappingRate()) > 0.0)
                            equilibrium_fraction = system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate());

                        sciantix_variable[sv[system.getGasName() + " in intragranular solution"]].setFinalValue(
                            equilibrium_fraction * sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
                        );

                        sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].setFinalValue(
                            (1.0 - equilibrium_fraction) * sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
                        );
                    }
                    else if (system.getRestructuredMatrix() == 1)
                    {
                        sciantix_variable[sv[system.getGasName() + " in grain HBS"]].setFinalValue(
                            solver.SpectralDiffusion(
                                getDiffusionModes(system.getGasName() + " in HBS"),
                                model[sm["Gas diffusion - " + system.getName()]].getParameter(),
                                physics_variable[pv["Time step"]].getFinalValue()
                            )
                        );
                    }
                    break;
                }

                case 2:
                {
                    double initial_value_solution(0.0), initial_value_bubbles(0.0);

                    if (system.getRestructuredMatrix() == 0)
                    {
                        initial_value_solution = sciantix_variable[sv[system.getGasName() + " in intragranular solution"]].getFinalValue();
                        initial_value_bubbles  = sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].getFinalValue();

                        solver.SpectralDiffusion2equations(
                            initial_value_solution,
                            initial_value_bubbles,
                            getDiffusionModesSolution(system.getGasName()),
                            getDiffusionModesBubbles(system.getGasName()),
                            model[sm["Gas diffusion - " + system.getName()]].getParameter(),
                            physics_variable[pv["Time step"]].getFinalValue()
                        );
                        sciantix_variable[sv[system.getGasName() + " in intragranular solution"]].setFinalValue(initial_value_solution);
                        sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].setFinalValue(initial_value_bubbles);
                        sciantix_variable[sv[system.getGasName() + " in grain"]].setFinalValue(initial_value_solution + initial_value_bubbles);
                    }
                    else if (system.getRestructuredMatrix() == 1)
                    {
                        sciantix_variable[sv[system.getGasName() + " in grain HBS"]].setFinalValue(0.0);
                    }					
                    break;
                }
    
                case 3:
                    break;

                default:
                    ErrorMessages::Switch("Simulation.h", "iDiffusionSolver", int(input_variable[iv["iDiffusionSolver"]].getValue()));
                    break;
            }
        }

        if (int(input_variable[iv["iDiffusionSolver"]].getValue()) == 3)
        {
            double initial_value_solution(0.0), initial_value_bubbles(0.0), initial_value_hbs(0.0);

            initial_value_solution = sciantix_variable[sv["Xe in intragranular solution"]].getFinalValue();
            initial_value_bubbles = sciantix_variable[sv["Xe in intragranular bubbles"]].getFinalValue();
            initial_value_hbs  = sciantix_variable[sv["Xe in grain HBS"]].getFinalValue();

            solver.SpectralDiffusion3equations(
                initial_value_solution,
                initial_value_bubbles,
                initial_value_hbs,
                getDiffusionModesSolution("Xe"),
                getDiffusionModesBubbles("Xe"),
                getDiffusionModes("Xe in HBS"),
                model[sm["Gas diffusion - Xe in UO2 with HBS"]].getParameter(),
                physics_variable[pv["Time step"]].getFinalValue()
            );
            sciantix_variable[sv["Xe in grain"]].setFinalValue(initial_value_solution + initial_value_bubbles);
            sciantix_variable[sv["Xe in intragranular solution"]].setFinalValue(initial_value_solution);
            sciantix_variable[sv["Xe in intragranular bubbles"]].setFinalValue(initial_value_bubbles);
            sciantix_variable[sv["Xe in grain HBS"]].setFinalValue(initial_value_hbs);

            sciantix_variable[sv["Intragranular gas solution swelling"]].setFinalValue(
                (sciantix_variable[sv["Xe in intragranular solution"]].getFinalValue() + sciantix_variable[sv["Xe in grain HBS"]].getFinalValue()) *
                pow(matrix[sma["UO2"]].getLatticeParameter(), 3) / 4
            );
        }

        // Calculation of the gas concentration arrived at the grain boundary, by mass balance.
        for (auto& system : sciantix_system)
        {
            if(system.getRestructuredMatrix() == 0)
            {
                sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(
                    sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                    sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                    sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
                    sciantix_variable[sv[system.getGasName() + " released"]].getInitialValue()
                );

                if (sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
                    sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);
            }
        }

        /**
         * @brief If **iGrainBoundaryBehaviour = 0** (e.g., grain-boundary calculations are neglected), 
         * all the gas arriving at the grain boundary is released.
         * 
         */
        if (input_variable[iv["iGrainBoundaryBehaviour"]].getValue() == 0)
        {
            for (auto& system : sciantix_system)
            {
                if(system.getRestructuredMatrix() == 0)
                {
                    {
                        sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setInitialValue(0.0);
                        sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);

                        sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
                            sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                            sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                            sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
                        );
                    }
                }
            }
        }
    }

    void GrainGrowth()
    {
        /**
         * @brief ### GrainGrowth
         * 
         */
        sciantix_variable[sv["Grain radius"]].setFinalValue(
            solver.QuarticEquation(model[sm["Grain growth"]].getParameter())
        );

        matrix[sma["UO2"]].setGrainRadius(sciantix_variable[sv["Grain radius"]].getFinalValue());
    }

    void IntraGranularBubbleBehaviour()
    {
        /**
         * @brief IntraGranularBubbleBehaviour is a method of the object Simulation.
         * This method computes concentration and average size of intragranular gas bubbles.
         *  
         */

        // dN / dt = - getParameter().at(0) * N + getParameter().at(1)
        sciantix_variable[sv["Intragranular bubble concentration"]].setFinalValue(
            solver.Decay(
                sciantix_variable[sv["Intragranular bubble concentration"]].getInitialValue(),
                model[sm["Intragranular bubble evolution"]].getParameter().at(0),
                model[sm["Intragranular bubble evolution"]].getParameter().at(1),
                physics_variable[pv["Time step"]].getFinalValue()
            )
        );

        // Atom per bubbles and bubble radius
        for (auto& system : sciantix_system)
        {
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            {
                if (sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue() > 0.0)
                    sciantix_variable[sv["Intragranular " + system.getGasName() + " atoms per bubble"]].setFinalValue(
                        sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].getFinalValue() /
                        sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue()
                    );

                else
                    sciantix_variable[sv["Intragranular " + system.getGasName() + " atoms per bubble"]].setFinalValue(0.0);

                sciantix_variable[sv["Intragranular bubble volume"]].addValue(
                    system.getVolumeInLattice() * sciantix_variable[sv["Intragranular " + system.getGasName() + " atoms per bubble"]].getFinalValue()
                );
            }
        }

        // Intragranular bubble radius
        sciantix_variable[sv["Intragranular bubble radius"]].setFinalValue(0.620350491 * pow(sciantix_variable[sv["Intragranular bubble volume"]].getFinalValue(), (1.0 / 3.0)));

        // Swelling
        // 4/3 pi N R^3
        sciantix_variable[sv["Intragranular gas bubble swelling"]].setFinalValue(4.188790205 *
            pow(sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue(), 3) *
            sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue()
        );

        if(sciantix_variable[sv["He in intragranular bubbles"]].getInitialValue() > 0.0)
            sciantix_variable[sv["Intragranular similarity ratio"]].setFinalValue(sqrt(sciantix_variable[sv["He in intragranular bubbles"]].getFinalValue() / sciantix_variable[sv["He in intragranular bubbles"]].getInitialValue()));
        else
            sciantix_variable[sv["Intragranular similarity ratio"]].setFinalValue(0.0);
    }

    void InterGranularBubbleBehaviour()
    {
        const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

        // Vacancy concentration
        sciantix_variable[sv["Intergranular vacancies per bubble"]].setFinalValue(
            solver.LimitedGrowth(sciantix_variable[sv["Intergranular vacancies per bubble"]].getInitialValue(),
                model[sm["Intergranular bubble evolution"]].getParameter(),
                physics_variable[pv["Time step"]].getFinalValue()
            )
        );

        // Grain-boundary bubble volume
        double vol(0);
        for (auto& system : sciantix_system)
        {
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            {
                vol += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue() *
                    gas[ga[system.getGasName()]].getVanDerWaalsVolume();
            }
        }
        vol += sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[sma["UO2"]].getSchottkyVolume();
        sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(vol);

        // Grain-boundary bubble radius
        sciantix_variable[sv["Intergranular bubble radius"]].setFinalValue(
            0.620350491 * pow(sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue() / (matrix[sma["UO2"]].getLenticularShapeFactor()), 1. / 3.));

        // Grain-boundary bubble area
        sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(
            pi * pow(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() * sin(matrix[sma["UO2"]].getSemidihedralAngle()), 2));

        // Grain-boundary bubble coalescence
        double dbubble_area = sciantix_variable[sv["Intergranular bubble area"]].getIncrement();
        sciantix_variable[sv["Intergranular bubble concentration"]].setFinalValue(
            solver.BinaryInteraction(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue(), 2.0, dbubble_area));

        // Conservation
        for (auto& system : sciantix_system)
        {
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            {
                sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].rescaleFinalValue(
                    sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() / sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue()
                );
            }
        }

        double n_at(0);
        for (auto& system : sciantix_system)
        {
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                n_at += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue();
        }
        sciantix_variable[sv["Intergranular atoms per bubble"]].setFinalValue(n_at);

        sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleFinalValue(
            sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() / sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue()
        );

        vol = 0.0;
        for (auto& system : sciantix_system)
        {
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                vol += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue() *
                    gas[ga[system.getGasName()]].getVanDerWaalsVolume();
        }
        vol += sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[sma["UO2"]].getSchottkyVolume();
        sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(vol);

        sciantix_variable[sv["Intergranular bubble radius"]].setFinalValue(
            0.620350491 * pow(sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue() / (matrix[sma["UO2"]].getLenticularShapeFactor()), 1. / 3.));

        sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(
            pi * pow(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() * sin(matrix[sma["UO2"]].getSemidihedralAngle()), 2));

        // Fractional coverage
        sciantix_variable[sv["Intergranular fractional coverage"]].setFinalValue(
            sciantix_variable[sv["Intergranular bubble area"]].getFinalValue() *
            sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue());

        // Intergranular gas release
        //                          F0
        //   ___________A0____________
        //   |_________A1__________  |
        //   |                    |  |
        //   |          F1        N1 N0
        //   |                    |  |
        //   |____________________|__|
        double similarity_ratio;
        
        if (sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue() > 0.0)
            similarity_ratio = sqrt(
                sciantix_variable[sv["Intergranular saturation fractional coverage"]].getFinalValue() /
                sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue()
            );
        else
            similarity_ratio = 1.0;

        if (similarity_ratio < 1.0)
        {
            sciantix_variable[sv["Intergranular bubble area"]].rescaleFinalValue(similarity_ratio);
            sciantix_variable[sv["Intergranular bubble concentration"]].rescaleFinalValue(similarity_ratio);
            sciantix_variable[sv["Intergranular fractional coverage"]].rescaleFinalValue(pow(similarity_ratio, 2));
            sciantix_variable[sv["Intergranular bubble volume"]].rescaleFinalValue(pow(similarity_ratio, 1.5));
            sciantix_variable[sv["Intergranular bubble radius"]].rescaleFinalValue(pow(similarity_ratio, 0.5));
            sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleFinalValue(pow(similarity_ratio, 1.5));

            // New intergranular gas concentration
            for (auto& system : sciantix_system)
            {
                if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                    sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].rescaleFinalValue(pow(similarity_ratio, 1.5));
            }

            n_at = 0.0;
            for (auto& system : sciantix_system)
            {
                if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                    n_at += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue();
            }
            sciantix_variable[sv["Intergranular atoms per bubble"]].setFinalValue(n_at);

            for (auto& system : sciantix_system)
            {
                if (system.getRestructuredMatrix() == 0)
                    sciantix_variable[sv[system.getGasName() + " at grain boundary"]].rescaleFinalValue(pow(similarity_ratio, 2.5));
            }
        }

        // Calculation of the gas concentration arrived at the grain boundary, by mass balance.
        for (auto& system : sciantix_system)
        {
            if(system.getRestructuredMatrix() == 0)
            {
                sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
                    sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                    sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                    sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
                    sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue()
                );

                if (sciantix_variable[sv[system.getGasName() + " released"]].getFinalValue() < 0.0)
                    sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(0.0);
            }
        }

        // Intergranular gaseous swelling
        sciantix_variable[sv["Intergranular gas swelling"]].setFinalValue(
            3 / sciantix_variable[sv["Grain radius"]].getFinalValue() *
            sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue() *
            sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue()
        );
    }

    void GrainBoundarySweeping()
    {
        // Sweeping of the intra-granular gas concentrations
        // dC / df = - C

        if (!input_variable[iv["Grain-boundary sweeping"]].getValue()) return;

        // intra-granular gas diffusion modes
        switch (int(input_variable[iv["iDiffusionSolver"]].getValue()))
        {
            case 1:
            {
                for (int i = 0; i < n_modes; ++i)
                {
                    modes_initial_conditions[6 * 40 + i] =
                        solver.Decay(
                            modes_initial_conditions[6 * 40 + i],
                            1.0,
                            0.0,
                            model[sm["Grain-boundary sweeping"]].getParameter().at(0)
                        );
                }
                
                break;
            }

            case 2:
            {
                for (int i = 0; i < n_modes; ++i)
                {
                    modes_initial_conditions[7 * 40 + i] =
                        solver.Decay(
                            modes_initial_conditions[7 * 40 + i],
                            1.0,
                            0.0,
                            model[sm["Grain-boundary sweeping"]].getParameter().at(0)
                        );

                    modes_initial_conditions[8 * 40 + i] =
                        solver.Decay(
                            modes_initial_conditions[8 * 40 + i],
                            1.0,
                            0.0,
                            model[sm["Grain-boundary sweeping"]].getParameter().at(0)
                        );
                }

                break;
            }

            case 3:
                break;

            default:
                // ErrorMessages::Switch("Simulation.h", "iDiffusionSolver", int(input_variable[iv["iDiffusionSolver"]].getValue()));
                break;
        }
    }

    void GrainBoundaryMicroCracking();

    void GrainBoundaryVenting();

    void HighBurnupStructureFormation();

    void HighBurnupStructurePorosity();

    void StoichiometryDeviation()
    {
        if (!input_variable[iv["iStoichiometryDeviation"]].getValue()) return;

        if(history_variable[hv["Temperature"]].getFinalValue() < 1000.0)
        {
            sciantix_variable[sv["Stoichiometry deviation"]].setConstant();
            sciantix_variable[sv["Fuel oxygen partial pressure"]].setFinalValue(0.0);
        }

        else if(input_variable[iv["iStoichiometryDeviation"]].getValue() < 5)
        {	
            sciantix_variable[sv["Stoichiometry deviation"]].setFinalValue(
            solver.Decay(
                sciantix_variable[sv["Stoichiometry deviation"]].getInitialValue(),
                    model[sm["Stoichiometry deviation"]].getParameter().at(0),
                    model[sm["Stoichiometry deviation"]].getParameter().at(1),
                    physics_variable[pv["Time step"]].getFinalValue()
                )
            );
        }

        else if(input_variable[iv["iStoichiometryDeviation"]].getValue() > 4)
        {
            sciantix_variable[sv["Stoichiometry deviation"]].setFinalValue(
                solver.NewtonLangmuirBasedModel(
                    sciantix_variable[sv["Stoichiometry deviation"]].getInitialValue(),
                        model[sm["Stoichiometry deviation"]].getParameter(),
                        physics_variable[pv["Time step"]].getFinalValue()
                )
            );
        }

        sciantix_variable[sv["Fuel oxygen partial pressure"]].setFinalValue(
        BlackburnThermochemicalModel(
            sciantix_variable[sv["Stoichiometry deviation"]].getFinalValue(),
            history_variable[hv["Temperature"]].getFinalValue()
            )
        );
    }

    void UO2Thermochemistry()
    {
    if (!input_variable[iv["iStoichiometryDeviation"]].getValue()) return;

    if(history_variable[hv["Temperature"]].getFinalValue() < 1000.0 || sciantix_variable[sv["Gap oxygen partial pressure"]].getFinalValue() == 0)
        sciantix_variable[sv["Equilibrium stoichiometry deviation"]].setFinalValue(0.0);

    else
        sciantix_variable[sv["Equilibrium stoichiometry deviation"]].setFinalValue(
            solver.NewtonBlackburn(
                model[sm["UO2 thermochemistry"]].getParameter()
                    )
        );
    }

    /**
     * @brief This method returns a pointer to the array of diffusion modes corresponding to the specified gas.
     * 
     * @param gas_name The name of the gas for which diffusion modes are required.
     * @return A pointer to the array of diffusion modes for the specified gas.
     *         Returns nullptr if the gas name is invalid.
     */
    double* getDiffusionModes(std::string gas_name)
    {
        if(gas_name == "Xe")
            return &modes_initial_conditions[0];
        else if(gas_name == "Kr")
            return &modes_initial_conditions[3 * 40];
        else if(gas_name == "He")
            return &modes_initial_conditions[6 * 40];
        else if(gas_name == "Xe133")
            return &modes_initial_conditions[9 * 40];

        else if (gas_name == "Kr85m")
            return &modes_initial_conditions[12 * 40];
            
        else if (gas_name == "Xe in HBS")
            return &modes_initial_conditions[15 * 40];

        else
        {
            std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModes." << std::endl;
            return nullptr;
        }
    }

    double* getDiffusionModesSolution(std::string gas_name)
    {	
        if(gas_name == "Xe")
            return &modes_initial_conditions[1 * 40];

        else if(gas_name == "Kr")
            return &modes_initial_conditions[4 * 40];

        else if(gas_name == "He")
            return &modes_initial_conditions[7 * 40];

        else if(gas_name == "Xe133")
            return &modes_initial_conditions[10 * 40];

        else if (gas_name == "Kr85m")
            return &modes_initial_conditions[13 * 40];
            
        else if (gas_name == "Xe in HBS")
            return &modes_initial_conditions[16 * 40];
        else
        {
            std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModesSolution." << std::endl;
            return nullptr;
        }
    }

    double* getDiffusionModesBubbles(std::string gas_name)
    {	
        if(gas_name == "Xe")
            return &modes_initial_conditions[2 * 40];

        else if(gas_name == "Kr")
            return &modes_initial_conditions[5 * 40];

        else if(gas_name == "He")
            return &modes_initial_conditions[8 * 40];

        else if(gas_name == "Xe133")
            return &modes_initial_conditions[11 * 40];

        else if (gas_name == "Kr85m")
            return &modes_initial_conditions[14 * 40];

        else
        {
            std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModesBubbles." << std::endl;
            return nullptr;
        }
    }

    Simulation() {}
    ~Simulation() {}
};

#endif
