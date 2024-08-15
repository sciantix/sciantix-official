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

#include "Simulation.h"

/**
 * @brief This routine defines the model the grain-boundary sweeping.
 * If activated, it describes the fraction of intra-granular gas concentration sweeped due to grain growth process.
 * 
 */

void Simulation::GrainBoundarySweeping()
{
    model.emplace_back();
    int model_index = int(model.size()) - 1;

    model[model_index].setName("Grain-boundary sweeping");

    switch (int(input_variable[iv["iGrainBoundarySweeping"]].getValue()))
    {
    case 0:
    {
        std::vector<double> parameter;
        parameter.push_back(0.0);
        model[model_index].setParameter(parameter);
        model[model_index].setRef(": Not considered");

        break;
    }

    case 1:
    {
        /**
         * @brief iGrainBoundarySweeping = 1 considers the fraction of grain swept volume (dV/V = 3 dr / r).
         * Then, the fraction of intra-granular gas concentration swept is dC / C = - 3 dr / r
         *  
         */

        std::vector<double> parameter;
        /// @param[out] grain_sweeped_volume
        parameter.push_back(3 * sciantix_variable[sv["Grain radius"]].getIncrement() / sciantix_variable[sv["Grain radius"]].getFinalValue());
        model[model_index].setParameter(parameter);
        model[model_index].setRef(": TRANSURANUS model");

        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "iGrainBoundarySweeping", int(input_variable[iv["iGrainBoundarySweeping"]].getValue()));
        break;
    }

    MapModel();

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
