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

#include "Simulation.h"

void Simulation::GrainBoundarySweeping()
{
    if (!input_variable["iGrainBoundarySweeping"].getValue()) return;

    // Model declaration
    Model model_;

    model_.setName("Grain-boundary sweeping");

    switch (int(input_variable["iGrainBoundarySweeping"].getValue()))
    {
        case 0:
        {
            std::vector<double> parameter;
            parameter.push_back(0.0);
            model_.setParameter(parameter);
            model_.setRef(": Not considered");

            break;
        }

        case 1:
        {
            /**
             * @brief iGrainBoundarySweeping = 1 considers the fraction of grain swept volume (dV/V = 3 dr / r).
             * Then the fraction of intra-granular gas concentration swept is dC / C = - 3 dr / r
             *
             */

            std::vector<double> parameter;
            /// @param[out] grain_sweeped_volume
            parameter.push_back(3 * sciantix_variable["Grain radius"].getIncrement() / sciantix_variable["Grain radius"].getFinalValue());
            model_.setParameter(parameter);
            model_.setRef(": TRANSURANUS model");

            break;
        }

    default:
        ErrorMessages::Switch(__FILE__, "iGrainBoundarySweeping", int(input_variable["iGrainBoundarySweeping"].getValue()));
        break;
    }

    model.push(model_);

    // Model resolution
    // dC / df = - C
    switch (int(input_variable["iDiffusionSolver"].getValue()))
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
                        model["Grain-boundary sweeping"].getParameter().at(0)
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
                        model["Grain-boundary sweeping"].getParameter().at(0)
                    );

                modes_initial_conditions[8 * 40 + i] =
                    solver.Decay(
                        modes_initial_conditions[8 * 40 + i],
                        1.0,
                        0.0,
                        model["Grain-boundary sweeping"].getParameter().at(0)
                    );
            }
            break;
        }

        case 3:
            break;

        default:
            ErrorMessages::Switch(__FILE__, "iDiffusionSolver", int(input_variable["iDiffusionSolver"].getValue()));
            break;
    }
}
