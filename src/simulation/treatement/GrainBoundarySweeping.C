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

void Simulation::GrainBoundarySweeping()
{
	Model grain_bound_sweeping;

	grain_bound_sweeping.setName("Grain-boundary sweeping");

	switch (int(input_variable["iGrainBoundarySweeping"].getValue()))
	{
	case 0:
	{
		std::vector<double> parameter;
		parameter.push_back(0.0);
		grain_bound_sweeping.setParameter(parameter);
		grain_bound_sweeping.setRef(": Not considered");

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
		parameter.push_back(3 * sciantix_variable["Grain radius"].getIncrement() / sciantix_variable["Grain radius"].getFinalValue());
		grain_bound_sweeping.setParameter(parameter);
		grain_bound_sweeping.setRef(": TRANSURANUS model");

		break;
	}

	default:
		ErrorMessages::Switch(__FILE__, "iGrainBoundarySweeping", int(input_variable["iGrainBoundarySweeping"].getValue()));
		break;
	}

	model.push(grain_bound_sweeping);





    // dC / df = - C

    if (!input_variable["Grain-boundary sweeping"].getValue())
        return;

    // intra-granular gas diffusion modes
    switch (int(input_variable["iDiffusionSolver"].getValue()))
    {
    case 1:
    {
        for (int i = 0; i < modes_initial_conditions.size(); ++i)
        {
            modes_initial_conditions[6 * 40 + i] =
                solver.Decay(
                    modes_initial_conditions[6 * 40 + i],
                    1.0,
                    0.0,
                    model["Grain-boundary sweeping"].getParameter().at(0));
        }

        break;
    }

    case 2:
    {
        for (int i = 0; i < modes_initial_conditions.size(); ++i)
        {
            modes_initial_conditions[7 * 40 + i] =
                solver.Decay(
                    modes_initial_conditions[7 * 40 + i],
                    1.0,
                    0.0,
                    model["Grain-boundary sweeping"].getParameter().at(0));

            modes_initial_conditions[8 * 40 + i] =
                solver.Decay(
                    modes_initial_conditions[8 * 40 + i],
                    1.0,
                    0.0,
                    model["Grain-boundary sweeping"].getParameter().at(0));
        }

        break;
    }

    case 3:
        break;

    default:
        // ErrorMessages::Switch("Simulation.h", "iDiffusionSolver", int(input_variable["iDiffusionSolver"].getValue()));
        break;
    }
}
