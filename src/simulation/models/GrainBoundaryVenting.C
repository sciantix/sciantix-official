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

void Simulation::GrainBoundaryVenting()
{
    Model grain_bound_vent_model;
    grain_bound_vent_model.setName("Grain-boundary venting");

    std::vector<double> parameter;
    std::string reference;

    switch (int(input_variable["iGrainBoundaryVenting"].getValue()))
    {
    case 0:
    {
        /**
         * \brief Not considered.
         *
         */

        sciantix_variable["Intergranular venting probability"].setFinalValue(0.0);
        reference = "not considered.";

        break;
    }

    case 1:
    {
        /**
         * \brief Release mechanisms quantified via the vented fraction
         *
         */

        // Shape of the sigmoid function
        const double screw_parameter = 0.1;
        const double span_parameter = 10.0;
        const double cent_parameter = 0.43;

        double sigmoid_variable;
        sigmoid_variable = sciantix_variable["Intergranular fractional coverage"].getInitialValue() *
                           exp(-sciantix_variable["Intergranular fractional intactness"].getIncrement());

        // Vented fraction
        sciantix_variable["Intergranular vented fraction"].setFinalValue(
            1.0 / pow((1.0 + screw_parameter * exp(-span_parameter * (sigmoid_variable - cent_parameter))), (1.0 / screw_parameter)));

        // Venting probability
        sciantix_variable["Intergranular venting probability"].setFinalValue(
            (1.0 - sciantix_variable["Intergranular fractional intactness"].getFinalValue()) + sciantix_variable["Intergranular fractional intactness"].getFinalValue() * 
            sciantix_variable["Intergranular vented fraction"].getFinalValue());

        reference = "Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE";

        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "iGrainBoundaryVenting", int(input_variable["iGrainBoundaryVenting"].getValue()));
        break;
    }

    parameter.push_back(sciantix_variable["Intergranular venting probability"].getFinalValue());

    grain_bound_vent_model.setParameter(parameter);
    grain_bound_vent_model.setRef(reference);

    model.push(grain_bound_vent_model);




    if (!int(input_variable["iGrainBoundaryVenting"].getValue()))
        return;

    for (auto &system : sciantix_system)
    {
        sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(
            solver.Integrator(
                sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue(),
                -model["Grain-boundary venting"].getParameter().at(0),
                sciantix_variable[system.getGasName() + " at grain boundary"].getIncrement()));
        sciantix_variable[system.getGasName() + " at grain boundary"].resetValue();
    }
}