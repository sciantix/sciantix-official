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

void Simulation::IntraGranularBubbleBehavior()
{
    // Model declaration
    Model model_;

    model_.setName("Intragranular bubble behavior");

    std::string reference;
    std::vector<double> parameter;

    switch (int(input_variable["iIntraGranularBubbleBehavior"].getValue()))
    {
        case 0:
        {
            reference += ": Constant bubble concentration and radius.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(7.0e23);
            sciantix_variable["Intragranular bubble radius"].setInitialValue(1.0e-9);

            sciantix_variable["Intragranular bubble concentration"].setFinalValue(7.0e23);
            sciantix_variable["Intragranular bubble radius"].setFinalValue(1.0e-9);

            parameter.push_back(0.);
            parameter.push_back(0.);

            break;
        }

        case 1:
        {
            reference += ": Pizzocri et al., JNM, 502 (2018) 323-330.";

            parameter.push_back(sciantix_system[0].getResolutionRate());
            parameter.push_back(sciantix_system[0].getNucleationRate());

            break;
        }

        case 2:
        {
            reference += "White and Tucker, JNM, 118 (1983), 1-38.";
            
            sciantix_variable["Intragranular bubble concentration"].setInitialValue(1.52e+27 / history_variable["Temperature"].getFinalValue() - 3.3e+23);
            parameter.push_back(0.0);
            parameter.push_back(0.0);

            break;
        }

        case 3:
        {
            reference += "Case specific for annealing experiments and helium intragranular behaviour.";

            if(physics_variable["Time step"].getFinalValue() > 0.0)
                parameter.push_back((1.0 / sciantix_variable["Intragranular similarity ratio"].getFinalValue() - 1.0) / physics_variable["Time step"].getFinalValue());
            else
                parameter.push_back(0.);

            parameter.push_back(0.);

            break;
        }

        case 99:
        {
            reference += "No intragranular bubbles.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(0.0);
            sciantix_variable["Intragranular bubble radius"].setInitialValue(0.0);
            sciantix_variable["Intragranular atoms per bubble"].setInitialValue(0.0);

            sciantix_variable["Intragranular bubble concentration"].setFinalValue(0.0);
            sciantix_variable["Intragranular bubble radius"].setFinalValue(0.0);
            sciantix_variable["Intragranular atoms per bubble"].setFinalValue(0.0);

            parameter.push_back(0.);
            parameter.push_back(0.);

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iIntraGranularBubbleBehavior", int(input_variable["iIntraGranularBubbleBehavior"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    // dN / dt = - getParameter().at(0) * N + getParameter().at(1)
    sciantix_variable["Intragranular bubble concentration"].setFinalValue(
        solver.Decay(
            sciantix_variable["Intragranular bubble concentration"].getInitialValue(),
            model["Intragranular bubble behavior"].getParameter().at(0),
            model["Intragranular bubble behavior"].getParameter().at(1),
            physics_variable["Time step"].getFinalValue()
        )
    );

    // Atom per bubbles and bubble radius
    for (auto &system : sciantix_system)
    {
        if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            if (sciantix_variable["Intragranular bubble concentration"].getFinalValue() > 0.0)
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].setFinalValue(
                    sciantix_variable[system.getGasName() + " in intragranular bubbles"].getFinalValue() /
                    sciantix_variable["Intragranular bubble concentration"].getFinalValue()
                );

            else
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].setFinalValue(0.0);

            sciantix_variable["Intragranular bubble volume"].addValue(
                system.getVolumeInLattice() * sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].getFinalValue());
        }
    }

    // Intragranular bubble radius
    sciantix_variable["Intragranular bubble radius"].setFinalValue(0.620350491 * pow(sciantix_variable["Intragranular bubble volume"].getFinalValue(), (1.0 / 3.0)));

    // Intragranular gaseous swelling
    // 4/3 pi N R^3
    sciantix_variable["Intragranular gas bubble swelling"].setFinalValue(
        4.188790205 * pow(sciantix_variable["Intragranular bubble radius"].getFinalValue(), 3) * sciantix_variable["Intragranular bubble concentration"].getFinalValue()
    );

    if (sciantix_variable["He in intragranular bubbles"].getInitialValue() > 0.0)
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(sqrt(sciantix_variable["He in intragranular bubbles"].getFinalValue() / sciantix_variable["He in intragranular bubbles"].getInitialValue()));
    else
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(0.0);
}