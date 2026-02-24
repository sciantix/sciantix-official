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
//  Version: 2.2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

void Simulation::GrainBoundaryVenting()
{
    if (!int(input_variable["iGrainBoundaryVenting"].getValue()))
        return;

    // Model declaration
    Model model_;
    model_.setName("Grain-boundary venting");

    std::vector<double> parameter;
    std::string         reference;

    switch (int(input_variable["iGrainBoundaryVenting"].getValue()))
    {
        case 0:
        {
            sciantix_variable["Intergranular venting probability"].setFinalValue(0.0);
            reference = "not considered.";

            break;
        }

        case 1:
        {
            // Shape of the sigmoid function
            const double screw_parameter = 0.1;
            const double span_parameter  = 10.0;
            const double cent_parameter  = 0.43;

            double sigmoid_variable;
            sigmoid_variable = sciantix_variable["Intergranular fractional coverage"].getInitialValue() *
                               exp(-sciantix_variable["Intergranular fractional intactness"].getIncrement());

            // Vented fraction
            sciantix_variable["Intergranular vented fraction"].setFinalValue(
                1.0 / pow((1.0 + screw_parameter * exp(-span_parameter * (sigmoid_variable - cent_parameter))),
                          (1.0 / screw_parameter)));

            // Venting probability
            sciantix_variable["Intergranular venting probability"].setFinalValue(
                (1.0 - sciantix_variable["Intergranular fractional intactness"].getFinalValue()) +
                sciantix_variable["Intergranular fractional intactness"].getFinalValue() *
                    sciantix_variable["Intergranular vented fraction"].getFinalValue());

            reference = ": Pizzocri et al., D6.4 (2020), H2020 Project INSPYRE";

            break;
        }

        case 2:
        {
            double open_porosity = openPorosity(sciantix_variable["Fabrication porosity"].getFinalValue());
            sciantix_variable["Open porosity"].setFinalValue(open_porosity);

            sciantix_variable["Intergranular venting probability"].setFinalValue(1.54 * sqrt(open_porosity));

            reference = ": Claisse and Van Uffelen, JNM, 466 (2015): 351-356.";
            break;
        }

        case 3:
        {
            // TODO: implement the model in development branches
            double open_porosity = openPorosity(sciantix_variable["Fabrication porosity"].getFinalValue());
            sciantix_variable["Open porosity"].setFinalValue(open_porosity);

            sciantix_variable["Intergranular venting probability"].setFinalValue(0.0);

            reference = ": None";
            break;
        }

        default:
            ErrorMessages::Switch(
                __FILE__, "iGrainBoundaryVenting", int(input_variable["iGrainBoundaryVenting"].getValue()));
            break;
    }

    parameter.push_back(sciantix_variable["Intergranular venting probability"].getFinalValue());

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    for (auto& system : sciantix_system)
    {
        sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(
            solver.Integrator(sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue(),
                              -sciantix_variable["Intergranular venting probability"].getFinalValue(),
                              sciantix_variable[system.getGasName() + " at grain boundary"].getIncrement()));
    }
}

double Simulation::openPorosity(double fabrication_porosity)
{
    // Model declaration
    Model model_;
    model_.setName("Grain-boundary venting");

    std::vector<double> parameter;
    std::string         reference;

    switch (int(input_variable["iGrainBoundaryVenting"].getValue()))
    {
        case 0:
        case 1:
            return 0;
            break;

        case 2:
        {
            if (fabrication_porosity <= 1.0)
            {
                const bool check1 = (fabrication_porosity < 0.050) ? 1 : 0;
                const bool check2 = (fabrication_porosity > 0.058) ? 1 : 0;

                return ((fabrication_porosity / 20) * (check1) +
                        (3.10 * fabrication_porosity - 0.1525) * (!check1 && !check2) +
                        (fabrication_porosity / 2.1 - 3.2e-4) * (check2));
            }

            else
            {
                std::cout << "ERROR: invalid fabrication porosity value!" << std::endl;
                return (0);
            }
        }
        break;

        case 3:
        {
            if (fabrication_porosity <= 1.0)
            {
                double p_open;
                p_open = 0.028 / (1.0 + exp(-500.0 * (fabrication_porosity - 0.055))) + 0.05 * fabrication_porosity;

                return (p_open);
            }

            else
            {
                std::cout << "ERROR: invalid fabrication porosity value!" << std::endl;
                return (0);
            }
        }
        break;

        default:
            ErrorMessages::Switch("GrainBoundaryVenting.C",
                                  "iGrainBoundaryVenting",
                                  int(input_variable["iGrainBoundaryVenting"].getValue()));
            return (0);
            break;
    }
}