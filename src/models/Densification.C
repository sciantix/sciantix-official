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

void Simulation::Densification()
{	
    if (!int(input_variable["iDensification"].getValue())) return;

    // Model declaration
    Model model_;
    model_.setName("Densification");

	std::vector<double> parameter;
    std::string reference;

    switch (int(input_variable["iDensification"].getValue()))
    {
        case 0:
        {
            parameter.push_back(0);
            parameter.push_back(0);
            
            reference = ": not considered.";

            break;
        }
        case 1:
        {
            parameter.push_back(2.0);
            parameter.push_back(0.006*exp(0.002*history_variable["Temperature"].getFinalValue()));

            reference = ": Fit from P. Van Uffelen PhD thesis (2002) athermal release analysis.";

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iDensification", int(input_variable["iDensification"].getValue()));
            break;

    }

    model_.setParameter(parameter);
    model_.setRef(reference);
    model.push(model_);

    double dens_factor = solver.Decay(
        sciantix_variable["Densification factor"].getInitialValue(),
        model["Densification"].getParameter().at(0),
        model["Densification"].getParameter().at(1),
        sciantix_variable["Burnup"].getIncrement()
    );

    if (dens_factor < 1.0)
        sciantix_variable["Densification factor"].setFinalValue(dens_factor);
    else
        sciantix_variable["Densification factor"].setFinalValue(1.0);

    sciantix_variable["Fabrication porosity"].setFinalValue(
        sciantix_variable["Residual porosity"].getFinalValue() + (sciantix_variable["Fabrication porosity"].getFinalValue() - 
        sciantix_variable["Residual porosity"].getFinalValue()) * (1 - sciantix_variable["Densification factor"].getFinalValue())
    );

    sciantix_variable["Porosity"].addValue(sciantix_variable["Fabrication porosity"].getIncrement());
}