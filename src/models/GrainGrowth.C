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

void Simulation::GrainGrowth()
{
    // Model declaration
    Model model_;

    model_.setName("Grain growth");
    std::string reference;
    std::vector<double> parameter;

    switch (int(input_variable["iGrainGrowth"].getValue()))
    {
        case 0:
        {
            reference += " : constant grain radius.";

            parameter.push_back(sciantix_variable["Grain radius"].getInitialValue());
            parameter.push_back(0.0);
            parameter.push_back(0.0);
            parameter.push_back(0.0);
            parameter.push_back(1.0);
            parameter.push_back(-sciantix_variable["Grain radius"].getInitialValue());

            break;
        }

        /** @brief iGrainGrowth = 1 considers that the grain growth kinetic is described by the semi-empirical model from
         * <a href="https://www.sciencedirect.com/science/article/abs/pii/0022311573900019" target="_blank">Ainscough J.B. et al (1973). Journal of Nuclear Materials, 49, 117-128.</a>
         * This model includes two contributions on the grain growth:
         * 1. Temperature;
         * 2. Burnup (i.e., increasing retarding effect due fission product accumulation).
         * 
         * Note that, the equation for grain growth is written in grain size.
         */
        case 1:
        {
            reference += " : Ainscough et al., JNM, 49 (1973) 117-128.";

            double limiting_grain_radius = 2.23e-03 * (1.56/2.0) * exp(-7620.0 / history_variable["Temperature"].getFinalValue());
            double burnup_factor = 1.0 + 2.0 * sciantix_variable["Burnup"].getFinalValue() / 0.8815;

            if (sciantix_variable["Grain radius"].getInitialValue() < limiting_grain_radius / burnup_factor)
            {
                double rate_constant = matrices["UO2"].getGrainBoundaryMobility();
                rate_constant *= (1.0 - burnup_factor / (limiting_grain_radius / (sciantix_variable["Grain radius"].getFinalValue())));

                parameter.push_back(sciantix_variable["Grain radius"].getInitialValue());
                parameter.push_back(0.0);
                parameter.push_back(0.0);
                parameter.push_back(1.0);
                parameter.push_back(- sciantix_variable["Grain radius"].getInitialValue());
                parameter.push_back(- rate_constant * physics_variable["Time step"].getFinalValue());

            }

            else
            {
                parameter.push_back(sciantix_variable["Grain radius"].getInitialValue());
                parameter.push_back(0.0);
                parameter.push_back(0.0);
                parameter.push_back(0.0);
                parameter.push_back(1.0);
                parameter.push_back(- sciantix_variable["Grain radius"].getInitialValue());
            }
            break;
        }

        /**
         * @brief The grain growth kinetic is described according to 
         * @ref <a href="https://www.sciencedirect.com/science/article/abs/pii/S0022311512006526" target="_blank">Van Uffelen P. et al (2013). Journal of Nuclear Materials, 434, 287-290.</a>
         * by means of the following equations:
         * 1) dD/dt = k/4D^3   if D < Dm
         * 2) dD/dt = 0        if D > Dm
         * where
         * D = grain diameter (um)
         * k, the rate constant, is
         * k =  3.1347e+14 * exp(-46524 / T)  (um^4/h)
         * T = temperature (K)
         * Dm = limiting grain diameter
         */
        case 2 :
        {
            double limiting_grain_radius = 3.345e-3 / 2.0 * exp(-7620.0 / history_variable["Temperature"].getFinalValue()); // (m)

            reference += "Van Uffelen et al. JNM, 434 (2013) 287–29.";

            if(sciantix_variable["Grain radius"].getInitialValue() < limiting_grain_radius)
            {
                double rate_constant = matrices["UO2"].getGrainBoundaryMobility();

                parameter.push_back(sciantix_variable["Grain radius"].getInitialValue());
                parameter.push_back(1.0);
                parameter.push_back(- sciantix_variable["Grain radius"].getInitialValue());
                parameter.push_back(0.0);
                parameter.push_back(0.0);
                parameter.push_back(- rate_constant * physics_variable["Time step"].getFinalValue());
            }
            else
            {
                parameter.push_back(sciantix_variable["Grain radius"].getInitialValue());
                parameter.push_back(0.0);
                parameter.push_back(0.0);
                parameter.push_back(0.0);
                parameter.push_back(1.0);
                parameter.push_back(- sciantix_variable["Grain radius"].getInitialValue());
            }
            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iGrainGrowth", int(input_variable["iGrainGrowth"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    sciantix_variable["Grain radius"].setFinalValue(
        solver.QuarticEquation(model["Grain growth"].getParameter()));

    sciantix_variable["Grain radius"].resetValue();

    matrices["UO2"].setGrainRadius(sciantix_variable["Grain radius"].getFinalValue());
}