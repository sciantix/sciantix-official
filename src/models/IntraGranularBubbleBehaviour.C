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
//  Year: 2023                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

void Simulation::IntraGranularBubbleBehavior()
{
    Model model_;

    model_.setName("Intragranular bubble evolution");

    std::string reference;
    std::vector<double> parameter;

    switch (int(input_variable["iIntraGranularBubbleBehavior"].getValue()))
    {
    case 0:
    {
        reference += "No evolution.";

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
        /// @brief
        /// iIntraGranularBubbleBehavior == 1
        /// ----------------------------------
        ///
        /// The evolution of small intra-granular bubbles in fuel grains is controlled by bubble nucleation, gas atom trapping, and irradiation-induced gas atom re-solution back in the lattice.
        /// @see Description of the model in <a href="../../references/pdf_link/Pizzocri_et_al_2018.pdf" target="_blank">Pizzocri et al., JNM, 502 (2018) 323-330</a>.
        /// @param[out] intragranular_bubble_concentration
        /// @param[out] intragranular_bubble_radius
        
        reference += ": Pizzocri et al., JNM, 502 (2018) 323-330.";

        /// @param[in] resolution_rate
        parameter.push_back(sciantix_system[0].getResolutionRate());

        /// @param[in] nucleation_rate
        parameter.push_back(sciantix_system[0].getNucleationRate());

        break;
    }

    case 2:
    {
        /// @brief
        /// iIntraGranularBubbleBehavior == 2
        /// ----------------------------------
        ///
        /// The evolution of intragranular bubbles is modelled by means of temperature-driven correlations.
        /// @see Description of the model in <a href="../../references/pdf_link/White_and_Tucker_1983.pdf" target="_blank">White, Tucker, Journal of Nuclear Materials, 118 (1983), 1-38</a>.

        /// @param[in] local_fuel_temperature

        reference += "White and Tucker, JNM, 118 (1983), 1-38.";
        
        sciantix_variable["Intragranular bubble concentration"].setInitialValue(1.52e+27 / history_variable["Temperature"].getFinalValue() - 3.3e+23);
        parameter.push_back(0.0);
        parameter.push_back(0.0);
        break;
    }

    case 3:
    {
        /**
         * @brief iIntraGranularBubbleBehavior == 3
         * 
         * The evolution of intragranular bubble concentration, radius and atoms per bubble is described through
         * the similarity ratio, based on the evolution of intragranular concentration of gas in bubbles.
         */
    
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
        /**
         * @brief iIntraGranularBubbleBehavior == 99
         * No intragranular bubbles.
         * 
         * To be used with iTrappingRate = 99, iResolutionRate = 99, iNucleationRate = 99.
         * 
         * @param[out] intragranular_bubble_radius
         * @param[out] intragranular_bubble_concentration
         * 
         */

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


    // dN / dt = - getParameter().at(0) * N + getParameter().at(1)
    sciantix_variable["Intragranular bubble concentration"].setFinalValue(
        solver.Decay(
            sciantix_variable["Intragranular bubble concentration"].getInitialValue(),
            model["Intragranular bubble evolution"].getParameter().at(0),
            model["Intragranular bubble evolution"].getParameter().at(1),
            physics_variable["Time step"].getFinalValue()));

    // Atom per bubbles and bubble radius
    for (auto &system : sciantix_system)
    {
        if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            if (sciantix_variable["Intragranular bubble concentration"].getFinalValue() > 0.0)
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].setFinalValue(
                    sciantix_variable[system.getGasName() + " in intragranular bubbles"].getFinalValue() /
                    sciantix_variable["Intragranular bubble concentration"].getFinalValue());

            else
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].setFinalValue(0.0);

            sciantix_variable["Intragranular bubble volume"].addValue(
                system.getVolumeInLattice() * sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].getFinalValue());
        }
    }

    // Intragranular bubble radius
    sciantix_variable["Intragranular bubble radius"].setFinalValue(0.620350491 * pow(sciantix_variable["Intragranular bubble volume"].getFinalValue(), (1.0 / 3.0)));

    // Swelling
    // 4/3 pi N R^3
    sciantix_variable["Intragranular gas bubble swelling"].setFinalValue(4.188790205 *
        pow(sciantix_variable["Intragranular bubble radius"].getFinalValue(), 3) *
        sciantix_variable["Intragranular bubble concentration"].getFinalValue());

    if (sciantix_variable["He in intragranular bubbles"].getInitialValue() > 0.0)
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(sqrt(sciantix_variable["He in intragranular bubbles"].getFinalValue() / sciantix_variable["He in intragranular bubbles"].getInitialValue()));
    else
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(0.0);
}