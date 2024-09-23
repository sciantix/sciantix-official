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

void Simulation::InterGranularBubbleBehavior()
{
    // Model declaration
    Model model_;
    Matrix fuel_(matrices[0]);

    model_.setName("Intergranular bubble behavior");
    std::string reference;
    std::vector<double> parameter;

    switch (int(input_variable["iGrainBoundaryBehaviour"].getValue()))
    {
        case 0:
        {
            parameter.push_back(0.0);
            parameter.push_back(0.0);

            reference += ": No model for grain-boundary bubble evolution.";
            break;
        }

        case 1:
        {
            // Gas is distributed among bubbles
            // n(at/bub) = c(at/m3) / (N(bub/m2) S/V(1/m))
            double n_at(0);

            for (auto& system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].setFinalValue(
                        sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() /
                        (sciantix_variable["Intergranular bubble concentration"].getInitialValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));

                    n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
                }
            }
            sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

            // Calculation of the bubble dimension
            // initial volume
            double vol(0);
            for (auto& system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    vol += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue() *
                        system.getGas().getVanDerWaalsVolume();
                }
            }
            vol += sciantix_variable["Intergranular vacancies per bubble"].getInitialValue() * fuel_.getSchottkyVolume();
            sciantix_variable["Intergranular bubble volume"].setInitialValue(vol);

            // initial radius
            sciantix_variable["Intergranular bubble radius"].setInitialValue(
                0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getInitialValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.)
            );

            // initial area
            sciantix_variable["Intergranular bubble area"].setInitialValue(
                M_PI * pow(sciantix_variable["Intergranular bubble radius"].getInitialValue() * sin(fuel_.getSemidihedralAngle()), 2)
            );

            // initial fractional coverage  
            sciantix_variable["Intergranular fractional coverage"].setInitialValue(
                sciantix_variable["Intergranular bubble concentration"].getInitialValue() *
                sciantix_variable["Intergranular bubble area"].getInitialValue()
            );

            // approximation of 1/S, S = -1/4 ((1-F)(3-F)+2lnF)
            const double AA = 1830.1;
            const double BB = -1599.2;
            const double CC = 690.91;
            const double DD = -99.993;
            const double EE = 20.594;

            double sink_strength = 0.4054 +
                AA * pow(sciantix_variable["Intergranular fractional coverage"].getInitialValue(), 5) +
                BB * pow(sciantix_variable["Intergranular fractional coverage"].getInitialValue(), 4) +
                CC * pow(sciantix_variable["Intergranular fractional coverage"].getInitialValue(), 3) +
                DD * pow(sciantix_variable["Intergranular fractional coverage"].getInitialValue(), 2) +
                EE * sciantix_variable["Intergranular fractional coverage"].getInitialValue();

            double volume_flow_rate
                = 2.0 * M_PI * fuel_.getGrainBoundaryThickness() * fuel_.getGrainBoundaryVacancyDiffusivity() * sink_strength;

            // Initial value of the growth rate = 2 pi t D n / S V
            const double growth_rate = volume_flow_rate * sciantix_variable["Intergranular atoms per bubble"].getFinalValue() / fuel_.getSchottkyVolume();

            double equilibrium_pressure(0), equilibrium_term(0);
            if (sciantix_variable["Intergranular bubble radius"].getInitialValue())
            {
                equilibrium_pressure = 2.0 * fuel_.getSurfaceTension() / sciantix_variable["Intergranular bubble radius"].getInitialValue() - history_variable["Hydrostatic stress"].getFinalValue() * 1e6;

                equilibrium_term = -volume_flow_rate * equilibrium_pressure / (boltzmann_constant * history_variable["Temperature"].getFinalValue());
            }

            parameter.push_back(growth_rate);
            parameter.push_back(equilibrium_term);

            reference += ": Pastore et al., NED, 256 (2013) 75-86.";

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iGrainBoundaryBehaviour", int(input_variable["iGrainBoundaryBehaviour"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    // Vacancy concentration
    sciantix_variable["Intergranular vacancies per bubble"].setFinalValue(
        solver.LimitedGrowth(
            sciantix_variable["Intergranular vacancies per bubble"].getInitialValue(),
            model["Intergranular bubble behavior"].getParameter(),
            physics_variable["Time step"].getFinalValue()
        )
    );

    // Grain-boundary bubble volume
    double vol(0);
    for (auto &system : sciantix_system)
    {
        if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            vol += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue() * system.getGas().getVanDerWaalsVolume();
        }
    }
    vol += sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * fuel_.getSchottkyVolume();
    sciantix_variable["Intergranular bubble volume"].setFinalValue(vol);

    // Grain-boundary bubble radius
    sciantix_variable["Intergranular bubble radius"].setFinalValue(
        0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.)
    );

    // Grain-boundary bubble area
    sciantix_variable["Intergranular bubble area"].setFinalValue(
        M_PI * pow(sciantix_variable["Intergranular bubble radius"].getFinalValue() * sin(fuel_.getSemidihedralAngle()), 2)
    );

    // Grain-boundary bubble coalescence
    double dbubble_area = sciantix_variable["Intergranular bubble area"].getIncrement();
    sciantix_variable["Intergranular bubble concentration"].setFinalValue(
        solver.BinaryInteraction(sciantix_variable["Intergranular bubble concentration"].getInitialValue(), 2.0, dbubble_area)
    );

    // Conservation
    for (auto &system : sciantix_system)
    {
        if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].rescaleFinalValue(
                sciantix_variable["Intergranular bubble concentration"].getInitialValue() / sciantix_variable["Intergranular bubble concentration"].getFinalValue()
            );
        }
    }

    double n_at(0);
    for (auto &system : sciantix_system)
    {
        if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
    }
    sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

    sciantix_variable["Intergranular vacancies per bubble"].rescaleFinalValue(
        sciantix_variable["Intergranular bubble concentration"].getInitialValue() / sciantix_variable["Intergranular bubble concentration"].getFinalValue()
    );

    vol = 0.0;
    for (auto &system : sciantix_system)
    {
        if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            vol += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue() *
                    system.getGas().getVanDerWaalsVolume();
    }

    vol += sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * fuel_.getSchottkyVolume();
    sciantix_variable["Intergranular bubble volume"].setFinalValue(vol);

    sciantix_variable["Intergranular bubble radius"].setFinalValue(
        0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.)
    );

    sciantix_variable["Intergranular bubble area"].setFinalValue(
        M_PI * pow(sciantix_variable["Intergranular bubble radius"].getFinalValue() * sin(fuel_.getSemidihedralAngle()), 2)
    );

    // Fractional coverage
    sciantix_variable["Intergranular fractional coverage"].setFinalValue(
        sciantix_variable["Intergranular bubble area"].getFinalValue() * sciantix_variable["Intergranular bubble concentration"].getFinalValue()
    );

    // Intergranular gas release
    //                          F0
    //   ___________A0____________
    //   |_________A1__________  |
    //   |                    |  |
    //   |          F1        N1 N0
    //   |                    |  |
    //   |____________________|__|
    double similarity_ratio;

    if (sciantix_variable["Intergranular fractional coverage"].getFinalValue() > 0.0)
        similarity_ratio = sqrt(
            sciantix_variable["Intergranular saturation fractional coverage"].getFinalValue() / sciantix_variable["Intergranular fractional coverage"].getFinalValue());
    else
        similarity_ratio = 1.0;

    if (similarity_ratio < 1.0)
    {
        sciantix_variable["Intergranular bubble area"].rescaleFinalValue(similarity_ratio);
        sciantix_variable["Intergranular bubble concentration"].rescaleFinalValue(similarity_ratio);
        sciantix_variable["Intergranular fractional coverage"].rescaleFinalValue(pow(similarity_ratio, 2));
        sciantix_variable["Intergranular bubble volume"].rescaleFinalValue(pow(similarity_ratio, 1.5));
        sciantix_variable["Intergranular bubble radius"].rescaleFinalValue(pow(similarity_ratio, 0.5));
        sciantix_variable["Intergranular vacancies per bubble"].rescaleFinalValue(pow(similarity_ratio, 1.5));

        // New intergranular gas concentration
        for (auto &system : sciantix_system)
        {
            if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].rescaleFinalValue(pow(similarity_ratio, 1.5));
        }

        n_at = 0.0;
        for (auto &system : sciantix_system)
        {
            if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
        }
        sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

        for (auto &system : sciantix_system)
        {
            if (system.getRestructuredMatrix() == 0)
                sciantix_variable[system.getGasName() + " at grain boundary"].rescaleFinalValue(pow(similarity_ratio, 2.5));
        }
    }

    // Intergranular bubble pressure p = kTng/Onv (MPa)
    if(sciantix_variable["Intergranular vacancies per bubble"].getFinalValue())
        sciantix_variable["Intergranular bubble pressure"].setFinalValue(
            1e-6 * boltzmann_constant * history_variable["Temperature"].getFinalValue() * sciantix_variable["Intergranular atoms per bubble"].getFinalValue() /
            (sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * matrices["UO2"].getSchottkyVolume())
        );
    else
        sciantix_variable["Intergranular bubble pressure"].setFinalValue(0.0);

    // Calculation of the gas concentration arrived at the grain boundary, by mass balance.
    for (auto &system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[system.getGasName() + " released"].setFinalValue(
                sciantix_variable[system.getGasName() + " produced"].getFinalValue() -
                sciantix_variable[system.getGasName() + " decayed"].getFinalValue() -
                sciantix_variable[system.getGasName() + " in grain"].getFinalValue() -
                sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue()
            );

            if (sciantix_variable[system.getGasName() + " released"].getFinalValue() < 0.0)
                sciantix_variable[system.getGasName() + " released"].setFinalValue(0.0);
        }
    }

    // Intergranular gaseous swelling
    sciantix_variable["Intergranular gas swelling"].setFinalValue(
        3 / sciantix_variable["Grain radius"].getFinalValue() *
        sciantix_variable["Intergranular bubble concentration"].getFinalValue() *
        sciantix_variable["Intergranular bubble volume"].getFinalValue()
    );

    // Fission gas release 
    if (sciantix_variable["Xe produced"].getFinalValue() + sciantix_variable["Kr produced"].getFinalValue() > 0.0)
        sciantix_variable["Fission gas release"].setFinalValue(
            (sciantix_variable["Xe released"].getFinalValue() + sciantix_variable["Kr released"].getFinalValue()) /
            (sciantix_variable["Xe produced"].getFinalValue() + sciantix_variable["Kr produced"].getFinalValue())
        );
    else
        sciantix_variable["Fission gas release"].setFinalValue(0.0);

    // Release-to-birth ratio: Xe133
    // Note that R/B is not defined with a null fission rate.
    if (sciantix_variable["Xe133 produced"].getFinalValue() - sciantix_variable["Xe133 decayed"].getFinalValue() > 0.0)
        sciantix_variable["Xe133 R/B"].setFinalValue(
            sciantix_variable["Xe133 released"].getFinalValue() /
            (sciantix_variable["Xe133 produced"].getFinalValue() - sciantix_variable["Xe133 decayed"].getFinalValue())
        );
    else
        sciantix_variable["Xe133 R/B"].setFinalValue(0.0);

    // Release-to-birth ratio: Kr85m
    // Note that R/B is not defined with a null fission rate.
    if (sciantix_variable["Kr85m produced"].getFinalValue() - sciantix_variable["Kr85m decayed"].getFinalValue() > 0.0)
        sciantix_variable["Kr85m R/B"].setFinalValue(
            sciantix_variable["Kr85m released"].getFinalValue() /
            (sciantix_variable["Kr85m produced"].getFinalValue() - sciantix_variable["Kr85m decayed"].getFinalValue())
        );
    else
        sciantix_variable["Kr85m R/B"].setFinalValue(0.0);

    // Helium fractional release
    if (sciantix_variable["He produced"].getFinalValue() > 0.0)
        sciantix_variable["He fractional release"].setFinalValue(
            sciantix_variable["He released"].getFinalValue() / sciantix_variable["He produced"].getFinalValue()
        );
    else
        sciantix_variable["He fractional release"].setFinalValue(0.0);

    // Helium release rate
    if (physics_variable["Time step"].getFinalValue() > 0.0)
        sciantix_variable["He release rate"].setFinalValue(
            sciantix_variable["He released"].getIncrement() / physics_variable["Time step"].getFinalValue()
        );
    else
        sciantix_variable["He release rate"].setFinalValue(0.0);
}