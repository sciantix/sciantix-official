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

/// InterGranularBubbleEvolution
/// This function contains a choice among possible
/// expressions for the bubble number density and
/// the bubble radius at grain boundaries.
/// The model considers one-off nucleation,
/// growth of lenticular bubbles by vacancy absorption
/// and coalescence of bubbles.
/// [2] White, JNM, 325 (2004) 61-77

#include "Simulation.h"

void Simulation::InterGranularBubbleBehavior()
{
    model.emplace_back();
    int model_index = int(model.size()) - 1;

    model[model_index].setName("Intergranular bubble evolution");
    std::string reference;
    std::vector<double> parameter;
    
    const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;
    const double boltzmann_constant = CONSTANT_NUMBERS_H::PhysicsConstants::boltzmann_constant;

    switch (int(input_variable[iv["iGrainBoundaryBehaviour"]].getValue()))
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
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            {
                sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].setFinalValue(
                    sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() /
                    (sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() * (3.0 / sciantix_variable[sv["Grain radius"]].getFinalValue())));

                n_at += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue();
            }
        }
        sciantix_variable[sv["Intergranular atoms per bubble"]].setFinalValue(n_at);

        // Calculation of the bubble dimension
        // initial volume
        double vol(0);
        for (auto& system : sciantix_system)
        {
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            {
                vol += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue() *
                    gas[ga[system.getGasName()]].getVanDerWaalsVolume();
            }
        }
        vol += sciantix_variable[sv["Intergranular vacancies per bubble"]].getInitialValue() * matrix[sma["UO2"]].getSchottkyVolume();
        sciantix_variable[sv["Intergranular bubble volume"]].setInitialValue(vol);

        // initial radius
        sciantix_variable[sv["Intergranular bubble radius"]].setInitialValue(
            0.620350491 * pow(sciantix_variable[sv["Intergranular bubble volume"]].getInitialValue() / (matrix[sma["UO2"]].getLenticularShapeFactor()), 1. / 3.));

        // initial area
        sciantix_variable[sv["Intergranular bubble area"]].setInitialValue(
            pi * pow(sciantix_variable[sv["Intergranular bubble radius"]].getInitialValue() * sin(matrix[sma["UO2"]].getSemidihedralAngle()), 2));

        // initial fractional coverage  
        sciantix_variable[sv["Intergranular fractional coverage"]].setInitialValue(
            sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() *
            sciantix_variable[sv["Intergranular bubble area"]].getInitialValue());

        // approximation of 1/S, S = -1/4 ((1-F)(3-F)+2lnF)
        const double AA = 1830.1;
        const double BB = -1599.2;
        const double CC = 690.91;
        const double DD = -99.993;
        const double EE = 20.594;

        double sink_strength = 0.4054 +
            AA * pow(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(), 5) +
            BB * pow(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(), 4) +
            CC * pow(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(), 3) +
            DD * pow(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(), 2) +
            EE * sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue();

        double volume_flow_rate
            = 2.0 * pi * matrix[sma["UO2"]].getGrainBoundaryThickness() * matrix[sma["UO2"]].getGrainBoundaryVacancyDiffusivity() * sink_strength;

        // Initial value of the growth rate = 2 pi t D n / S V
        const double growth_rate = volume_flow_rate * sciantix_variable[sv["Intergranular atoms per bubble"]].getFinalValue() / matrix[sma["UO2"]].getSchottkyVolume();

        double equilibrium_pressure(0), equilibrium_term(0);
        if (sciantix_variable[sv["Intergranular bubble radius"]].getInitialValue())
        {
            equilibrium_pressure = 2.0 * matrix[sma["UO2"]].getSurfaceTension() / sciantix_variable[sv["Intergranular bubble radius"]].getInitialValue() -
                history_variable[hv["Hydrostatic stress"]].getFinalValue() * 1e6;

            equilibrium_term = -volume_flow_rate * equilibrium_pressure /
                (boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue());
        }

        parameter.push_back(growth_rate);
        parameter.push_back(equilibrium_term);

        reference += ": Pastore et al., NED, 256 (2013) 75-86.";

        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "iGrainBoundaryBehaviour", int(input_variable[iv["iGrainBoundaryBehaviour"]].getValue()));
        break;
    }

    model[model_index].setParameter(parameter);
    model[model_index].setRef(reference);

    MapModel();

    // Vacancy concentration
    sciantix_variable[sv["Intergranular vacancies per bubble"]].setFinalValue(
        solver.LimitedGrowth(sciantix_variable[sv["Intergranular vacancies per bubble"]].getInitialValue(),
            model[sm["Intergranular bubble evolution"]].getParameter(),
            physics_variable[pv["Time step"]].getFinalValue()
        )
    );

    // Grain-boundary bubble volume
    double vol(0);
    for (auto& system : sciantix_system)
    {
        if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            vol += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue() *
                gas[ga[system.getGasName()]].getVanDerWaalsVolume();
        }
    }
    vol += sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[sma["UO2"]].getSchottkyVolume();
    sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(vol);

    // Grain-boundary bubble radius
    sciantix_variable[sv["Intergranular bubble radius"]].setFinalValue(
        0.620350491 * pow(sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue() / (matrix[sma["UO2"]].getLenticularShapeFactor()), 1. / 3.));

    // Grain-boundary bubble area
    sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(
        pi * pow(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() * sin(matrix[sma["UO2"]].getSemidihedralAngle()), 2));

    // Grain-boundary bubble coalescence
    double dbubble_area = sciantix_variable[sv["Intergranular bubble area"]].getIncrement();
    sciantix_variable[sv["Intergranular bubble concentration"]].setFinalValue(
        solver.BinaryInteraction(sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue(), 2.0, dbubble_area));

    // Conservation
    for (auto& system : sciantix_system)
    {
        if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].rescaleFinalValue(
                sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() / sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue()
            );
        }
    }

    double n_at(0);
    for (auto& system : sciantix_system)
    {
        if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            n_at += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue();
    }
    sciantix_variable[sv["Intergranular atoms per bubble"]].setFinalValue(n_at);

    sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleFinalValue(
        sciantix_variable[sv["Intergranular bubble concentration"]].getInitialValue() / sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue()
    );

    vol = 0.0;
    for (auto& system : sciantix_system)
    {
        if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            vol += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue() *
                gas[ga[system.getGasName()]].getVanDerWaalsVolume();
    }
    vol += sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[sma["UO2"]].getSchottkyVolume();
    sciantix_variable[sv["Intergranular bubble volume"]].setFinalValue(vol);

    sciantix_variable[sv["Intergranular bubble radius"]].setFinalValue(
        0.620350491 * pow(sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue() / (matrix[sma["UO2"]].getLenticularShapeFactor()), 1. / 3.));

    sciantix_variable[sv["Intergranular bubble area"]].setFinalValue(
        pi * pow(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() * sin(matrix[sma["UO2"]].getSemidihedralAngle()), 2));

    // Fractional coverage
    sciantix_variable[sv["Intergranular fractional coverage"]].setFinalValue(
        sciantix_variable[sv["Intergranular bubble area"]].getFinalValue() *
        sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue());

    // Intergranular gas release
    //                          F0
    //   ___________A0____________
    //   |_________A1__________  |
    //   |                    |  |
    //   |          F1        N1 N0
    //   |                    |  |
    //   |____________________|__|
    double similarity_ratio;
    
    if (sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue() > 0.0)
        similarity_ratio = sqrt(
            sciantix_variable[sv["Intergranular saturation fractional coverage"]].getFinalValue() /
            sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue()
        );
    else
        similarity_ratio = 1.0;

    if (similarity_ratio < 1.0)
    {
        sciantix_variable[sv["Intergranular bubble area"]].rescaleFinalValue(similarity_ratio);
        sciantix_variable[sv["Intergranular bubble concentration"]].rescaleFinalValue(similarity_ratio);
        sciantix_variable[sv["Intergranular fractional coverage"]].rescaleFinalValue(pow(similarity_ratio, 2));
        sciantix_variable[sv["Intergranular bubble volume"]].rescaleFinalValue(pow(similarity_ratio, 1.5));
        sciantix_variable[sv["Intergranular bubble radius"]].rescaleFinalValue(pow(similarity_ratio, 0.5));
        sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleFinalValue(pow(similarity_ratio, 1.5));

        // New intergranular gas concentration
        for (auto& system : sciantix_system)
        {
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].rescaleFinalValue(pow(similarity_ratio, 1.5));
        }

        n_at = 0.0;
        for (auto& system : sciantix_system)
        {
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                n_at += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getFinalValue();
        }
        sciantix_variable[sv["Intergranular atoms per bubble"]].setFinalValue(n_at);

        for (auto& system : sciantix_system)
        {
            if (system.getRestructuredMatrix() == 0)
                sciantix_variable[sv[system.getGasName() + " at grain boundary"]].rescaleFinalValue(pow(similarity_ratio, 2.5));
        }
    }

    // Calculation of the gas concentration arrived at the grain boundary, by mass balance.
    for (auto& system : sciantix_system)
    {
        if(system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
                sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue()
            );

            if (sciantix_variable[sv[system.getGasName() + " released"]].getFinalValue() < 0.0)
                sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(0.0);
        }
    }

    // Intergranular gaseous swelling
    sciantix_variable[sv["Intergranular gas swelling"]].setFinalValue(
        3 / sciantix_variable[sv["Grain radius"]].getFinalValue() *
        sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue() *
        sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue()
    );

    /// Fission gas release 
    if (sciantix_variable[sv["Xe produced"]].getFinalValue() + sciantix_variable[sv["Kr produced"]].getFinalValue() > 0.0)
        sciantix_variable[sv["Fission gas release"]].setFinalValue(
            (sciantix_variable[sv["Xe released"]].getFinalValue() + sciantix_variable[sv["Kr released"]].getFinalValue()) /
            (sciantix_variable[sv["Xe produced"]].getFinalValue() + sciantix_variable[sv["Kr produced"]].getFinalValue())
        );
    else
        sciantix_variable[sv["Fission gas release"]].setFinalValue(0.0);

  // Release-to-birth ratio: Xe133
  // Note that R/B is not defined with a null fission rate.
    if (sciantix_variable[sv["Xe133 produced"]].getFinalValue() - sciantix_variable[sv["Xe133 decayed"]].getFinalValue() > 0.0)
        sciantix_variable[sv["Xe133 R/B"]].setFinalValue(
        sciantix_variable[sv["Xe133 released"]].getFinalValue() /
        (sciantix_variable[sv["Xe133 produced"]].getFinalValue() - sciantix_variable[sv["Xe133 decayed"]].getFinalValue())
        );
    else
        sciantix_variable[sv["Xe133 R/B"]].setFinalValue(0.0);

  // Release-to-birth ratio: Kr85m
  // Note that R/B is not defined with a null fission rate.
    if (sciantix_variable[sv["Kr85m produced"]].getFinalValue() - sciantix_variable[sv["Kr85m decayed"]].getFinalValue() > 0.0)
            sciantix_variable[sv["Kr85m R/B"]].setFinalValue(
            sciantix_variable[sv["Kr85m released"]].getFinalValue() /
            (sciantix_variable[sv["Kr85m produced"]].getFinalValue() - sciantix_variable[sv["Kr85m decayed"]].getFinalValue())
        );
    else
        sciantix_variable[sv["Kr85m R/B"]].setFinalValue(0.0);

  // Helium fractional release
    if (sciantix_variable[sv["He produced"]].getFinalValue() > 0.0)
        sciantix_variable[sv["He fractional release"]].setFinalValue(
            sciantix_variable[sv["He released"]].getFinalValue() /
            sciantix_variable[sv["He produced"]].getFinalValue()
        );
    else
        sciantix_variable[sv["He fractional release"]].setFinalValue(0.0);

    // Helium release rate
    if (physics_variable[pv["Time step"]].getFinalValue() > 0.0)
        sciantix_variable[sv["He release rate"]].setFinalValue(
            sciantix_variable[sv["He released"]].getIncrement() /
            physics_variable[pv["Time step"]].getFinalValue()
        );
    else
        sciantix_variable[sv["He release rate"]].setFinalValue(0.0);

    // Intergranular bubble pressure p = kTng/Onv (MPa)
    if(sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue())
        sciantix_variable[sv["Intergranular bubble pressure"]].setFinalValue(1e-6 *
            boltzmann_constant * history_variable[hv["Temperature"]].getFinalValue() *
            sciantix_variable[sv["Intergranular atoms per bubble"]].getFinalValue() /
            (sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[sma["UO2"]].getSchottkyVolume())
        );
    else
        sciantix_variable[sv["Intergranular bubble pressure"]].setFinalValue(0.0);
}
