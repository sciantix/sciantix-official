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

void Simulation::GrainBoundaryMicroCracking()
{
    // Model declaration
    if (!input_variable[iv["iGrainBoundaryMicroCracking"]].getValue()) return;

    model.emplace_back();
    int model_index = int(model.size()) - 1;
    model[model_index].setName("Grain-boundary micro-cracking");
    std::vector<double> parameter;

    const double dTemperature = history_variable[hv["Temperature"]].getIncrement();

    const bool heating = (dTemperature > 0.0) ? 1 : 0;
    const double transient_type = heating ? +1.0 : -1.0;
    const double span = 10.0;

    // microcracking parameter
    const double inflection = 1773.0 + 520.0 * exp(-sciantix_variable[sv["Burnup"]].getFinalValue() / (10.0 * 0.8814));
    const double exponent = 33.0;
    const double arg = (transient_type / span) * (history_variable[hv["Temperature"]].getFinalValue() - inflection);
    const double microcracking_parameter = (transient_type / span) * exp(arg) * pow((exponent * exp(arg) + 1), -1. / exponent - 1.); // dm/dT

    parameter.push_back(microcracking_parameter);

    // healing parameter
    const double healing_parameter = 1.0 / 0.8814; // 1 / (u * burnup)
    parameter.push_back(healing_parameter);

    model[model_index].setParameter(parameter);
    model[model_index].setRef("from Barani et al. (2017), JNM");

    // Model mapping
    MapModel();

    // Model resolution
    if (!input_variable[iv["iGrainBoundaryMicroCracking"]].getValue()) return;

    // ODE for the intergranular fractional intactness: this equation accounts for the reduction of the intergranular fractional intactness following a temperature transient
    // df / dT = - dm/dT f
    sciantix_variable[sv["Intergranular fractional intactness"]].setFinalValue(
        solver.Decay(sciantix_variable[sv["Intergranular fractional intactness"]].getInitialValue(),
            model[sm["Grain-boundary micro-cracking"]].getParameter().at(0), // 1st parameter = microcracking parameter
            0.0,
            history_variable[hv["Temperature"]].getIncrement()
        )
    );

    // ODE for the intergranular fractional coverage: this equation accounts for the reduction of the intergranular fractional coverage following a temperature transient
    // dFc / dT = - ( dm/dT f) Fc
    sciantix_variable[sv["Intergranular fractional coverage"]].setFinalValue(
        solver.Decay(sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue(),
            model[sm["Grain-boundary micro-cracking"]].getParameter().at(0) * sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue(),
            0.0,
            history_variable[hv["Temperature"]].getIncrement()
        )
    );

    // ODE for the saturation fractional coverage: this equation accounts for the reduction of the intergranular saturation fractional coverage following a temperature transient
    // dFcsat / dT = - (dm/dT f) Fcsat
    sciantix_variable[sv["Intergranular saturation fractional coverage"]].setFinalValue(
        solver.Decay(
            sciantix_variable[sv["Intergranular saturation fractional coverage"]].getInitialValue(),
            model[sm["Grain-boundary micro-cracking"]].getParameter().at(0) * sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue(),
            0.0,
            history_variable[hv["Temperature"]].getIncrement()
        )
    );

    // ODE for the intergranular fractional intactness: this equation accounts for the healing of the intergranular fractional intactness with burnup
    // df / dBu = - h f + h
    sciantix_variable[sv["Intergranular fractional intactness"]].setFinalValue(
        solver.Decay(
            sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue(),
            model[sm["Grain-boundary micro-cracking"]].getParameter().at(1),  // 2nd parameter = healing parameter
            model[sm["Grain-boundary micro-cracking"]].getParameter().at(1),
            sciantix_variable[sv["Burnup"]].getIncrement()
        )
    );

    // ODE for the saturation fractional coverage: this equation accounts for the healing of the intergranular saturation fractional coverage with burnup
    // dFcsat / dBu = h (1-f) Fcsat
    sciantix_variable[sv["Intergranular saturation fractional coverage"]].setFinalValue(
        solver.Decay(
            sciantix_variable[sv["Intergranular saturation fractional coverage"]].getFinalValue(),
            - model[sm["Grain-boundary micro-cracking"]].getParameter().at(1) * (1.0 - sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue()),
            0.0,
            sciantix_variable[sv["Burnup"]].getIncrement()
        )
    );

    // Re-scaling: to maintain the current fractional coverage unchanged
    double similarity_ratio;
    
    if (sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue() > 0.0)
        similarity_ratio = sqrt(
            sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue() / sciantix_variable[sv["Intergranular fractional coverage"]].getInitialValue()
        );
    else
        similarity_ratio = 1.0;

    if (similarity_ratio < 1.0)
    {
        sciantix_variable[sv["Intergranular bubble area"]].rescaleInitialValue(similarity_ratio);
        sciantix_variable[sv["Intergranular bubble concentration"]].rescaleInitialValue(similarity_ratio);
        sciantix_variable[sv["Intergranular fractional coverage"]].rescaleInitialValue(pow(similarity_ratio, 2));
        sciantix_variable[sv["Intergranular bubble volume"]].rescaleInitialValue(pow(similarity_ratio, 1.5));
        sciantix_variable[sv["Intergranular bubble radius"]].rescaleInitialValue(pow(similarity_ratio, 0.5));
        sciantix_variable[sv["Intergranular vacancies per bubble"]].rescaleInitialValue(pow(similarity_ratio, 1.5));

        for (auto& system : sciantix_system)
        {
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].rescaleInitialValue(pow(similarity_ratio, 1.5));
        }

        double n_at(0);
        for (auto& system : sciantix_system)
        {
            if (gas[ga[system.getGasName()]].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                n_at += sciantix_variable[sv["Intergranular " + system.getGasName() + " atoms per bubble"]].getInitialValue();
        }
        sciantix_variable[sv["Intergranular atoms per bubble"]].setInitialValue(n_at);

        for (auto& system : sciantix_system)
        {	
            if(system.getRestructuredMatrix() == 0)
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

            if (sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
                sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);
        }
    }
}
