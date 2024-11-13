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

void Simulation::GrainBoundaryMicroCracking()
{
    if (!input_variable["iGrainBoundaryMicroCracking"].getValue()) return;

    // Model declaration
    Model model_;
    model_.setName("Grain-boundary micro-cracking");
    std::vector<double> parameter;

    const double dTemperature = history_variable["Temperature"].getIncrement();

    const bool heating = (dTemperature > 0.0) ? 1 : 0;
    const double transient_type = heating ? +1.0 : -1.0;
    const double span = 10.0;

    // microcracking parameter
    const double inflection = 1773.0 + 520.0 * exp(-sciantix_variable["Burnup"].getFinalValue() / (10.0 * 0.8814));
    const double exponent = 33.0;
    const double arg = (transient_type / span) * (history_variable["Temperature"].getFinalValue() - inflection);
    const double microcracking_parameter = (transient_type / span) * exp(arg) * pow((exponent * exp(arg) + 1), -1. / exponent - 1.); // dm/dT

    parameter.push_back(microcracking_parameter);

    // healing parameter
    const double healing_parameter = 1.0 / 0.8814; // 1 / (u * burnup)
    parameter.push_back(healing_parameter);

    model_.setParameter(parameter);
    model_.setRef(" : from Barani et al. (2017), JNM");

    model.push(model_);

    // Model resolution

    
    // ODE for the intergranular fractional intactness
    // This equation accounts for the reduction of the intergranular fractional intactness following a temperature transient
    // df / dT = - dm/dT f
    sciantix_variable["Intergranular fractional intactness"].setFinalValue(
        solver.Decay(sciantix_variable["Intergranular fractional intactness"].getInitialValue(),
            model["Grain-boundary micro-cracking"].getParameter().at(0), // 1st parameter = microcracking parameter
            0.0,
            history_variable["Temperature"].getIncrement()
        )
    );

    // ODE for the intergranular fractional coverage:
    // This equation accounts for the reduction of the intergranular fractional coverage following a temperature transient
    // dFc / dT = - ( dm/dT f) Fc
    sciantix_variable["Intergranular fractional coverage"].setFinalValue(
        solver.Decay(sciantix_variable["Intergranular fractional coverage"].getInitialValue(),
                        model["Grain-boundary micro-cracking"].getParameter().at(0) * sciantix_variable["Intergranular fractional intactness"].getFinalValue(),
                        0.0,
                        history_variable["Temperature"].getIncrement()));

    // ODE for the saturation fractional coverage:
    // This equation accounts for the reduction of the intergranular saturation fractional coverage following a temperature transient
    // dFcsat / dT = - (dm/dT f) Fcsat
    sciantix_variable["Intergranular saturation fractional coverage"].setFinalValue(
        solver.Decay(
            sciantix_variable["Intergranular saturation fractional coverage"].getInitialValue(),
            model["Grain-boundary micro-cracking"].getParameter().at(0) * sciantix_variable["Intergranular fractional intactness"].getFinalValue(),
            0.0,
            history_variable["Temperature"].getIncrement()));
    //std::cout << "microcracking = " << input_variable["iGrainBoundaryMicroCracking"].getValue() << std::endl; 
    //@changes M. Di Gennaro aggiunto l'if perchè SUPERFACT-1 e SPHERE crashano per problemi di convergenza.
    if (input_variable["iGrainBoundaryMicroCracking"].getValue() != 2)
    {
        // ODE for the intergranular fractional intactness:
        // This equation accounts for the healing of the intergranular fractional intactness with burnup
        // df / dBu = - h f + h
        sciantix_variable["Intergranular fractional intactness"].setFinalValue(
            solver.Decay(
                sciantix_variable["Intergranular fractional intactness"].getFinalValue(),
                model["Grain-boundary micro-cracking"].getParameter().at(1), // 2nd parameter = healing parameter
                model["Grain-boundary micro-cracking"].getParameter().at(1),
                sciantix_variable["Burnup"].getIncrement()));
        //std::cout << "Entra dentro l'healing" << std::endl; 
        
        // ODE for the saturation fractional coverage:
        // This equation accounts for the healing of the intergranular saturation fractional coverage with burnup
        // dFcsat / dBu = h (1-f) Fcsat
        sciantix_variable["Intergranular saturation fractional coverage"].setFinalValue(
            solver.Decay(
                sciantix_variable["Intergranular saturation fractional coverage"].getFinalValue(),
                -model["Grain-boundary micro-cracking"].getParameter().at(1) * (1.0 - sciantix_variable["Intergranular fractional intactness"].getFinalValue()),
                0.0,
                sciantix_variable["Burnup"].getIncrement()));
    }
    //@changes M. Di Gennaro


    // Re-scaling: to conserve the fractional coverage
    double similarity_ratio;

    if (sciantix_variable["Intergranular fractional coverage"].getInitialValue() > 0.0)
        similarity_ratio = sqrt(sciantix_variable["Intergranular fractional coverage"].getFinalValue() / sciantix_variable["Intergranular fractional coverage"].getInitialValue());
    else
        similarity_ratio = 1.0;

    if (similarity_ratio < 1.0)
    {
        sciantix_variable["Intergranular bubble area"].rescaleInitialValue(similarity_ratio);
        sciantix_variable["Intergranular bubble concentration"].rescaleInitialValue(similarity_ratio);
        sciantix_variable["Intergranular fractional coverage"].rescaleInitialValue(pow(similarity_ratio, 2));
        sciantix_variable["Intergranular bubble volume"].rescaleInitialValue(pow(similarity_ratio, 1.5));
        sciantix_variable["Intergranular bubble radius"].rescaleInitialValue(pow(similarity_ratio, 0.5));
        sciantix_variable["Intergranular vacancies per bubble"].rescaleInitialValue(pow(similarity_ratio, 1.5));

        for (auto &system : sciantix_system)
        {
            if (gas[system.getGasName()].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].rescaleInitialValue(pow(similarity_ratio, 1.5));
        }

        double n_at(0);
        for (auto &system : sciantix_system)
        {
            if (gas[system.getGasName()].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getInitialValue();
        }
        sciantix_variable["Intergranular atoms per bubble"].setInitialValue(n_at);

        for (auto &system : sciantix_system)
        {
            if (system.getRestructuredMatrix() == 0)
                sciantix_variable[system.getGasName() + " at grain boundary"].rescaleFinalValue(pow(similarity_ratio, 2.5));
        }
    }

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

            if (sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() < 0.0)
                sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(0.0);
        }
    }
}
