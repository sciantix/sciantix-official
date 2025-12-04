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
    if (!int(input_variable["iGrainBoundaryMicroCracking"].getValue()))
        return;

    // Model declaration
    Model  model_;
    Matrix fuel_(matrices[0]);
    model_.setName("Grain-boundary micro-cracking");

    std::vector<double> parameter;
    std::string         reference;

    switch (int(input_variable["iGrainBoundaryMicroCracking"].getValue()))
    {
        case 0:
        {
            parameter.push_back(0.0);
            parameter.push_back(0.0);
            parameter.push_back(0.0);

            reference = ": not considered.";

            break;
        }

        case 1:
        {
            const double dTemperature = history_variable["Temperature"].getIncrement();

            const bool   heating        = (dTemperature > 0.0) ? 1 : 0;
            const double transient_type = heating ? +1.0 : -1.0;
            const double span           = 10.0;

            // microcracking parameter
            const double inflection =
                1773.0 +
                520.0 * exp(-sciantix_variable["Burnup"].getFinalValue() / (10.0 * 0.8814));
            const double exponent = 33.0;
            const double arg      = (transient_type / span) *
                               (history_variable["Temperature"].getFinalValue() - inflection);
            const double microcracking_parameter =
                (transient_type / span) * exp(arg) *
                pow((exponent * exp(arg) + 1), -1. / exponent - 1.);  // dm/dT

            parameter.push_back(microcracking_parameter);
            parameter.push_back(dTemperature);

            // healing parameter
            const double healing_parameter = 1.0 / 0.8814;  // 1 / (u * burnup)
            parameter.push_back(healing_parameter);

            reference = ": Barani et al. (2017), JNM";

            break;
        }

        case 2:
        {
            // Material properties
            double E  = fuel_.getElasticModulus() * 1e6;  // Pa
            double nu = fuel_.getPoissonRatio();
            double G  = fuel_.getGrainBoundaryFractureEnergy();  // J/m2

            if (sciantix_variable["Intergranular bubble radius"].getFinalValue() == 0)
            {
                parameter.push_back(0.0);
                parameter.push_back(0.0);
                parameter.push_back(1.0 / 0.8814);
                break;
            };  // m

            // Fracture toughness
            double K_IC = sqrt(E * G / (1.0 - pow(nu, 2))) * 1.0e-6;  // (MPa m0.5)

            // Stress intensification at grain-boundary bubble tip
            // ABAQUS 3D fitting:
            double stressintensification = 3.25;

            // Equilibrium pressure by capillary pressure and hydrostatic stress
            double equilibriumpressure =
                2.0 * fuel_.getSurfaceTension() * (1 - cos(fuel_.getSemidihedralAngle())) /
                    sciantix_variable["Intergranular bubble radius"].getFinalValue() -
                history_variable["Hydrostatic stress"].getFinalValue() * 1.0e6;  // Pa

            // Geometrical factor accounting for fractional coverage of the grain face h(F_c, Y)
            // defined only if F_c > 0 otherwise tends to 1.
            double Y = 3.28;
            double h(1.0);
            if (sciantix_variable["Intergranular fractional coverage"].getFinalValue() > 0.01)
                h = pow(1.0 -
                            1.0 / (M_PI * Y *
                                   (2.0 *
                                    sqrt(pow(sciantix_variable["Intergranular fractional coverage"]
                                                 .getFinalValue(),
                                             -0.5) -
                                         1.0))),
                        -1.0);

            // Pcrit = Peq + fracture stress
            // fracture stress = Kic*sqrt(1/pi*radius)*(1/kt) //Pa
            double fracture_stress =
                K_IC * 1e6 /
                (stressintensification * h *
                 sqrt(M_PI * sciantix_variable["Intergranular bubble radius"].getFinalValue() *
                      sin(fuel_.getSemidihedralAngle())));

            // Critical pressure:
            double critical_bubble_pressure = equilibriumpressure + fracture_stress;  // Pa

            // Upper limit for atom-to-vacancy ratio to limit the pressure value
            double maxatompervacancy = 1.0;
            double atompervacancyf   = 0.0;

            if (sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() != 0)
            {
                atompervacancyf =
                    sciantix_variable["Intergranular atoms per bubble"].getFinalValue() /
                    sciantix_variable["Intergranular vacancies per bubble"].getFinalValue();
            }
            if (atompervacancyf > maxatompervacancy)
            {
                atompervacancyf = maxatompervacancy;
            }

            // Bubble gas pressure
            double bubble_pressure =
                (boltzmann_constant * history_variable["Temperature"].getFinalValue() *
                 atompervacancyf / fuel_.getSchottkyVolume());  // Pa

            double microcracking_parameter = 0.0;
            if (bubble_pressure > critical_bubble_pressure && bubble_pressure > equilibriumpressure)
            {
                microcracking_parameter =
                    (bubble_pressure - equilibriumpressure) *
                    sciantix_variable["Intergranular fractional coverage"].getFinalValue() / G;
            }

            // Microcracking parameter
            parameter.push_back(microcracking_parameter);

            // Increment in opening surface (0 to minor-axis bubble radius)
            parameter.push_back((1.0 - cos(fuel_.getSemidihedralAngle())) *
                                sciantix_variable["Intergranular bubble radius"].getFinalValue());

            // healing parameter from Barani et al.(2017)
            const double healing_parameter = 1.0 / 0.8814;  // 1 / (u * burnup)
            parameter.push_back(healing_parameter);

            reference =
                ": Cappellari et al., JNM, (2025, under review); healing from Barani et al. "
                "(2017), JNM";

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iGrainBoundaryMicroCracking",
                                  int(input_variable["iGrainBoundaryMicroCracking"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    // ODE for the intergranular fractional intactness
    // This equation accounts for the reduction of the intergranular fractional intactness following
    // a transient df / dT = - dm/dT f for case 1, temperature transient df / dq = - dm/dq f for
    // case 2, all transients causing the opening q of the grain boundary
    sciantix_variable["Intergranular fractional intactness"].setFinalValue(solver.Decay(
        sciantix_variable["Intergranular fractional intactness"].getInitialValue(),
        model["Grain-boundary micro-cracking"].getParameter().at(
            0),  // 1st parameter = microcracking parameter
        0.0,
        model["Grain-boundary micro-cracking"].getParameter().at(1)  // Increment of interest
        ));

    // Gas inventory and grain boundary storage capability affected: change in fractional coverage
    // and saturation fractional coverage
    if (input_variable["iReleaseMode"].getValue() == 0)
    {
        // ODE for the intergranular fractional coverage:
        // This equation accounts for the reduction of the intergranular fractional coverage
        // following a transient dFc = + Fc df
        sciantix_variable["Intergranular fractional coverage"].setFinalValue(solver.Decay(
            sciantix_variable["Intergranular fractional coverage"].getInitialValue(), 1.0, 0.0,
            -sciantix_variable["Intergranular fractional intactness"]
                 .getIncrement()));  // Increment of interest

        // ODE for the saturation fractional coverage:
        // This equation accounts for the reduction of the intergranular saturation fractional
        // coverage following a transient dFcsat = + Fcsat df
        sciantix_variable["Intergranular saturation fractional coverage"].setFinalValue(solver.Decay(
            sciantix_variable["Intergranular saturation fractional coverage"].getInitialValue(),
            1.0, 0.0, -sciantix_variable["Intergranular fractional intactness"].getIncrement()));
    }

    // ODE for the intergranular fractional intactness:
    // This equation accounts for the healing of the intergranular fractional intactness with burnup
    // df / dBu = - h f + h
    sciantix_variable["Intergranular fractional intactness"].setFinalValue(
        solver.Decay(sciantix_variable["Intergranular fractional intactness"].getFinalValue(),
                     model["Grain-boundary micro-cracking"].getParameter().at(
                         2),  // 3nd parameter = healing parameter
                     model["Grain-boundary micro-cracking"].getParameter().at(2),
                     sciantix_variable["Burnup"].getIncrement()));

    // Gas inventory and grain boundary storage capability affected: change in fractional coverage
    // and saturation fractional coverage
    if (input_variable["iReleaseMode"].getValue() == 0)
    {
        // ODE for the saturation fractional coverage:
        // This equation accounts for the healing of the intergranular saturation fractional
        // coverage with burnup dFcsat / dBu = h (1-f) Fcsat
        sciantix_variable["Intergranular saturation fractional coverage"].setFinalValue(solver.Decay(
            sciantix_variable["Intergranular saturation fractional coverage"].getFinalValue(),
            -model["Grain-boundary micro-cracking"].getParameter().at(2) *
                (1.0 - sciantix_variable["Intergranular fractional intactness"].getFinalValue()),
            0.0, sciantix_variable["Burnup"].getIncrement()));
    }

    if (input_variable["iReleaseMode"].getValue() == 0)
    {
        // Re-scaling: to conserve the fractional coverage
        double similarity_ratio;

        if (sciantix_variable["Intergranular fractional coverage"].getInitialValue() > 0.0)
            similarity_ratio =
                sqrt(sciantix_variable["Intergranular fractional coverage"].getFinalValue() /
                     sciantix_variable["Intergranular fractional coverage"].getInitialValue());
        else
            similarity_ratio = 1.0;

        if (similarity_ratio < 1.0)
        {
            sciantix_variable["Intergranular bubble area"].rescaleInitialValue(similarity_ratio);
            sciantix_variable["Intergranular bubble concentration"].rescaleInitialValue(
                similarity_ratio);
            sciantix_variable["Intergranular fractional coverage"].rescaleInitialValue(
                pow(similarity_ratio, 2));
            sciantix_variable["Intergranular bubble volume"].rescaleInitialValue(
                pow(similarity_ratio, 1.5));
            sciantix_variable["Intergranular bubble radius"].rescaleInitialValue(
                pow(similarity_ratio, 0.5));
            sciantix_variable["Intergranular vacancies per bubble"].rescaleInitialValue(
                pow(similarity_ratio, 1.5));

            for (auto& system : sciantix_system)
            {
                if (gas[system.getGasName()].getDecayRate() == 0.0 &&
                    system.getRestructuredMatrix() == 0)
                    sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"]
                        .rescaleInitialValue(pow(similarity_ratio, 1.5));
            }

            double n_at(0);
            for (auto& system : sciantix_system)
            {
                if (gas[system.getGasName()].getDecayRate() == 0.0 &&
                    system.getRestructuredMatrix() == 0)
                    n_at += sciantix_variable["Intergranular " + system.getGasName() +
                                              " atoms per bubble"]
                                .getInitialValue();
            }
            sciantix_variable["Intergranular atoms per bubble"].setInitialValue(n_at);

            for (auto& system : sciantix_system)
            {
                if (system.getRestructuredMatrix() == 0)
                    sciantix_variable[system.getGasName() + " at grain boundary"].rescaleFinalValue(
                        pow(similarity_ratio, 2.5));
            }
        }
    }
}