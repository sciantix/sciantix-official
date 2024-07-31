#include "Simulation.h"


Simulation& Simulation::getInstance()
{
    if (instance == nullptr)
    {
        instance = new Simulation;
    }
    return *instance;
}

void Simulation::initialize(
    int Sciantix_options[], 
    double Sciantix_history[], 
    double Sciantix_variables[], 
    double Sciantix_scaling_factors[], 
    double Sciantix_diffusion_modes[]
)
{
    setVariables(
        Sciantix_options, 
		Sciantix_history, 
		Sciantix_variables, 
		Sciantix_scaling_factors, 
		Sciantix_diffusion_modes
    );

    setGas();
    setMatrix();
    setSystem();
}

void Simulation::Burnup()
{
    sciantix_variable["Burnup"].setFinalValue(
        solver.Integrator(
            sciantix_variable["Burnup"].getInitialValue(),
            model["Burnup"].getParameter().at(0),
            physics_variable["Time step"].getFinalValue()));

    if (history_variable["Fission rate"].getFinalValue() > 0.0)
        sciantix_variable["Irradiation time"].setFinalValue(
            solver.Integrator(
                sciantix_variable["Irradiation time"].getInitialValue(),
                1.0 / sciantix_variable["Specific power"].getFinalValue(),
                24.0 * sciantix_variable["Burnup"].getIncrement()));
    else
        sciantix_variable["Irradiation time"].setConstant();

    sciantix_variable["FIMA"].setFinalValue(
        solver.Integrator(
            sciantix_variable["FIMA"].getInitialValue(),
            history_variable["Fission rate"].getFinalValue() * 3.6e5 / sciantix_variable["U"].getFinalValue(),
            sciantix_variable["Irradiation time"].getIncrement()));
}

void Simulation::EffectiveBurnup()
{

    sciantix_variable["Effective burnup"].setFinalValue(
        solver.Integrator(
            sciantix_variable["Effective burnup"].getInitialValue(),
            model["Effective burnup"].getParameter().at(0),
            physics_variable["Time step"].getFinalValue()));
}

void Simulation::GasProduction()
{
    for (auto &system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0)
            sciantix_variable[system.getGasName() + " produced"].setFinalValue(
                solver.Integrator(
                    sciantix_variable[system.getGasName() + " produced"].getInitialValue(),
                    model["Gas production - " + system.getName()].getParameter().at(0),
                    model["Gas production - " + system.getName()].getParameter().at(1)));
        else if (system.getRestructuredMatrix() == 1)
            sciantix_variable[system.getGasName() + " produced in HBS"].setFinalValue(
                solver.Integrator(
                    sciantix_variable[system.getGasName() + " produced in HBS"].getInitialValue(),
                    model["Gas production - " + system.getName()].getParameter().at(0),
                    model["Gas production - " + system.getName()].getParameter().at(1)));
    }
}

void Simulation::GasDecay()
{
    for (auto &system : sciantix_system)
    {
        if (gas[system.getGasName()].getDecayRate() > 0.0 && system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[system.getGasName() + " decayed"].setFinalValue(
                solver.Decay(
                    sciantix_variable[system.getGasName() + " decayed"].getInitialValue(),
                    gas[system.getGasName()].getDecayRate(),
                    gas[system.getGasName()].getDecayRate() * sciantix_variable[system.getGasName() + " produced"].getFinalValue(), // sarebbe produced + produced in HBS ma le seconde devono esistere per tutte le specie..
                    physics_variable["Time step"].getFinalValue()));
        }
    }
}


void Simulation::GasDiffusion()
{
    for (auto &system : sciantix_system)
    {
        switch (int(input_variable["iDiffusionSolver"].getValue()))
        {
        case 1:
        {
            if (system.getRestructuredMatrix() == 0)
            {
                sciantix_variable[system.getGasName() + " in grain"].setFinalValue(
                    solver.SpectralDiffusion(
                        getDiffusionModes(system.getGasName()),
                        model["Gas diffusion - " + system.getName()].getParameter(),
                        physics_variable["Time step"].getFinalValue()));

                double equilibrium_fraction(1.0);
                if ((system.getResolutionRate() + system.getTrappingRate()) > 0.0)
                    equilibrium_fraction = system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate());

                sciantix_variable[system.getGasName() + " in intragranular solution"].setFinalValue(
                    equilibrium_fraction * sciantix_variable[system.getGasName() + " in grain"].getFinalValue());

                sciantix_variable[system.getGasName() + " in intragranular bubbles"].setFinalValue(
                    (1.0 - equilibrium_fraction) * sciantix_variable[system.getGasName() + " in grain"].getFinalValue());
            }
            else if (system.getRestructuredMatrix() == 1)
            {
                sciantix_variable[system.getGasName() + " in grain HBS"].setFinalValue(
                    solver.SpectralDiffusion(
                        getDiffusionModes(system.getGasName() + " in HBS"),
                        model["Gas diffusion - " + system.getName()].getParameter(),
                        physics_variable["Time step"].getFinalValue()));
            }
            break;
        }

        case 2:
        {
            double initial_value_solution(0.0), initial_value_bubbles(0.0);

            if (system.getRestructuredMatrix() == 0)
            {
                initial_value_solution = sciantix_variable[system.getGasName() + " in intragranular solution"].getFinalValue();
                initial_value_bubbles = sciantix_variable[system.getGasName() + " in intragranular bubbles"].getFinalValue();

                solver.SpectralDiffusion2equations(
                    initial_value_solution,
                    initial_value_bubbles,
                    getDiffusionModesSolution(system.getGasName()),
                    getDiffusionModesBubbles(system.getGasName()),
                    model["Gas diffusion - " + system.getName()].getParameter(),
                    physics_variable["Time step"].getFinalValue());
                sciantix_variable[system.getGasName() + " in intragranular solution"].setFinalValue(initial_value_solution);
                sciantix_variable[system.getGasName() + " in intragranular bubbles"].setFinalValue(initial_value_bubbles);
                sciantix_variable[system.getGasName() + " in grain"].setFinalValue(initial_value_solution + initial_value_bubbles);
            }
            else if (system.getRestructuredMatrix() == 1)
            {
                sciantix_variable[system.getGasName() + " in grain HBS"].setFinalValue(0.0);
            }
            break;
        }

        case 3:
            break;

        default:
            ErrorMessages::Switch("Simulation.h", "iDiffusionSolver", int(input_variable["iDiffusionSolver"].getValue()));
            break;
        }
    }

    if (int(input_variable["iDiffusionSolver"].getValue()) == 3)
    {
        double initial_value_solution(0.0), initial_value_bubbles(0.0), initial_value_hbs(0.0);

        initial_value_solution = sciantix_variable["Xe in intragranular solution"].getFinalValue();
        initial_value_bubbles = sciantix_variable["Xe in intragranular bubbles"].getFinalValue();
        initial_value_hbs = sciantix_variable["Xe in grain HBS"].getFinalValue();

        solver.SpectralDiffusion3equations(
            initial_value_solution,
            initial_value_bubbles,
            initial_value_hbs,
            getDiffusionModesSolution("Xe"),
            getDiffusionModesBubbles("Xe"),
            getDiffusionModes("Xe in HBS"),
            model["Gas diffusion - Xe in UO2 with HBS"].getParameter(),
            physics_variable["Time step"].getFinalValue());
        sciantix_variable["Xe in grain"].setFinalValue(initial_value_solution + initial_value_bubbles);
        sciantix_variable["Xe in intragranular solution"].setFinalValue(initial_value_solution);
        sciantix_variable["Xe in intragranular bubbles"].setFinalValue(initial_value_bubbles);
        sciantix_variable["Xe in grain HBS"].setFinalValue(initial_value_hbs);

        sciantix_variable["Intragranular gas solution swelling"].setFinalValue(
            (sciantix_variable["Xe in intragranular solution"].getFinalValue() + sciantix_variable["Xe in grain HBS"].getFinalValue()) *
            pow(matrices["UO2"].getLatticeParameter(), 3) / 4);
    }

    // Calculation of the gas concentration arrived at the grain boundary, by mass balance.
    for (auto &system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(
                sciantix_variable[system.getGasName() + " produced"].getFinalValue() -
                sciantix_variable[system.getGasName() + " decayed"].getFinalValue() -
                sciantix_variable[system.getGasName() + " in grain"].getFinalValue() -
                sciantix_variable[system.getGasName() + " released"].getInitialValue());

            if (sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() < 0.0)
                sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(0.0);
        }
    }

    /**
     * @brief If **iGrainBoundaryBehaviour = 0** (e.g., grain-boundary calculations are neglected),
     * all the gas arriving at the grain boundary is released.
     *
     */
    if (input_variable["iGrainBoundaryBehaviour"].getValue() == 0)
    {
        for (auto &system : sciantix_system)
        {
            if (system.getRestructuredMatrix() == 0)
            {
                {
                    sciantix_variable[system.getGasName() + " at grain boundary"].setInitialValue(0.0);
                    sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(0.0);

                    sciantix_variable[system.getGasName() + " released"].setFinalValue(
                        sciantix_variable[system.getGasName() + " produced"].getFinalValue() -
                        sciantix_variable[system.getGasName() + " decayed"].getFinalValue() -
                        sciantix_variable[system.getGasName() + " in grain"].getFinalValue());
                }
            }
        }
    }
}

void Simulation::GrainGrowth()
{
    sciantix_variable["Grain radius"].setFinalValue(
        solver.QuarticEquation(model["Grain growth"].getParameter()));

    matrices["UO2"].setGrainRadius(sciantix_variable["Grain radius"].getFinalValue());
}

void Simulation::IntraGranularBubbleBehaviour()
{

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
        if (gas[system.getGasName()].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
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

void Simulation::InterGranularBubbleBehaviour()
{
    // Vacancy concentration
    sciantix_variable["Intergranular vacancies per bubble"].setFinalValue(
        solver.LimitedGrowth(sciantix_variable["Intergranular vacancies per bubble"].getInitialValue(),
                                model["Intergranular bubble evolution"].getParameter(),
                                physics_variable["Time step"].getFinalValue()));

    // Grain-boundary bubble volume
    double vol(0);
    for (auto &system : sciantix_system)
    {
        if (gas[system.getGasName()].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            vol += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue() *
                    gas[system.getGasName()].getVanDerWaalsVolume();
        }
    }
    vol += sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * matrices["UO2"].getSchottkyVolume();
    sciantix_variable["Intergranular bubble volume"].setFinalValue(vol);

    // Grain-boundary bubble radius
    sciantix_variable["Intergranular bubble radius"].setFinalValue(
        0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (matrices["UO2"].getLenticularShapeFactor()), 1. / 3.));

    // Grain-boundary bubble area
    sciantix_variable["Intergranular bubble area"].setFinalValue(
        M_PI * pow(sciantix_variable["Intergranular bubble radius"].getFinalValue() * sin(matrices["UO2"].getSemidihedralAngle()), 2));

    // Grain-boundary bubble coalescence
    double dbubble_area = sciantix_variable["Intergranular bubble area"].getIncrement();
    sciantix_variable["Intergranular bubble concentration"].setFinalValue(
        solver.BinaryInteraction(sciantix_variable["Intergranular bubble concentration"].getInitialValue(), 2.0, dbubble_area));

    // Conservation
    for (auto &system : sciantix_system)
    {
        if (gas[system.getGasName()].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].rescaleFinalValue(
                sciantix_variable["Intergranular bubble concentration"].getInitialValue() / sciantix_variable["Intergranular bubble concentration"].getFinalValue());
        }
    }

    double n_at(0);
    for (auto &system : sciantix_system)
    {
        if (gas[system.getGasName()].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
    }
    sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

    sciantix_variable["Intergranular vacancies per bubble"].rescaleFinalValue(
        sciantix_variable["Intergranular bubble concentration"].getInitialValue() / sciantix_variable["Intergranular bubble concentration"].getFinalValue());

    vol = 0.0;
    for (auto &system : sciantix_system)
    {
        if (gas[system.getGasName()].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
            vol += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue() *
                    gas[system.getGasName()].getVanDerWaalsVolume();
    }
    vol += sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * matrices["UO2"].getSchottkyVolume();
    sciantix_variable["Intergranular bubble volume"].setFinalValue(vol);

    sciantix_variable["Intergranular bubble radius"].setFinalValue(
        0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (matrices["UO2"].getLenticularShapeFactor()), 1. / 3.));

    sciantix_variable["Intergranular bubble area"].setFinalValue(
        M_PI * pow(sciantix_variable["Intergranular bubble radius"].getFinalValue() * sin(matrices["UO2"].getSemidihedralAngle()), 2));

    // Fractional coverage
    sciantix_variable["Intergranular fractional coverage"].setFinalValue(
        sciantix_variable["Intergranular bubble area"].getFinalValue() *
        sciantix_variable["Intergranular bubble concentration"].getFinalValue());

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
            sciantix_variable["Intergranular saturation fractional coverage"].getFinalValue() /
            sciantix_variable["Intergranular fractional coverage"].getFinalValue());
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
            if (gas[system.getGasName()].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].rescaleFinalValue(pow(similarity_ratio, 1.5));
        }

        n_at = 0.0;
        for (auto &system : sciantix_system)
        {
            if (gas[system.getGasName()].getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
        }
        sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

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
                sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue());

            if (sciantix_variable[system.getGasName() + " released"].getFinalValue() < 0.0)
                sciantix_variable[system.getGasName() + " released"].setFinalValue(0.0);
        }
    }

    // Intergranular gaseous swelling
    sciantix_variable["Intergranular gas swelling"].setFinalValue(
        3 / sciantix_variable["Grain radius"].getFinalValue() *
        sciantix_variable["Intergranular bubble concentration"].getFinalValue() *
        sciantix_variable["Intergranular bubble volume"].getFinalValue());
}

void Simulation::GrainBoundarySweeping()
{
    // dC / df = - C

    if (!input_variable["Grain-boundary sweeping"].getValue())
        return;

    // intra-granular gas diffusion modes
    switch (int(input_variable["iDiffusionSolver"].getValue()))
    {
    case 1:
    {
        for (int i = 0; i < modes_initial_conditions.size(); ++i)
        {
            modes_initial_conditions[6 * 40 + i] =
                solver.Decay(
                    modes_initial_conditions[6 * 40 + i],
                    1.0,
                    0.0,
                    model["Grain-boundary sweeping"].getParameter().at(0));
        }

        break;
    }

    case 2:
    {
        for (int i = 0; i < modes_initial_conditions.size(); ++i)
        {
            modes_initial_conditions[7 * 40 + i] =
                solver.Decay(
                    modes_initial_conditions[7 * 40 + i],
                    1.0,
                    0.0,
                    model["Grain-boundary sweeping"].getParameter().at(0));

            modes_initial_conditions[8 * 40 + i] =
                solver.Decay(
                    modes_initial_conditions[8 * 40 + i],
                    1.0,
                    0.0,
                    model["Grain-boundary sweeping"].getParameter().at(0));
        }

        break;
    }

    case 3:
        break;

    default:
        // ErrorMessages::Switch("Simulation.h", "iDiffusionSolver", int(input_variable["iDiffusionSolver"].getValue()));
        break;
    }
}

void Simulation::GrainBoundaryMicroCracking()
{
    if (!input_variable["iGrainBoundaryMicroCracking"].getValue())
        return;

    // ODE for the intergranular fractional intactness: this equation accounts for the reduction of the intergranular fractional intactness following a temperature transient
    // df / dT = - dm/dT f
    sciantix_variable["Intergranular fractional intactness"].setFinalValue(
        solver.Decay(sciantix_variable["Intergranular fractional intactness"].getInitialValue(),
                        model["Grain-boundary micro-cracking"].getParameter().at(0), // 1st parameter = microcracking parameter
                        0.0,
                        history_variable["Temperature"].getIncrement()));

    // ODE for the intergranular fractional coverage: this equation accounts for the reduction of the intergranular fractional coverage following a temperature transient
    // dFc / dT = - ( dm/dT f) Fc
    sciantix_variable["Intergranular fractional coverage"].setFinalValue(
        solver.Decay(sciantix_variable["Intergranular fractional coverage"].getInitialValue(),
                        model["Grain-boundary micro-cracking"].getParameter().at(0) * sciantix_variable["Intergranular fractional intactness"].getFinalValue(),
                        0.0,
                        history_variable["Temperature"].getIncrement()));

    // ODE for the saturation fractional coverage: this equation accounts for the reduction of the intergranular saturation fractional coverage following a temperature transient
    // dFcsat / dT = - (dm/dT f) Fcsat
    sciantix_variable["Intergranular saturation fractional coverage"].setFinalValue(
        solver.Decay(
            sciantix_variable["Intergranular saturation fractional coverage"].getInitialValue(),
            model["Grain-boundary micro-cracking"].getParameter().at(0) * sciantix_variable["Intergranular fractional intactness"].getFinalValue(),
            0.0,
            history_variable["Temperature"].getIncrement()));

    // ODE for the intergranular fractional intactness: this equation accounts for the healing of the intergranular fractional intactness with burnup
    // df / dBu = - h f + h
    sciantix_variable["Intergranular fractional intactness"].setFinalValue(
        solver.Decay(
            sciantix_variable["Intergranular fractional intactness"].getFinalValue(),
            model["Grain-boundary micro-cracking"].getParameter().at(1), // 2nd parameter = healing parameter
            model["Grain-boundary micro-cracking"].getParameter().at(1),
            sciantix_variable["Burnup"].getIncrement()));

    // ODE for the saturation fractional coverage: this equation accounts for the healing of the intergranular saturation fractional coverage with burnup
    // dFcsat / dBu = h (1-f) Fcsat
    sciantix_variable["Intergranular saturation fractional coverage"].setFinalValue(
        solver.Decay(
            sciantix_variable["Intergranular saturation fractional coverage"].getFinalValue(),
            -model["Grain-boundary micro-cracking"].getParameter().at(1) * (1.0 - sciantix_variable["Intergranular fractional intactness"].getFinalValue()),
            0.0,
            sciantix_variable["Burnup"].getIncrement()));

    // Re-scaling: to maintain the current fractional coverage unchanged
    double similarity_ratio;

    if (sciantix_variable["Intergranular fractional coverage"].getInitialValue() > 0.0)
        similarity_ratio = sqrt(
            sciantix_variable["Intergranular fractional coverage"].getFinalValue() / sciantix_variable["Intergranular fractional coverage"].getInitialValue());
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
                sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue());

            if (sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() < 0.0)
                sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(0.0);
        }
    }
}

void Simulation::GrainBoundaryVenting()
{

    if (!int(input_variable["iGrainBoundaryVenting"].getValue()))
        return;

    for (auto &system : sciantix_system)
    {
        sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(
            solver.Integrator(
                sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue(),
                -model["Grain-boundary venting"].getParameter().at(0),
                sciantix_variable[system.getGasName() + " at grain boundary"].getIncrement()));
        sciantix_variable[system.getGasName() + " at grain boundary"].resetValue();
    }
}

void Simulation::HighBurnupStructureFormation()
{
    if (!int(input_variable["iHighBurnupStructureFormation"].getValue()))
        return;

    // Restructuring rate:
    // dalpha_r / bu = 3.54 * 2.77e-7 (1-alpha_r) b^2.54
    double coefficient =
        model["High-burnup structure formation"].getParameter().at(0) *
        model["High-burnup structure formation"].getParameter().at(1) *
        pow(sciantix_variable["Effective burnup"].getFinalValue(), 2.54);

    sciantix_variable["Restructured volume fraction"].setFinalValue(
        solver.Decay(
            sciantix_variable["Restructured volume fraction"].getInitialValue(),
            coefficient,
            coefficient,
            sciantix_variable["Effective burnup"].getIncrement()));
}

void Simulation::HighBurnupStructurePorosity()
{
    if (!int(input_variable["iHighBurnupStructurePorosity"].getValue()))
        return;

    // porosity evolution
    sciantix_variable["HBS porosity"].setFinalValue(
        solver.Integrator(
            sciantix_variable["HBS porosity"].getInitialValue(),
            model["High-burnup structure porosity"].getParameter().at(0),
            sciantix_variable["Burnup"].getIncrement()));

    if (sciantix_variable["HBS porosity"].getFinalValue() > 0.15)
        sciantix_variable["HBS porosity"].setFinalValue(0.15);

    // evolution of pore number density via pore nucleation and re-solution
    if (sciantix_variable["HBS porosity"].getFinalValue())
        sciantix_variable["HBS pore density"].setFinalValue(
            solver.Decay(
                sciantix_variable["HBS pore density"].getInitialValue(),
                matrices["UO2HBS"].getPoreResolutionRate(),
                matrices["UO2HBS"].getPoreNucleationRate(),
                physics_variable["Time step"].getFinalValue()));
    else
        sciantix_variable["HBS pore density"].setFinalValue(0.0);

    // calculation of pore volume based on porosity and pore number density
    if (sciantix_variable["HBS pore density"].getFinalValue())
        sciantix_variable["HBS pore volume"].setFinalValue(
            sciantix_variable["HBS porosity"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue());

    sciantix_variable["HBS pore radius"].setFinalValue(0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), (1.0 / 3.0)));

    // update of number density of HBS pores: interconnection by impingement
    double limiting_factor =
        (2.0 - sciantix_variable["HBS porosity"].getFinalValue()) /
        (2.0 * pow(1.0 - sciantix_variable["HBS porosity"].getFinalValue(), 3.0));

    double pore_interconnection_rate = 4.0 * limiting_factor;
    sciantix_variable["HBS pore density"].setFinalValue(
        solver.BinaryInteraction(
            sciantix_variable["HBS pore density"].getFinalValue(),
            pore_interconnection_rate,
            sciantix_variable["HBS pore volume"].getIncrement()));

    // update of pore volume and pore radius after interconnection by impingement
    if (sciantix_variable["HBS pore density"].getFinalValue())
        sciantix_variable["HBS pore volume"].setFinalValue(
            sciantix_variable["HBS porosity"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue());

    sciantix_variable["HBS pore radius"].setFinalValue(0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), (1.0 / 3.0)));

    // average (at/m^3) of gas atoms in HBS pores
    sciantix_variable["Xe in HBS pores"].setFinalValue(
        solver.Integrator(
            sciantix_variable["Xe in HBS pores"].getInitialValue(),

            2.0 * matrices["UO2HBS"].getPoreNucleationRate() +
                sciantix_variable["HBS pore density"].getFinalValue() *
                    (matrices["UO2HBS"].getPoreTrappingRate() - matrices["UO2HBS"].getPoreResolutionRate()),

            physics_variable["Time step"].getFinalValue()));

    if (sciantix_variable["HBS pore density"].getFinalValue())
        sciantix_variable["Xe atoms per HBS pore"].setFinalValue(
            sciantix_variable["Xe in HBS pores"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue());

    sciantix_variable["Xe in HBS pores - variance"].setFinalValue(
        solver.Integrator(
            sciantix_variable["Xe in HBS pores - variance"].getInitialValue(),

            matrices["UO2"].getPoreTrappingRate() * sciantix_variable["HBS pore density"].getFinalValue() -
                matrices["UO2"].getPoreResolutionRate() * sciantix_variable["HBS pore density"].getFinalValue() +
                matrices["UO2"].getPoreNucleationRate() * pow((sciantix_variable["Xe atoms per HBS pore"].getFinalValue() - 2.0), 2.0),

            physics_variable["Time step"].getFinalValue()));

    if (sciantix_variable["HBS pore density"].getFinalValue())
        sciantix_variable["Xe atoms per HBS pore - variance"].setFinalValue(
            sciantix_variable["Xe in HBS pores - variance"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue());
}

void Simulation::StoichiometryDeviation()
{
    if (!input_variable["iStoichiometryDeviation"].getValue())
        return;

    if (history_variable["Temperature"].getFinalValue() < 1000.0)
    {
        sciantix_variable["Stoichiometry deviation"].setConstant();
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(0.0);
    }

    else if (input_variable["iStoichiometryDeviation"].getValue() < 5)
    {
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            solver.Decay(
                sciantix_variable["Stoichiometry deviation"].getInitialValue(),
                model["Stoichiometry deviation"].getParameter().at(0),
                model["Stoichiometry deviation"].getParameter().at(1),
                physics_variable["Time step"].getFinalValue()));
    }

    else if (input_variable["iStoichiometryDeviation"].getValue() > 4)
    {
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            solver.NewtonLangmuirBasedModel(
                sciantix_variable["Stoichiometry deviation"].getInitialValue(),
                model["Stoichiometry deviation"].getParameter(),
                physics_variable["Time step"].getFinalValue()));
    }

    sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(
        BlackburnThermochemicalModel(
            sciantix_variable["Stoichiometry deviation"].getFinalValue(),
            history_variable["Temperature"].getFinalValue()));
}

void Simulation::UO2Thermochemistry()
{
    if (!input_variable["iStoichiometryDeviation"].getValue())
        return;

    if (history_variable["Temperature"].getFinalValue() < 1000.0 || sciantix_variable["Gap oxygen partial pressure"].getFinalValue() == 0)
        sciantix_variable["Equilibrium stoichiometry deviation"].setFinalValue(0.0);

    else
        sciantix_variable["Equilibrium stoichiometry deviation"].setFinalValue(
            solver.NewtonBlackburn(
                model["UO2 thermochemistry"].getParameter()));
}

