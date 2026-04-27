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

#include "IntragranularDiffusion.h"

void Simulation::IntragranularDiffusion() // qui tutti i gas e i volatili, cerca di unificare. no i metallici!
{
    // Model declaration
    switch (static_cast<int>(input_variable["iDiffusionSolver"].getValue()))
    {
        case 1:
            defineSpectralDiffusion1Equation(sciantix_system, model, n_modes);
            break;

        case 2:
            defineSpectralDiffusion2Equations(sciantix_system, model, n_modes);
            break;

        case 3:
            defineSpectralDiffusion3Equations(sciantix_system, model, sciantix_variable, physics_variable, n_modes);
            break;

        default:
            errorHandling(input_variable);
            break;
    }

    // Model resolution
    for (auto& system : sciantix_system)
    {
        if (!system.isGasOrVolatileFP())
            continue;

        switch (int(input_variable["iDiffusionSolver"].getValue()))
        {
            case 1:
            {
                if (system.getRestructuredMatrix() == 0)
                {
                    sciantix_variable[system.getFissionProductName() + " in grain"].setFinalValue(
                        solver.SpectralDiffusion(getDiffusionModes(system.getFissionProductName()),
                                                 model["Intragranular diffusion - " + system.getName()].getParameter(),
                                                 physics_variable["Time step"].getFinalValue()));

                    double equilibrium_fraction(1.0);
                    if ((system.getResolutionRate() + system.getTrappingRate()) > 0.0)
                        equilibrium_fraction =
                            system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate());

                    sciantix_variable[system.getFissionProductName() + " in intragranular solution"].setFinalValue(
                        equilibrium_fraction * sciantix_variable[system.getFissionProductName() + " in grain"].getFinalValue());

                    sciantix_variable[system.getFissionProductName() + " in intragranular bubbles"].setFinalValue(
                        (1.0 - equilibrium_fraction) *
                        sciantix_variable[system.getFissionProductName() + " in grain"].getFinalValue());
                }

                else if (system.getRestructuredMatrix() == 1)
                {
                    sciantix_variable[system.getFissionProductName() + " in grain HBS"].setFinalValue(
                        solver.SpectralDiffusion(getDiffusionModes(system.getFissionProductName() + " in HBS"),
                                                 model["Intragranular diffusion - " + system.getName()].getParameter(),
                                                 physics_variable["Time step"].getFinalValue()));
                }

                break;
            }

            case 2:
            {
                double initial_value_solution(0.0), initial_value_bubbles(0.0);

                if (system.getRestructuredMatrix() == 0)
                {
                    initial_value_solution =
                        sciantix_variable[system.getFissionProductName() + " in intragranular solution"].getFinalValue();
                    initial_value_bubbles =
                        sciantix_variable[system.getFissionProductName() + " in intragranular bubbles"].getFinalValue();

                    solver.SpectralDiffusion2equations(initial_value_solution,
                                                       initial_value_bubbles,
                                                       getDiffusionModesSolution(system.getFissionProductName()),
                                                       getDiffusionModesBubbles(system.getFissionProductName()),
                                                       model["Intragranular diffusion - " + system.getName()].getParameter(),
                                                       physics_variable["Time step"].getFinalValue());

                    sciantix_variable[system.getFissionProductName() + " in intragranular solution"].setFinalValue(
                        initial_value_solution);
                    sciantix_variable[system.getFissionProductName() + " in intragranular bubbles"].setFinalValue(
                        initial_value_bubbles);
                    sciantix_variable[system.getFissionProductName() + " in grain"].setFinalValue(initial_value_solution +
                                                                                       initial_value_bubbles);
                }
                else if (system.getRestructuredMatrix() == 1)
                {
                    sciantix_variable[system.getFissionProductName() + " in grain HBS"].setFinalValue(0.0);
                }
                break;
            }

            case 3:
                break;

            default:
                ErrorMessages::Switch(__FILE__, "iDiffusionSolver", int(input_variable["iDiffusionSolver"].getValue()));
                break;
        }
    }

    if (int(input_variable["iDiffusionSolver"].getValue()) == 3)
    {
        double initial_value_solution(0.0), initial_value_bubbles(0.0), initial_value_hbs(0.0);

        initial_value_solution = sciantix_variable["Xe in intragranular solution"].getFinalValue();
        initial_value_bubbles  = sciantix_variable["Xe in intragranular bubbles"].getFinalValue();
        initial_value_hbs      = sciantix_variable["Xe in grain HBS"].getFinalValue();

        solver.SpectralDiffusion3equations(initial_value_solution,
                                           initial_value_bubbles,
                                           initial_value_hbs,
                                           getDiffusionModesSolution("Xe"),
                                           getDiffusionModesBubbles("Xe"),
                                           getDiffusionModes("Xe in HBS"),
                                           model["Intragranular diffusion - Xe in UO2 with HBS"].getParameter(),
                                           physics_variable["Time step"].getFinalValue());

        sciantix_variable["Xe in grain"].setFinalValue(initial_value_solution + initial_value_bubbles);
        sciantix_variable["Xe in intragranular solution"].setFinalValue(initial_value_solution);
        sciantix_variable["Xe in intragranular bubbles"].setFinalValue(initial_value_bubbles);
        sciantix_variable["Xe in grain HBS"].setFinalValue(initial_value_hbs);

        sciantix_variable["Intragranular gas solution swelling"].setFinalValue(
            (sciantix_variable["Xe in intragranular solution"].getFinalValue() +
             sciantix_variable["Xe in grain HBS"].getFinalValue()) *
             // CODE DEVELOPMENT : GENERALIZATION FROM UO2 TO ALL MATRICES
            pow(matrices["UO2"].getLatticeParameter(), 3) / 4);
    }

    // Calculation of the fission product concentration at grain boundary, by mass balance
    for (auto& system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0 && system.isGasFP())
        {
            sciantix_variable[system.getFissionProductName() + " at grain boundary"].setFinalValue(
                sciantix_variable[system.getFissionProductName() + " produced"].getFinalValue() -
                sciantix_variable[system.getFissionProductName() + " decayed"].getFinalValue() -
                sciantix_variable[system.getFissionProductName() + " in grain"].getFinalValue() -
                sciantix_variable[system.getFissionProductName() + " released"].getInitialValue());

            if (sciantix_variable[system.getFissionProductName() + " at grain boundary"].getFinalValue() < 0.0)
                sciantix_variable[system.getFissionProductName() + " at grain boundary"].setFinalValue(0.0);
        }
        
        if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
        {            
            sciantix_variable[system.getFissionProductName() + " reacted - GB"].setFinalValue(
                sciantix_variable[system.getFissionProductName() + " produced"].getFinalValue() -
                sciantix_variable[system.getFissionProductName() + " decayed"].getFinalValue() -
                sciantix_variable[system.getFissionProductName() + " at grain boundary"].getFinalValue() -
                sciantix_variable[system.getFissionProductName() + " in grain"].getFinalValue() -
                sciantix_variable[system.getFissionProductName() + " released"].getInitialValue());

            
            if (sciantix_variable[system.getFissionProductName() + " reacted - GB"].getFinalValue() < 0.0)
                sciantix_variable[system.getFissionProductName() + " reacted - GB"].setFinalValue(0.0);

        }
        // 
    }

    /**
     * @brief If **iGrainBoundaryBehaviour = 0** (e.g., no grain-boundary bubbles),
     * fission products at grain boundary are immediately released.
     *
     */
    if (input_variable["iGrainBoundaryBehaviour"].getValue() == 0)
    {
        for (auto& system : sciantix_system)
        {
            if (system.getRestructuredMatrix() == 0 && system.isGasFP())
            {
                {
                    sciantix_variable[system.getFissionProductName() + " at grain boundary"].setInitialValue(0.0);
                    sciantix_variable[system.getFissionProductName() + " at grain boundary"].setFinalValue(0.0);

                    sciantix_variable[system.getFissionProductName() + " released"].setFinalValue(
                        sciantix_variable[system.getFissionProductName() + " produced"].getFinalValue() -
                        sciantix_variable[system.getFissionProductName() + " decayed"].getFinalValue() -
                        sciantix_variable[system.getFissionProductName() + " in grain"].getFinalValue()
                    );
                }
            }

            if (system.getRestructuredMatrix() == 0 && system.isVolatileFP())
            {
                {
                    sciantix_variable[system.getFissionProductName() + " at grain boundary"].setInitialValue(0.0);
                    sciantix_variable[system.getFissionProductName() + " at grain boundary"].setFinalValue(0.0);

                    sciantix_variable[system.getFissionProductName() + " released"].setFinalValue(
                        sciantix_variable[system.getFissionProductName() + " produced"].getFinalValue() -
                        sciantix_variable[system.getFissionProductName() + " decayed"].getFinalValue() -
                        sciantix_variable[system.getFissionProductName() + " in grain"].getFinalValue()
                    );
                }
            }
            //
        }
    }
}

void defineSpectralDiffusion1Equation(SciantixArray<System>& sciantix_system, SciantixArray<Model>& model, int n_modes)
{
    std::string reference;

    for (auto& system : sciantix_system)
    {
        if (!system.isGasOrVolatileFP())
            continue;

        Model model_;
        model_.setName("Intragranular diffusion - " + system.getName());
        model_.setRef(reference);

        std::vector<double> parameters;
        parameters.push_back(n_modes);
        double FPsDiffusivity;
        if (system.getResolutionRate() + system.getTrappingRate() == 0)
            FPsDiffusivity = system.getFissionProductDiffusivity() * system.getFissionProduct().getPrecursorFactor();
        else
            FPsDiffusivity = (system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate())) *
                                 system.getFissionProductDiffusivity() * system.getFissionProduct().getPrecursorFactor() +
                             (system.getTrappingRate() / (system.getResolutionRate() + system.getTrappingRate())) *
                                 system.getBubbleDiffusivity();

        parameters.push_back(FPsDiffusivity);
        parameters.push_back(system.getMatrix().getGrainRadius());
        parameters.push_back(system.getProductionRate());
        parameters.push_back(system.getFissionProduct().getDecayRate());

        model_.setParameter(parameters);
        model.push(model_);
    }
}

void defineSpectralDiffusion2Equations(SciantixArray<System>& sciantix_system, SciantixArray<Model>& model, int n_modes)
{
    std::string reference;

    for (auto& system : sciantix_system)
    {
        if (!system.isGasOrVolatileFP())
            continue;

        Model model_;
        model_.setName("Intragranular diffusion - " + system.getName());
        model_.setRef(reference);

        std::vector<double> parameters;

        parameters.push_back(n_modes);
        parameters.push_back(system.getFissionProductDiffusivity() * system.getFissionProduct().getPrecursorFactor());
        parameters.push_back(system.getBubbleDiffusivity());
        parameters.push_back(system.getMatrix().getGrainRadius());
        parameters.push_back(system.getProductionRate());
        parameters.push_back(0.0);
        parameters.push_back(system.getResolutionRate());
        parameters.push_back(system.getTrappingRate());
        parameters.push_back(system.getFissionProduct().getDecayRate());

        model_.setParameter(parameters);
        model.push(model_);
    }
}

void defineSpectralDiffusion3Equations(SciantixArray<System>&          sciantix_system,
                                       SciantixArray<Model>&           model,
                                       SciantixArray<SciantixVariable> sciantix_variable,
                                       SciantixArray<SciantixVariable> physics_variable,
                                       int                             n_modes)
{
    std::string reference;

    Model model_;
    model_.setName("Intragranular diffusion - Xe in UO2 with HBS");
    model_.setRef(reference);

    std::vector<double> parameters;

    parameters.push_back(n_modes);

    System xe_in_uo2_(sciantix_system["Xe in UO2"]);
    System xe_in_uo2hbs_(sciantix_system["Xe in UO2HBS"]);

    parameters.push_back(xe_in_uo2_.getFissionProduct().getPrecursorFactor() * xe_in_uo2_.getFissionProductDiffusivity() /
                         (pow(xe_in_uo2_.getMatrix().getGrainRadius(), 2)));
    parameters.push_back(0.0);
    parameters.push_back(xe_in_uo2hbs_.getFissionProductDiffusivity() /
                         (pow(xe_in_uo2hbs_.getMatrix().getGrainRadius(), 2)));

    parameters.push_back(1.0);

    parameters.push_back(xe_in_uo2_.getProductionRate());
    parameters.push_back(0.0);
    parameters.push_back(xe_in_uo2hbs_.getProductionRate());

    parameters.push_back(xe_in_uo2_.getResolutionRate());
    parameters.push_back(xe_in_uo2_.getTrappingRate());
    parameters.push_back(xe_in_uo2_.getFissionProduct().getDecayRate());

    double sweeping_term(0.0);
    if (physics_variable["Time step"].getFinalValue())
        sweeping_term = 1. / (1. - sciantix_variable["Restructured volume fraction"].getFinalValue()) *
                        sciantix_variable["Restructured volume fraction"].getIncrement() /
                        physics_variable["Time step"].getFinalValue();

    if (std::isinf(sweeping_term) || std::isnan(sweeping_term))
        sweeping_term = 0.0;

    parameters.push_back(sweeping_term);

    model_.setParameter(parameters);
    model.push(model_);
}

void errorHandling(SciantixArray<InputVariable> input_variable)
{
    ErrorMessages::Switch(__FILE__, "iDiffusionSolver", static_cast<int>(input_variable["iDiffusionSolver"].getValue()));
}
