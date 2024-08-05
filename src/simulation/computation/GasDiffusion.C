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
//////////////////////////////////////////////////////////////////////////////////////

#include "GasDiffusion.h"

void Simulation::GasDiffusion()
{
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

            std::cout << sciantix_variable[system.getGasName() + " produced"].getFinalValue() << ", " <<
                sciantix_variable[system.getGasName() + " decayed"].getFinalValue() << ", " << 
                sciantix_variable[system.getGasName() + " in grain"].getFinalValue() << ", " <<
                sciantix_variable[system.getGasName() + " released"].getInitialValue() << std::endl;

            if (sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() < 0.0)
                sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(0.0);
            
            std::cout << "Gas diff : " << sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() << std::endl;
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
                        
                    std::cout << "RELEASED diff : " << sciantix_variable[system.getGasName() + " released"].getFinalValue() << std::endl;
                }
            }
        }
    }
}

void defineSpectralDiffusion1Equation(SciantixArray<System> &sciantix_system, SciantixArray<Model> &model, int n_modes)
{
	std::string reference;

 
    for (auto& system : sciantix_system)
	{
		Model new_model;
		new_model.setName("Gas diffusion - " + system.getName());
		new_model.setRef(reference);

		std::vector<double> parameters;
		parameters.push_back(n_modes);
		double gasDiffusivity;
		if (system.getResolutionRate() + system.getTrappingRate() == 0)
		{
			gasDiffusivity = system.getFissionGasDiffusivity() * system.getGas().getPrecursorFactor();
		}
		else
		{
			gasDiffusivity = (system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate())) * system.getFissionGasDiffusivity() * system.getGas().getPrecursorFactor() +
							 (system.getTrappingRate() / (system.getResolutionRate() + system.getTrappingRate())) * system.getBubbleDiffusivity();
		}
		parameters.push_back(gasDiffusivity);
		parameters.push_back(system.getMatrix().getGrainRadius());
		parameters.push_back(system.getProductionRate());
		parameters.push_back(system.getGas().getDecayRate());



		new_model.setParameter(parameters);

		model.push(new_model);
	}
}

void defineSpectralDiffusion2Equations(SciantixArray<System> &sciantix_system, SciantixArray<Model> &model, int n_modes)
{
	std::string reference;

    for (auto& system : sciantix_system)
	{
		Model new_model;
		new_model.setName("Gas diffusion - " + system.getName());
		new_model.setRef(reference);

		std::vector<double> parameters;

		parameters.push_back(n_modes);

		parameters.push_back(system.getFissionGasDiffusivity() *system.getGas().getPrecursorFactor());
		parameters.push_back(system.getBubbleDiffusivity());

		parameters.push_back(system.getMatrix().getGrainRadius());

		parameters.push_back(system.getProductionRate());
		parameters.push_back(0.0);
		
		parameters.push_back(system.getResolutionRate());
		parameters.push_back(system.getTrappingRate());
		parameters.push_back(system.getGas().getDecayRate());

		new_model.setParameter(parameters);
		model.push(new_model);
	}
}

void defineSpectralDiffusion3Equations(SciantixArray<System> &sciantix_system, SciantixArray<Model> &model, 
	SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> physics_variable, int n_modes)
{
	std::string reference;

	Model new_model;
	new_model.setName("Gas diffusion - Xe in UO2 with HBS");
	new_model.setRef(reference);

	std::vector<double> parameters;

	parameters.push_back(n_modes);

	System xe_uo2_system(sciantix_system["Xe in UO2"]);
	System xe_uo2hbs_system(sciantix_system["Xe in UO2HBS"]);

	parameters.push_back(xe_uo2_system.getGas().getPrecursorFactor() * xe_uo2_system.getFissionGasDiffusivity() / (pow(xe_uo2_system.getMatrix().getGrainRadius(),2)));
	parameters.push_back(0.0);
	parameters.push_back(xe_uo2hbs_system.getFissionGasDiffusivity() / (pow(xe_uo2hbs_system.getMatrix().getGrainRadius(),2)));
	
	parameters.push_back(1.0);
	
	parameters.push_back(xe_uo2_system.getProductionRate());
	parameters.push_back(0.0);
	parameters.push_back(xe_uo2hbs_system.getProductionRate());

	parameters.push_back(xe_uo2_system.getResolutionRate());
	parameters.push_back(xe_uo2_system.getTrappingRate());
	parameters.push_back(xe_uo2_system.getGas().getDecayRate());

	double sweeping_term(0.0);
	if(physics_variable["Time step"].getFinalValue())
		sweeping_term = 1./(1. - sciantix_variable["Restructured volume fraction"].getFinalValue()) * sciantix_variable["Restructured volume fraction"].getIncrement() / physics_variable["Time step"].getFinalValue();

	if (std::isinf(sweeping_term) || std::isnan(sweeping_term))
		sweeping_term = 0.0;

	// exchange 1 --> 3
	parameters.push_back(sweeping_term);

	new_model.setParameter(parameters);
	model.push(new_model);
}

void errorHandling(SciantixArray<InputVariable> input_variable)
{
	ErrorMessages::Switch(__FILE__, "iDiffusionSolver", static_cast<int>(input_variable["iDiffusionSolver"].getValue()));
}
