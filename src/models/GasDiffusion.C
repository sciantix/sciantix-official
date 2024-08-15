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

#include "Simulation.h"
#include "GasDiffusion.h"

void Simulation::GasDiffusion()
{
    switch (static_cast<int>(input_variable[iv["iDiffusionSolver"]].getValue()))
    {
        case 1:
            defineSpectralDiffusion1Equation();
            break;

        case 2:
            defineSpectralDiffusion2Equations();
            break;

        case 3:
            defineSpectralDiffusion3Equations();
            break;

        default:
            errorHandling();
            break;
    }
}

void defineSpectralDiffusion1Equation()
{
    // Model declaration
    std::string reference;

    for (auto& system : sciantix_system)
	{
		model.emplace_back();
		int modelIndex = static_cast<int>(model.size()) - 1;
		model[modelIndex].setName("Gas diffusion - " + system.getName());
		model[modelIndex].setRef(reference);

		std::vector<double> parameters;
		parameters.push_back(n_modes);
		double gasDiffusivity;
		if (system.getResolutionRate() + system.getTrappingRate() == 0)
		{
			gasDiffusivity = system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor();
		}
		else
		{
			gasDiffusivity = (system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate())) * system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor() +
							 (system.getTrappingRate() / (system.getResolutionRate() + system.getTrappingRate())) * system.getBubbleDiffusivity();
		}
		parameters.push_back(gasDiffusivity);
		parameters.push_back(matrix[sma[system.getMatrixName()]].getGrainRadius());
		parameters.push_back(system.getProductionRate());
		parameters.push_back(gas[ga[system.getGasName()]].getDecayRate());

		model[modelIndex].setParameter(parameters);
	}
    
    // Model mapping
    MapModel();
    
    // Model resolution
    for (auto& system : sciantix_system)
    {
        if (system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[sv[system.getGasName() + " in grain"]].setFinalValue(
                solver.SpectralDiffusion(
                    getDiffusionModes(system.getGasName()),
                    model[sm["Gas diffusion - " + system.getName()]].getParameter(),
                    physics_variable[pv["Time step"]].getFinalValue()
                )
            );

            double equilibrium_fraction(1.0);
            if ((system.getResolutionRate() + system.getTrappingRate()) > 0.0)
                equilibrium_fraction = system.getResolutionRate() / (system.getResolutionRate() + system.getTrappingRate());

            sciantix_variable[sv[system.getGasName() + " in intragranular solution"]].setFinalValue(
                equilibrium_fraction * sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
            );

            sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].setFinalValue(
                (1.0 - equilibrium_fraction) * sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
            );
        }
        else if (system.getRestructuredMatrix() == 1)
        {
            sciantix_variable[sv[system.getGasName() + " in grain HBS"]].setFinalValue(
                solver.SpectralDiffusion(
                    getDiffusionModes(system.getGasName() + " in HBS"),
                    model[sm["Gas diffusion - " + system.getName()]].getParameter(),
                    physics_variable[pv["Time step"]].getFinalValue()
                )
            );
        }
    }

    // Calculation of the gas concentration arrived at the grain boundary, by mass balance.
    for (auto& system : sciantix_system)
    {
        if(system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(
                sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " released"]].getInitialValue()
            );

            if (sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
                sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);
        }
    }

        /**
         * @brief If **iGrainBoundaryBehaviour = 0** (e.g., grain-boundary calculations are neglected), 
         * all the gas arriving at the grain boundary is released.
         * 
         */
        if (input_variable[iv["iGrainBoundaryBehaviour"]].getValue() == 0)
        {
            for (auto& system : sciantix_system)
            {
                if(system.getRestructuredMatrix() == 0)
                {
                    {
                        sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setInitialValue(0.0);
                        sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);

                        sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
                            sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                            sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                            sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
                        );
                    }
                }
            }
        }


    // Calculation of the gas concentration arrived at the grain boundary, by mass balance.
    for (auto& system : sciantix_system)
    {
        if(system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(
                sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " released"]].getInitialValue()
            );

            if (sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
                sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);
        }
    }

    /**
     * @brief If **iGrainBoundaryBehaviour = 0** (e.g., grain-boundary calculations are neglected), 
     * all the gas arriving at the grain boundary is released.
     * 
     */
    if (input_variable[iv["iGrainBoundaryBehaviour"]].getValue() == 0)
    {
        for (auto& system : sciantix_system)
        {
            if(system.getRestructuredMatrix() == 0)
            {
                {
                    sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setInitialValue(0.0);
                    sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);

                    sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
                        sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                        sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                        sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
                    );
                }
            }
        }
    }
}

void defineSpectralDiffusion2Equations()
{
    std::string reference;

    for (auto& system : sciantix_system)
    {
        model.emplace_back();
        int modelIndex = static_cast<int>(model.size()) - 1;
        model[modelIndex].setName("Gas diffusion - " + system.getName());
        model[modelIndex].setRef(reference);

        std::vector<double> parameters;

        parameters.push_back(n_modes);

        parameters.push_back(system.getFissionGasDiffusivity() * gas[ga[system.getGasName()]].getPrecursorFactor());
        parameters.push_back(system.getBubbleDiffusivity());

        parameters.push_back(matrix[sma[system.getMatrixName()]].getGrainRadius());

        parameters.push_back(system.getProductionRate());
        parameters.push_back(0.0);
        
        parameters.push_back(system.getResolutionRate());
        parameters.push_back(system.getTrappingRate());
        parameters.push_back(gas[ga[system.getGasName()]].getDecayRate());

        model[modelIndex].setParameter(parameters);
    }

    MapModel();

    for (auto& system : sciantix_system)
    {
        double initial_value_solution(0.0), initial_value_bubbles(0.0);

        if (system.getRestructuredMatrix() == 0)
        {
            initial_value_solution = sciantix_variable[sv[system.getGasName() + " in intragranular solution"]].getFinalValue();
            initial_value_bubbles  = sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].getFinalValue();

            solver.SpectralDiffusion2equations(
                initial_value_solution,
                initial_value_bubbles,
                getDiffusionModesSolution(system.getGasName()),
                getDiffusionModesBubbles(system.getGasName()),
                model[sm["Gas diffusion - " + system.getName()]].getParameter(),
                physics_variable[pv["Time step"]].getFinalValue()
            );
            sciantix_variable[sv[system.getGasName() + " in intragranular solution"]].setFinalValue(initial_value_solution);
            sciantix_variable[sv[system.getGasName() + " in intragranular bubbles"]].setFinalValue(initial_value_bubbles);
            sciantix_variable[sv[system.getGasName() + " in grain"]].setFinalValue(initial_value_solution + initial_value_bubbles);
        }
        else if (system.getRestructuredMatrix() == 1)
        {
            sciantix_variable[sv[system.getGasName() + " in grain HBS"]].setFinalValue(0.0);
        }
    }

    // Calculation of the gas concentration arrived at the grain boundary, by mass balance.
    for (auto& system : sciantix_system)
    {
        if(system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(
                sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " released"]].getInitialValue()
            );

            if (sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
                sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);
        }
    }

        /**
         * @brief If **iGrainBoundaryBehaviour = 0** (e.g., grain-boundary calculations are neglected), 
         * all the gas arriving at the grain boundary is released.
         * 
         */
        if (input_variable[iv["iGrainBoundaryBehaviour"]].getValue() == 0)
        {
            for (auto& system : sciantix_system)
            {
                if(system.getRestructuredMatrix() == 0)
                {
                    {
                        sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setInitialValue(0.0);
                        sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);

                        sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
                            sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                            sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                            sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
                        );
                    }
                }
            }
        }
}

void defineSpectralDiffusion3Equations()
{
    // Model declaration
    std::string reference;

    model.emplace_back();
    int modelIndex = static_cast<int>(model.size()) - 1;
    model[modelIndex].setName("Gas diffusion - Xe in UO2 with HBS");
    model[modelIndex].setRef(reference);

    std::vector<double> parameters;

    parameters.push_back(n_modes);

    parameters.push_back(gas[ga["Xe"]].getPrecursorFactor() * sciantix_system[sy["Xe in UO2"]].getFissionGasDiffusivity() / (pow(matrix[sma["UO2"]].getGrainRadius(),2)));
    parameters.push_back(0.0);
    parameters.push_back(sciantix_system[sy["Xe in UO2HBS"]].getFissionGasDiffusivity() / (pow(matrix[sma["UO2HBS"]].getGrainRadius(),2)));
    
    parameters.push_back(1.0);
    
    parameters.push_back(sciantix_system[sy["Xe in UO2"]].getProductionRate());
    parameters.push_back(0.0);
    parameters.push_back(sciantix_system[sy["Xe in UO2HBS"]].getProductionRate());

    parameters.push_back(sciantix_system[sy["Xe in UO2"]].getResolutionRate());
    parameters.push_back(sciantix_system[sy["Xe in UO2"]].getTrappingRate());
    parameters.push_back(gas[ga["Xe"]].getDecayRate());

    double sweeping_term(0.0);
    if(physics_variable[pv["Time step"]].getFinalValue())
        sweeping_term = 1./(1. - sciantix_variable[sv["Restructured volume fraction"]].getFinalValue()) * sciantix_variable[sv["Restructured volume fraction"]].getIncrement() / physics_variable[pv["Time step"]].getFinalValue();

    if (std::isinf(sweeping_term) || std::isnan(sweeping_term))
        sweeping_term = 0.0;

    parameters.push_back(sweeping_term);

    model[modelIndex].setParameter(parameters);

    // Model mapping
    MapModel();

    // Model resolution
    double initial_value_solution(0.0), initial_value_bubbles(0.0), initial_value_hbs(0.0);

    initial_value_solution = sciantix_variable[sv["Xe in intragranular solution"]].getFinalValue();
    initial_value_bubbles = sciantix_variable[sv["Xe in intragranular bubbles"]].getFinalValue();
    initial_value_hbs  = sciantix_variable[sv["Xe in grain HBS"]].getFinalValue();

    solver.SpectralDiffusion3equations(
        initial_value_solution,
        initial_value_bubbles,
        initial_value_hbs,
        getDiffusionModesSolution("Xe"),
        getDiffusionModesBubbles("Xe"),
        getDiffusionModes("Xe in HBS"),
        model[sm["Gas diffusion - Xe in UO2 with HBS"]].getParameter(),
        physics_variable[pv["Time step"]].getFinalValue()
    );
    sciantix_variable[sv["Xe in grain"]].setFinalValue(initial_value_solution + initial_value_bubbles);
    sciantix_variable[sv["Xe in intragranular solution"]].setFinalValue(initial_value_solution);
    sciantix_variable[sv["Xe in intragranular bubbles"]].setFinalValue(initial_value_bubbles);
    sciantix_variable[sv["Xe in grain HBS"]].setFinalValue(initial_value_hbs);

    sciantix_variable[sv["Intragranular gas solution swelling"]].setFinalValue(
        (sciantix_variable[sv["Xe in intragranular solution"]].getFinalValue() + sciantix_variable[sv["Xe in grain HBS"]].getFinalValue()) *
        pow(matrix[sma["UO2"]].getLatticeParameter(), 3) / 4
    );

    // Calculation of the gas concentration arrived at the grain boundary, by mass balance.
    for (auto& system : sciantix_system)
    {
        if(system.getRestructuredMatrix() == 0)
        {
            sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(
                sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue() -
                sciantix_variable[sv[system.getGasName() + " released"]].getInitialValue()
            );

            if (sciantix_variable[sv[system.getGasName() + " at grain boundary"]].getFinalValue() < 0.0)
                sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);
        }
    }

    /**
     * @brief If **iGrainBoundaryBehaviour = 0** (e.g., grain-boundary calculations are neglected), 
     * all the gas arriving at the grain boundary is released.
     * 
     */
    if (input_variable[iv["iGrainBoundaryBehaviour"]].getValue() == 0)
    {
        for (auto& system : sciantix_system)
        {
            if(system.getRestructuredMatrix() == 0)
            {
                {
                    sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setInitialValue(0.0);
                    sciantix_variable[sv[system.getGasName() + " at grain boundary"]].setFinalValue(0.0);

                    sciantix_variable[sv[system.getGasName() + " released"]].setFinalValue(
                        sciantix_variable[sv[system.getGasName() + " produced"]].getFinalValue() -
                        sciantix_variable[sv[system.getGasName() + " decayed"]].getFinalValue() -
                        sciantix_variable[sv[system.getGasName() + " in grain"]].getFinalValue()
                    );
                }
            }
        }
    }
}


/**
 * @brief This method returns a pointer to the array of diffusion modes corresponding to the specified gas.
 * 
 * @param gas_name The name of the gas for which diffusion modes are required.
 * @return A pointer to the array of diffusion modes for the specified gas.
 *         Returns nullptr if the gas name is invalid.
 */
double* getDiffusionModes(std::string gas_name)
{
    if(gas_name == "Xe")
        return &modes_initial_conditions[0];
    else if(gas_name == "Kr")
        return &modes_initial_conditions[3 * 40];
    else if(gas_name == "He")
        return &modes_initial_conditions[6 * 40];
    else if(gas_name == "Xe133")
        return &modes_initial_conditions[9 * 40];

    else if (gas_name == "Kr85m")
        return &modes_initial_conditions[12 * 40];
        
    else if (gas_name == "Xe in HBS")
        return &modes_initial_conditions[15 * 40];

    else
    {
        std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModes." << std::endl;
        return nullptr;
    }
}

double* getDiffusionModesSolution(std::string gas_name)
{	
    if(gas_name == "Xe")
        return &modes_initial_conditions[1 * 40];

    else if(gas_name == "Kr")
        return &modes_initial_conditions[4 * 40];

    else if(gas_name == "He")
        return &modes_initial_conditions[7 * 40];

    else if(gas_name == "Xe133")
        return &modes_initial_conditions[10 * 40];

    else if (gas_name == "Kr85m")
        return &modes_initial_conditions[13 * 40];
        
    else if (gas_name == "Xe in HBS")
        return &modes_initial_conditions[16 * 40];
    else
    {
        std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModesSolution." << std::endl;
        return nullptr;
    }
}

double* getDiffusionModesBubbles(std::string gas_name)
{	
    if(gas_name == "Xe")
        return &modes_initial_conditions[2 * 40];

    else if(gas_name == "Kr")
        return &modes_initial_conditions[5 * 40];

    else if(gas_name == "He")
        return &modes_initial_conditions[8 * 40];

    else if(gas_name == "Xe133")
        return &modes_initial_conditions[11 * 40];

    else if (gas_name == "Kr85m")
        return &modes_initial_conditions[14 * 40];

    else
    {
        std::cerr << "Error: Invalid gas name \"" << gas_name << "\" in Simulation::getDiffusionModesBubbles." << std::endl;
        return nullptr;
    }
}

void errorHandling()
{
    ErrorMessages::Switch(__FILE__, "iDiffusionSolver", static_cast<int>(input_variable[iv["iDiffusionSolver"]].getValue()));
}
