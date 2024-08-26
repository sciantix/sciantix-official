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

void Simulation::HighBurnupStructurePorosity()
{
	Model model_;

	model_.setName("High-burnup structure porosity");
	double porosity_increment = 0.0;

	std::string reference;
	std::vector<double> parameter;

	switch (int(input_variable["iHighBurnupStructurePorosity"].getValue()))
	{
	case 0:
	{
		/// @brief
		/// No HBS case - no evolution of HBS porosity

		reference += "not considered";
		parameter.push_back(0.0);
		sciantix_variable["HBS porosity"].setInitialValue(0.0);
		sciantix_variable["HBS porosity"].setFinalValue(0.0);
		break;
	}

	case 1:
	{

		/// @brief
		/// Correlation for the HBS porosity evolution based on Spino et al. 2006 data

		double rate_coefficient = 1.3e-3;
		double porosity_upper_threshold = 0.15;
		double burnup_threshold = 50.0;

		if (sciantix_variable["HBS porosity"].getInitialValue() < porosity_upper_threshold)
		{
			if (sciantix_variable["Burnup"].getFinalValue() < burnup_threshold)
				porosity_increment = 0.0;
			else
				porosity_increment = rate_coefficient;
		}

		else
		{
			sciantix_variable["HBS porosity"].setInitialValue(0.15);
			porosity_increment = 0.0;
		}

		reference = ": Based on Spino et al. 2006 data";

		parameter.push_back(porosity_increment);

		break;
	}

	default:
		ErrorMessages::Switch(__FILE__, "HighBurnupStructurePorosity", int(input_variable["HighBurnupStructurePorosity"].getValue()));
		break;
	}

	model_.setParameter(parameter);
	model_.setRef(reference);

	model.push(model_);




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