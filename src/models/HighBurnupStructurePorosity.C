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

void Simulation::HighBurnupStructurePorosity()
{
    /**
     * @brief This routine sets the model for High burnup structure porosity evolution
     * 
     * @author
     * A. Magni
     * E. Redaelli
     * G. Zullo
    */

    model.emplace_back();
    int model_index = int(model.size()) - 1;
    model[model_index].setName("High-burnup structure porosity");
    double porosity_increment = 0.0;

    std::string reference;
    std::vector<double> parameter;

    switch (int(input_variable[iv["iHighBurnupStructurePorosity"]].getValue()))
    {
    case 0:
    {
        /// @brief 
        /// No HBS case - no evolution of HBS porosity

        reference += "not considered";
        parameter.push_back(0.0);
        sciantix_variable[sv["HBS porosity"]].setInitialValue(0.0);
        sciantix_variable[sv["HBS porosity"]].setFinalValue(0.0);
        break;
    }

    case 1:
    {

        /// @brief 
        /// Correlation for the HBS porosity evolution based on Spino et al. 2006 data

        double rate_coefficient = 1.3e-3;
        double porosity_upper_threshold = 0.15;
        double burnup_threshold = 50.0;

        if (sciantix_variable[sv["HBS porosity"]].getInitialValue() < porosity_upper_threshold)
        {
            if (sciantix_variable[sv["Burnup"]].getFinalValue() < burnup_threshold)
                porosity_increment = 0.0;
            else
                porosity_increment = rate_coefficient;
        }

        else
        {
            sciantix_variable[sv["HBS porosity"]].setInitialValue(0.15);
            porosity_increment = 0.0;
        }

        reference = ": Based on Spino et al. 2006 data";

        parameter.push_back(porosity_increment);

        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "HighBurnupStructurePorosity", int(input_variable[iv["HighBurnupStructurePorosity"]].getValue()));
        break;
    }

    model[model_index].setParameter(parameter);
    model[model_index].setRef(reference);

    MapModel();


    if (!int(input_variable[iv["iHighBurnupStructurePorosity"]].getValue())) return;

    // porosity evolution 
    sciantix_variable[sv["HBS porosity"]].setFinalValue(
        solver.Integrator(
            sciantix_variable[sv["HBS porosity"]].getInitialValue(),
            model[sm["High-burnup structure porosity"]].getParameter().at(0),
            sciantix_variable[sv["Burnup"]].getIncrement()
        )
    );

    if(sciantix_variable[sv["HBS porosity"]].getFinalValue() > 0.15)
        sciantix_variable[sv["HBS porosity"]].setFinalValue(0.15);

    // evolution of pore number density via pore nucleation and re-solution
    if(sciantix_variable[sv["HBS porosity"]].getFinalValue())
        sciantix_variable[sv["HBS pore density"]].setFinalValue(
            solver.Decay(
                    sciantix_variable[sv["HBS pore density"]].getInitialValue(),
                    matrix[sma["UO2HBS"]].getPoreResolutionRate(),
                    matrix[sma["UO2HBS"]].getPoreNucleationRate(),
                    physics_variable[pv["Time step"]].getFinalValue()
                )
            );
    else
        sciantix_variable[sv["HBS pore density"]].setFinalValue(0.0);

    // calculation of pore volume based on porosity and pore number density	
    if(sciantix_variable[sv["HBS pore density"]].getFinalValue())
        sciantix_variable[sv["HBS pore volume"]].setFinalValue(
            sciantix_variable[sv["HBS porosity"]].getFinalValue() / sciantix_variable[sv["HBS pore density"]].getFinalValue());

    sciantix_variable[sv["HBS pore radius"]].setFinalValue(0.620350491 * pow(sciantix_variable[sv["HBS pore volume"]].getFinalValue(), (1.0 / 3.0)));

    // update of number density of HBS pores: interconnection by impingement
    double limiting_factor =
        (2.0 - sciantix_variable[sv["HBS porosity"]].getFinalValue()) /
        (2.0 * pow(1.0 - sciantix_variable[sv["HBS porosity"]].getFinalValue(), 3.0));

    double pore_interconnection_rate = 4.0 * limiting_factor;
    sciantix_variable[sv["HBS pore density"]].setFinalValue(
        solver.BinaryInteraction(
            sciantix_variable[sv["HBS pore density"]].getFinalValue(),
            pore_interconnection_rate,
            sciantix_variable[sv["HBS pore volume"]].getIncrement()
        )
    );
    
    // update of pore volume and pore radius after interconnection by impingement
    if(sciantix_variable[sv["HBS pore density"]].getFinalValue())
        sciantix_variable[sv["HBS pore volume"]].setFinalValue(
            sciantix_variable[sv["HBS porosity"]].getFinalValue() / sciantix_variable[sv["HBS pore density"]].getFinalValue());

    sciantix_variable[sv["HBS pore radius"]].setFinalValue(0.620350491 * pow(sciantix_variable[sv["HBS pore volume"]].getFinalValue(), (1.0 / 3.0)));

    // average (at/m^3) of gas atoms in HBS pores
    sciantix_variable[sv["Xe in HBS pores"]].setFinalValue(
        solver.Integrator(
            sciantix_variable[sv["Xe in HBS pores"]].getInitialValue(),

            2.0 * matrix[sma["UO2HBS"]].getPoreNucleationRate() +
            sciantix_variable[sv["HBS pore density"]].getFinalValue() *
            (matrix[sma["UO2HBS"]].getPoreTrappingRate() - matrix[sma["UO2HBS"]].getPoreResolutionRate()),

            physics_variable[pv["Time step"]].getFinalValue()
        )
    );

    if(sciantix_variable[sv["HBS pore density"]].getFinalValue())
        sciantix_variable[sv["Xe atoms per HBS pore"]].setFinalValue(
        sciantix_variable[sv["Xe in HBS pores"]].getFinalValue() / sciantix_variable[sv["HBS pore density"]].getFinalValue()
    );

    sciantix_variable[sv["Xe in HBS pores - variance"]].setFinalValue(
        solver.Integrator(
            sciantix_variable[sv["Xe in HBS pores - variance"]].getInitialValue(),

            matrix[sma["UO2"]].getPoreTrappingRate() * sciantix_variable[sv["HBS pore density"]].getFinalValue() -
            matrix[sma["UO2"]].getPoreResolutionRate() * sciantix_variable[sv["HBS pore density"]].getFinalValue() +
            matrix[sma["UO2"]].getPoreNucleationRate() * pow((sciantix_variable[sv["Xe atoms per HBS pore"]].getFinalValue()-2.0), 2.0),

            physics_variable[pv["Time step"]].getFinalValue()
        )
    );

    if(sciantix_variable[sv["HBS pore density"]].getFinalValue())
        sciantix_variable[sv["Xe atoms per HBS pore - variance"]].setFinalValue(
            sciantix_variable[sv["Xe in HBS pores - variance"]].getFinalValue() / sciantix_variable[sv["HBS pore density"]].getFinalValue()
        );
}
