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
//  Version: under development                                                      //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E.Cappellari                                    //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

#include <algorithm>

void Simulation::JOGFormation()
{
    if (input_variable["iThermochimica"].getValue() == 0) return;

    const double temperature_celsius = history_variable["Temperature"].getFinalValue() - 273.15;
    
    // Data on Cs2MoO4 from Wallez et al., Journal of Solid State Chemistry 215 (2014) 225-230.

    // Reference unit-cell parameters
    const double a_o_ref = 0.8477e-9; // m
    const double b_o_ref = 0.6840e-9; // m
    const double c_o_ref = 2 * 2 * b_o_ref; // m

    // Polynomial fits are relative linear expansions versus temperature in degree Celsius.
    double alpha = -7.12e-4 + 2.57e-5 * temperature_celsius + 4.03e-8 * std::pow(temperature_celsius, 2.0);
    if (temperature_celsius > 568) // Transition temperature
        alpha = -0.0102 + 8.50e-5 * temperature_celsius - 2.13e-8 * std::pow(temperature_celsius, 2.0);

    const double V_cell = (a_o_ref * b_o_ref * c_o_ref)*(1.0 + 3 * alpha)/ (4.0); // 4.0 = Z in orthorombic

    const double theoretical_density = 425.76 / (avogadro_number * V_cell); // g/m3

    double JOG_thickness = 0.0;
    double JOG_thickness_condensed = 0.0;
    double JOG_thickness_liquid = 0.0;

    for (auto& variable : thermochemistry_variable)
    {
        if (variable.getLocation() != "at grain boundary")
            continue;

        const std::string phase = variable.getPhase();
        if (phase != "condensed" && phase != "liquid" && phase != "ionic_liquid")
            continue;

        const std::string variable_name = variable.getName();
        if ((phase == "liquid" || phase == "ionic_liquid") &&
            variable_name.rfind("LIQUID (", 0) != 0)
        {
            continue;
        }

        double contribution = std::max(0.0, variable.getFinalValue() * variable.getMolarMass() / theoretical_density);

        JOG_thickness += contribution;

        if (phase == "condensed")
            JOG_thickness_condensed += contribution;
        else
            JOG_thickness_liquid += contribution;

        if (variable_name == "CS2MOO4_S1 (condensed, at grain boundary)")
            sciantix_variable["JOG from CS2MOO4_S1"].setFinalValue(contribution);
        else if (variable_name == "CS2MOO4_S2 (condensed, at grain boundary)")
            sciantix_variable["JOG from CS2MOO4_S2"].setFinalValue(contribution);
        else if (variable_name == "MOO2 (condensed, at grain boundary)")
            sciantix_variable["JOG from MOO2"].setFinalValue(contribution);
        else if (variable_name == "CS2MO3O10 (condensed, at grain boundary)")
            sciantix_variable["JOG from CS2MO3O10"].setFinalValue(contribution);
        else if (variable_name == "CS2MO4O13 (condensed, at grain boundary)")
            sciantix_variable["JOG from CS2MO4O13"].setFinalValue(contribution);
        else if (variable_name == "BCC_A2 (condensed, at grain boundary)")
            sciantix_variable["JOG from BCC_A2"].setFinalValue(contribution);
        else if (variable_name == "FCC_A1 (condensed, at grain boundary)")
            sciantix_variable["JOG from FCC_A1"].setFinalValue(contribution);
        else if (variable_name == "HCP_A3 (condensed, at grain boundary)")
            sciantix_variable["JOG from HCP_A3"].setFinalValue(contribution);
    }

    sciantix_variable["JOG"].setFinalValue(JOG_thickness);
    sciantix_variable["JOG from condensed"].setFinalValue(JOG_thickness_condensed);
    sciantix_variable["JOG from liquid"].setFinalValue(JOG_thickness_liquid);
}
