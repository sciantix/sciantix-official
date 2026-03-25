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
//  Authors: D. Pizzocri, G. Zullo, G. Nicodemo                                     //
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
    double JOG_thickness_cs2moo4 = 0.0;

    for (auto& variable : thermochemistry_variable)
    {
        if (variable.getLocation() != "at grain boundary")
            continue;

        const std::string phase = variable.getPhase();
        if (phase != "condensed" && phase != "liquid" && phase != "ionic_liquid")
            continue;

        const std::map<std::string, int> stoichiometry = variable.getStoichiometry();
        const bool contains_uranium = stoichiometry.count("U") > 0;
        const bool oxygen_only_species =
            !stoichiometry.empty() &&
            std::all_of(
                stoichiometry.begin(),
                stoichiometry.end(),
                [](const std::pair<const std::string, int>& entry) { return entry.first == "O"; });

        if (contains_uranium || oxygen_only_species)
            continue;

        double contribution = std::max(0.0, variable.getFinalValue() * variable.getMolarMass() / theoretical_density);

        JOG_thickness += contribution;

        if (phase == "condensed")
            JOG_thickness_condensed += contribution;
        else
            JOG_thickness_liquid += contribution;

        if (variable.getName() == "CS2MOO4_S1 (condensed, at grain boundary)" || variable.getName() == "CS2MOO4_S2 (condensed, at grain boundary)")
            JOG_thickness_cs2moo4 += contribution;
    }

    sciantix_variable["JOG"].setFinalValue(JOG_thickness);
    sciantix_variable["JOG from condensed"].setFinalValue(JOG_thickness_condensed);
    sciantix_variable["JOG from liquid"].setFinalValue(JOG_thickness_liquid);
    sciantix_variable["JOG from Cs2MoO4"].setFinalValue(JOG_thickness_cs2moo4);

    std::cout << " JOG Formation -------------------------------------------------------" << std::endl;
    std::cout << "  FIMA [perc] = " << sciantix_variable["FIMA"].getFinalValue() << std::endl;
    std::cout << "  Temperature [C] = " << temperature_celsius << std::endl;
    std::cout << "  Orthorhombic reference a [m] = " << a_o_ref << std::endl;
    std::cout << "  Orthorhombic reference b [m] = " << b_o_ref << std::endl;
    std::cout << "  Orthorhombic reference c [m] = " << c_o_ref << std::endl;
    std::cout << "  Thermal expansion [-] = " << alpha << std::endl;
    std::cout << "  Cell volume [m3] = " << V_cell << std::endl;
    std::cout << "  Theoretical density [g/m3] = " << theoretical_density << std::endl;
    std::cout << "  JOG thickness from condensed species [m3 compound / m3 fuel] = " << JOG_thickness_condensed << std::endl;
    std::cout << "  JOG thickness from liquid species [m3 compound / m3 fuel] = " << JOG_thickness_liquid << std::endl;
    std::cout << "  JOG thickness from Cs2MoO4 [m3 compound / m3 fuel] = " << JOG_thickness_cs2moo4 << std::endl;
    std::cout << "  Total JOG thickness - volume [m3 compound / m3 fuel] = " << JOG_thickness << std::endl;
}
