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

void Simulation::JOGFormation()
{
    if (input_variable["iThermochimica"].getValue() == 0) return;

    const double temperature = history_variable["Temperature"].getFinalValue();
    const double temperature_celsius = temperature - 273.15;

    const double C_Cs2MoO4_s1 = thermochemistry_variable["CS2MOO4_S1 (condensed, at grain boundary)"].getFinalValue();
    const double C_Cs2MoO4_s2 = thermochemistry_variable["CS2MOO4_S2 (condensed, at grain boundary)"].getFinalValue();
    const double MM_Cs2MoO4 = thermochemistry_variable["CS2MOO4_S1 (condensed, at grain boundary)"].getMolarMass();
    const double fuel_radius = 5.42e-3; // m, pellet radius assumption used in the JOG estimate.

    // Data on Cs2MoO4 from Wallez et al., Journal of Solid State Chemistry 215 (2014) 225-230.

    // Reference unit-cell parameters (to be checked) are kept explicit here so the intermediate values can be checked.
    // https://materials.springer.com/isp/crystallographic/docs/sd_1501582 
    const double a_o_ref = 0.8499e-9; // m
    const double b_o_ref = 0.6551e-9; // m
    const double c_o_ref = 1.1586e-9; // m

    // Polynomial fits are relative linear expansions versus temperature in degree Celsius.
    const double alpha_o = -7.12e-4 + 2.57e-5 * temperature_celsius + 4.03e-8 * std::pow(temperature_celsius, 2.0);
    const double alpha_h = -0.0102 + 8.50e-5 * temperature_celsius - 2.13e-8 * std::pow(temperature_celsius, 2.0);

    const double lattice_parameter_o = pow(a_o_ref * b_o_ref * c_o_ref, (1.0/3.0));

    // to be checked
    const double Z_o = 4.0;
    const double Z_h = 2.0;

    const double V_cell_o = (a_o_ref * b_o_ref * c_o_ref) * (1.0 + 3 * alpha_o);
    const double V_cell_h = (a_o_ref * b_o_ref * c_o_ref) * (1.0 + 3 * alpha_h);

    const double theoretical_density_o = (MM_Cs2MoO4 / avogadro_number) * (Z_o / V_cell_o); // g/m3
    const double theoretical_density_h = (MM_Cs2MoO4 / avogadro_number) * (Z_h / V_cell_h); // g/m3

    const double JOG_thickness_s1 = C_Cs2MoO4_s1 * fuel_radius * MM_Cs2MoO4 / (2.0 * theoretical_density_o);
    const double JOG_thickness_s2 = C_Cs2MoO4_s2 * fuel_radius * MM_Cs2MoO4 / (2.0 * theoretical_density_h);
    const double JOG_thickness = JOG_thickness_s1 + JOG_thickness_s2;
    sciantix_variable["JOG thickness"].setFinalValue(JOG_thickness*1e6);

    std::cout << std::scientific << std::setprecision(8);
    std::cout << "JOGFormation variables:" << std::endl;
    std::cout << "  FIMA [perc] = " << sciantix_variable["FIMA"].getFinalValue() << std::endl;
    std::cout << "  Temperature [K] = " << temperature << std::endl;
    std::cout << "  Temperature [C] = " << temperature_celsius << std::endl;
    std::cout << "  Grain radius [m] = " << sciantix_variable["Grain radius"].getFinalValue() << std::endl;
    std::cout << "  Fuel radius assumption [m] = " << fuel_radius << std::endl;
    std::cout << "  Cs2MoO4_s1 concentration [mol/m3] = " << C_Cs2MoO4_s1 << std::endl;
    std::cout << "  Cs2MoO4_s2 concentration [mol/m3] = " << C_Cs2MoO4_s2 << std::endl;
    std::cout << "  Molar mass Cs2MoO4 [g/mol] = " << MM_Cs2MoO4 << std::endl;
    std::cout << "  Orthorhombic reference a [m] = " << a_o_ref << std::endl;
    std::cout << "  Orthorhombic reference b [m] = " << b_o_ref << std::endl;
    std::cout << "  Orthorhombic reference c [m] = " << c_o_ref << std::endl;
    std::cout << "  Orthorhombic reference equivalent length [m] = " << lattice_parameter_o << std::endl;
    std::cout << "  Orthorhombic alpha [-] = " << alpha_o << std::endl;
    std::cout << "  Hexagonal alpha [-] = " << alpha_h << std::endl;
    std::cout << "  Orthorhombic Z [-] = " << Z_o << std::endl;
    std::cout << "  Hexagonal Z [-] = " << Z_h << std::endl;
    std::cout << "  Orthorhombic cell volume [m3] = " << V_cell_o << std::endl;
    std::cout << "  Hexagonal cell volume [m3] = " << V_cell_h << std::endl;
    std::cout << "  Orthorhombic theoretical density [g/m3] = " << theoretical_density_o << std::endl;
    std::cout << "  Hexagonal theoretical density [g/m3] = " << theoretical_density_h << std::endl;
    std::cout << "  JOG thickness from s1 [m] = " << JOG_thickness_s1 << std::endl;
    std::cout << "  JOG thickness from s2 [m] = " << JOG_thickness_s2 << std::endl;
    std::cout << "  Total JOG thickness [m] = " << JOG_thickness << std::endl;
}
