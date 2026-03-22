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
#include <iomanip>
#include <vector>

namespace
{
struct JOGSpeciesContribution
{
    std::string name;
    std::string phase;
    double concentration = 0.0;
    double molar_mass = 0.0;
    double density = 0.0;
    double thickness = 0.0;
    bool   uses_estimated_density = false;
};
}

void Simulation::JOGFormation()
{
    if (input_variable["iThermochimica"].getValue() == 0) return;

    const std::string cs2moo4_s1_name = "CS2MOO4_S1 (condensed, at grain boundary)";
    const std::string cs2moo4_s2_name = "CS2MOO4_S2 (condensed, at grain boundary)";
    if (!thermochemistry_variable.isElementPresent(cs2moo4_s1_name))
        return;

    const double temperature = history_variable["Temperature"].getFinalValue();
    const double temperature_celsius = temperature - 273.15;

    const double MM_Cs2MoO4 = thermochemistry_variable[cs2moo4_s1_name].getMolarMass();
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

    const double reference_density = theoretical_density_o;

    double JOG_thickness = 0.0;
    double JOG_thickness_condensed = 0.0;
    double JOG_thickness_liquid = 0.0;
    double JOG_thickness_known_density = 0.0;
    double JOG_thickness_estimated_density = 0.0;
    double JOG_thickness_cs2moo4 = 0.0;
    std::vector<JOGSpeciesContribution> contributions;

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

        const double concentration = variable.getFinalValue();
        if (concentration <= 0.0)
            continue;

        const std::string variable_name = variable.getName();
        const double molar_mass = variable.getMolarMass();

        double density = reference_density;
        bool uses_estimated_density = true;

        if (variable_name == cs2moo4_s1_name)
        {
            density = theoretical_density_o;
            uses_estimated_density = false;
        }
        else if (variable_name == cs2moo4_s2_name)
        {
            density = theoretical_density_h;
            uses_estimated_density = false;
        }

        if (density <= 0.0)
            continue;

        const double contribution = concentration * molar_mass / density;
        if (contribution <= 0.0)
            continue;

        JOGSpeciesContribution species_contribution;
        species_contribution.name = variable_name;
        species_contribution.phase = phase;
        species_contribution.concentration = concentration;
        species_contribution.molar_mass = molar_mass;
        species_contribution.density = density;
        species_contribution.thickness = contribution;
        species_contribution.uses_estimated_density = uses_estimated_density;
        contributions.push_back(species_contribution);

        JOG_thickness += contribution;

        if (phase == "condensed")
            JOG_thickness_condensed += contribution;
        else
            JOG_thickness_liquid += contribution;

        if (uses_estimated_density)
            JOG_thickness_estimated_density += contribution;
        else
            JOG_thickness_known_density += contribution;

        if (variable_name == cs2moo4_s1_name || variable_name == cs2moo4_s2_name)
            JOG_thickness_cs2moo4 += contribution;
    }

    sciantix_variable["JOG"].setFinalValue(JOG_thickness);
    sciantix_variable["JOG from condensed"].setFinalValue(JOG_thickness_condensed);
    sciantix_variable["JOG from liquid"].setFinalValue(JOG_thickness_liquid);
    sciantix_variable["JOG from known densities"].setFinalValue(JOG_thickness_known_density);
    sciantix_variable["JOG from estimated densities"].setFinalValue(JOG_thickness_estimated_density);
    sciantix_variable["JOG from Cs2MoO4"].setFinalValue(JOG_thickness_cs2moo4);

    std::sort(
        contributions.begin(),
        contributions.end(),
        [](const JOGSpeciesContribution& left, const JOGSpeciesContribution& right)
        {
            return left.thickness > right.thickness;
        });

    std::cout << std::scientific << std::setprecision(8);
    std::cout << "JOGFormation variables:" << std::endl;
    std::cout << "  FIMA [perc] = " << sciantix_variable["FIMA"].getFinalValue() << std::endl;
    std::cout << "  Temperature [K] = " << temperature << std::endl;
    std::cout << "  Temperature [C] = " << temperature_celsius << std::endl;
    std::cout << "  Grain radius [m] = " << sciantix_variable["Grain radius"].getFinalValue() << std::endl;
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
    std::cout << "  Reference density for non-Cs2MoO4 species [g/m3] = " << reference_density << std::endl;
    std::cout << "  JOG thickness from condensed species [m3 compound / m3 fuel] = " << JOG_thickness_condensed << std::endl;
    std::cout << "  JOG thickness from liquid species [m3 compound / m3 fuel] = " << JOG_thickness_liquid << std::endl;
    std::cout << "  JOG thickness from known densities [m3 compound / m3 fuel] = " << JOG_thickness_known_density << std::endl;
    std::cout << "  JOG thickness from estimated densities [m3 compound / m3 fuel] = " << JOG_thickness_estimated_density << std::endl;
    std::cout << "  JOG thickness from Cs2MoO4 [m3 compound / m3 fuel] = " << JOG_thickness_cs2moo4 << std::endl;
    std::cout << "  Total JOG thickness - volume [m3 ompound / m3 fuel] = " << JOG_thickness << std::endl;

    const size_t number_of_reported_species = std::min<size_t>(5, contributions.size());
    for (size_t index = 0; index < number_of_reported_species; ++index)
    {
        const JOGSpeciesContribution& contribution = contributions[index];
        std::cout << "  JOG contributor " << index + 1 << ": "
                  << contribution.name
                  << " | phase=" << contribution.phase
                  << " | contribution=" << contribution.thickness
                  << " | density=" << contribution.density;

        if (contribution.uses_estimated_density)
            std::cout << " | estimated_density";
        else
            std::cout << " | dedicated_density";

        std::cout << std::endl;
    }
}
