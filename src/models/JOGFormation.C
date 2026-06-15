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
#include "Constants.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <map>
#include <string>

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

    sciantix_variable["Phase std density"].setFinalValue(theoretical_density);
    double JOG_Cs2MoO4 = 0.0;
    double JOG_BaMoO4 = 0.0;

    double total_mo_moles = 0.0;
    double oxide_mo_moles = 0.0;
    double oxide_mo_valence_sum = 0.0;
    double total_ba_moles = 0.0;
    double oxide_ba_moles = 0.0;
    double oxide_ba_valence_sum = 0.0;

    auto normalizeElementName = [](std::string element)
    {
        if (element.empty())
            return element;

        std::transform(element.begin(), element.end(), element.begin(), [](unsigned char c) {
            return static_cast<char>(std::tolower(c));
        });
        element[0] = static_cast<char>(std::toupper(static_cast<unsigned char>(element[0])));
        return element;
    };

    auto getElementAmount = [&normalizeElementName](const std::map<std::string, double>& composition,
                                                    const std::string& element)
    {
        double amount = 0.0;
        for (const auto& entry : composition)
        {
            if (normalizeElementName(entry.first) == element)
                amount += entry.second;
        }
        return amount;
    };

    auto molarMass = [&normalizeElementName](const std::map<std::string, double>& composition)
    {
        static const std::map<std::string, double> atomic_masses = {
            {"Cs", 132.90545196},
            {"Ba", 137.327},
            {"Mo", 95.95},
            {"O", 15.999},
            {"Pd", 106.42},
            {"Rh", 102.9055},
            {"Ru", 101.07},
            {"Tc", 98.9063},
        };

        double value = 0.0;
        for (const auto& entry : composition)
        {
            const auto atomic_mass = atomic_masses.find(normalizeElementName(entry.first));
            if (atomic_mass == atomic_masses.end())
                return 0.0;

            value += entry.second * atomic_mass->second;
        }
        return value;
    };

    auto effectiveMoValence = [&getElementAmount](const std::map<std::string, double>& composition)
    {
        const double mo = getElementAmount(composition, "Mo");
        if (mo <= 0.0)
            return 0.0;

        const double oxygen = getElementAmount(composition, "O");
        const double monovalent_cations = getElementAmount(composition, "Cs");
        const double divalent_cations = getElementAmount(composition, "Ba");

        // Charge-balance estimate with O=-2, Cs=+1 and Ba=+2.
        // For Cs2MoO4 and BaMoO4 this gives Mo=+6; for MoO2 it gives Mo=+4.
        const double valence =
            (2.0 * oxygen - monovalent_cations - 2.0 * divalent_cations) / mo;

        return std::max(0.0, std::min(6.0, valence));
    };


    auto accumulateOxidePhase = [&](const std::map<std::string, double>& composition,
                                    double phase_molar_mass,
                                    double mass,
                                    double thickness)
    {
        const double mo_stoichiometry = getElementAmount(composition, "Mo");
        if (mo_stoichiometry > 0.0 && phase_molar_mass > 0.0)
        {
            const double mo_moles = mass * mo_stoichiometry / phase_molar_mass;
            oxide_mo_moles += mo_moles;
            oxide_mo_valence_sum += mo_moles * effectiveMoValence(composition);
        }

        const double ba_stoichiometry = getElementAmount(composition, "Ba");
        if (ba_stoichiometry > 0.0 && phase_molar_mass > 0.0)
        {
            const double ba_moles = mass * ba_stoichiometry / phase_molar_mass;
            oxide_ba_moles += ba_moles;
            // Valence fixed to + 2
            oxide_ba_valence_sum += ba_moles * 2;
        }
    };

    for (auto& variable : thermochemistry_variable)
    {
        if (variable.getLocation() != "at grain boundary")
            continue;

        if (variable.getFinalValue() <= 0.0)
            continue;

        const std::string phase = variable.getPhase();
        if (phase != "condensed" && phase != "liquid" && phase != "ionic_liquid")
            continue;

        const std::string variable_name = variable.getName();
        if ((phase == "liquid" || phase == "ionic_liquid") && variable_name.rfind("LIQUID (", 0) != 0)
            continue;

        const std::map<std::string, double> composition = variable.getComposition();
        const double phase_molar_mass = molarMass(composition);
        const double mo_stoichiometry = getElementAmount(composition, "Mo");
        if (mo_stoichiometry > 0.0 && phase_molar_mass > 0.0)
        {
            const double mo_moles = variable.getMass() * mo_stoichiometry / phase_molar_mass;
            total_mo_moles += mo_moles;
        }

        const double ba_stoichiometry = getElementAmount(composition, "Ba");
        if (ba_stoichiometry > 0.0 && phase_molar_mass > 0.0)
        {
            const double ba_moles = variable.getMass() * ba_stoichiometry / phase_molar_mass;
            total_ba_moles += ba_moles;
        }

        const double mass = variable.getMass();
        double contribution = mass / theoretical_density;
            

        if (getElementAmount(composition, "O") > 0.0)
            accumulateOxidePhase(composition, phase_molar_mass, mass, contribution);

        if (variable_name == "CS2MOO4_S1 (condensed, at grain boundary)")
            JOG_Cs2MoO4 += contribution;
        else if (variable_name == "CS2MOO4_S2 (condensed, at grain boundary)")
            JOG_Cs2MoO4 += contribution;
        else if (variable_name == "BAMOO4 (condensed, at grain boundary)")
            JOG_BaMoO4 += contribution;
        else if (variable_name == "HCP_A3 (condensed, at grain boundary)")
        {

            auto getElementFraction = [&composition](const std::string& standard_name,
                                                      const std::string& uppercase_name,
                                                      const std::string& lowercase_name)
            {
                const auto standard = composition.find(standard_name);
                if (standard != composition.end())
                    return standard->second;

                const auto uppercase = composition.find(uppercase_name);
                if (uppercase != composition.end())
                    return uppercase->second;

                const auto lowercase = composition.find(lowercase_name);
                if (lowercase != composition.end())
                    return lowercase->second;

                return 0.0;
            };

            const double mo = getElementFraction("Mo", "MO", "mo");
            const double ru = getElementFraction("Ru", "RU", "ru");
            if (ru > 0.0)
                sciantix_variable["Mo/Ru in HCP_A3"].setFinalValue(mo / ru);
            else
                sciantix_variable["Mo/Ru in HCP_A3"].setFinalValue(0.0);
        }
    }

    sciantix_variable["JOG (Cs2MoO4)"].setFinalValue(JOG_Cs2MoO4);
    sciantix_variable["JOG (BaMoO4)"].setFinalValue(JOG_BaMoO4);
    sciantix_variable["Mo in oxide fraction"].setFinalValue(
        total_mo_moles > 0.0 ? oxide_mo_moles / total_mo_moles : 0.0);
    sciantix_variable["Mo oxide valence"].setFinalValue(
        oxide_mo_moles > 0.0 ? oxide_mo_valence_sum / oxide_mo_moles : 0.0);
    sciantix_variable["Ba/Mo in oxide compounds"].setFinalValue(
        oxide_mo_moles > 0.0 ? oxide_ba_moles / oxide_mo_moles : 0.0);
    sciantix_variable["Ba in oxide fraction"].setFinalValue(
        total_ba_moles > 0.0 ? oxide_ba_moles / total_ba_moles : 0.0);
    sciantix_variable["Ba oxide valence"].setFinalValue(
        oxide_ba_moles > 0.0 ? oxide_ba_valence_sum / oxide_ba_moles : 0.0);
}
