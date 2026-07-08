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
#include <vector>
#include <cctype>
#include <cmath>
#include <iostream>
#include <map>
#include <string>

void Simulation::JOGFormation()
{
    const int thermochimica_mode = (int)input_variable["iThermochimica"].getValue();
    if (thermochimica_mode == 0) return;

    // CODE DEVELOPMENT : THERMOCHEMISTRY OUTER-NODE MODE
    const bool is_outer_node = input_variable["iThermochimicaOuterNode"].getValue() != 0;
    if (thermochimica_mode == 1 && !is_outer_node)
    {
        // Simplified fixed-speciation assumption, used on every axial slice
        // other than the outer/last one in TRANSURANUS-SCIANTIX-OC calculations.
        sciantix_variable["Mo in oxide fraction"].setFinalValue(0.6);
        sciantix_variable["Mo oxide valence"].setFinalValue(6.0);
        sciantix_variable["Ba in oxide fraction"].setFinalValue(1.0);
        sciantix_variable["Ba oxide valence"].setFinalValue(2.0);
        return;
    }

    const double temperature_celsius = history_variable["Temperature"].getFinalValue() - 273.15;
    
    // Data on Cs2MoO4 from Wallez et al., Journal of Solid State Chemistry 215 (2014) 225-230.

    // The polynomial fits give the mean relative linear expansion,
    // eps_l = Delta l / l0, as a function of temperature in degree Celsius.

    // Reference density from Wallez et al. at 675 °C:
    // rho = 3.89 g/cm3 = 3890 kg/m3

    // Transition temperature, °C
    const double T_transition = 568.0;

    // Reference state: h-Cs2MoO4 at 675 °C
    const double T_ref = 675.0;
    const double rho_ref = 3.89 ; // g/cm3
    // Relative linear expansion at the reference temperature
    const double eps_ref = -0.0102 + 8.50e-5 * T_ref - 2.13e-8 * std::pow(T_ref, 2.0);

    // Relative linear expansion at current temperature
    double eps_T;

    if (temperature_celsius < T_transition)
        eps_T = - 7.12e-4 + 2.57e-5 * temperature_celsius + 4.03e-8 * std::pow(temperature_celsius, 2.0);
    else
        eps_T = -0.0102 + 8.50e-5 * temperature_celsius - 2.13e-8 * std::pow(temperature_celsius, 2.0);

    // Density from mass conservation:
    // V(T) / V_ref = [(1 + eps_T) / (1 + eps_ref)]^3
    double theoretical_density =
        1e6 * rho_ref * std::pow((1.0 + eps_ref) / (1.0 + eps_T), 3.0); // g/m3

    // Theoretical densities for BaMoO4, Ba2MoO5 and Ba3MoO6 (and any other
    // condensed/liquid grain-boundary phase) are supplied via the optional
    // density column of input_thermochemistry.txt.

    double JOG_Cs2MoO4 = 0.0;
    double JOG_BaMoO4 = 0.0;
    double JOG_Ba3MoO6 = 0.0;
    double JOG_Ba2MoO5 = 0.0;
    double JOG_liquid = 0.0;
    double JOG_other = 0.0;

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

    auto isOxidizedElement = [&getElementAmount](
                                 const std::map<std::string, double>& composition,
                                 const std::string&                   element)
    {
        const double element_stoichiometry = getElementAmount(composition, element);
        if (element_stoichiometry <= 0.0)
            return false;

        // OpenCalphad can leave tiny oxygen site fractions in metallic phases.
        // Treat only meaningful O/element ratios as oxidized inventory.
        const double oxygen_stoichiometry = getElementAmount(composition, "O");
        return oxygen_stoichiometry / element_stoichiometry > 1.0e-8;
    };

    auto isOxidizedMo = [&effectiveMoValence](
                            const std::map<std::string, double>& composition)
    {
        return effectiveMoValence(composition) >= 2.0 - 1.0e-6;
    };

    auto normalizeSublatticeConstituent = [](std::string constituent)
    {
        std::transform(constituent.begin(), constituent.end(), constituent.begin(), [](unsigned char c) {
            return static_cast<char>(std::toupper(c));
        });

        const size_t charge_pos = constituent.find_first_of("+-");
        if (charge_pos != std::string::npos)
            constituent = constituent.substr(0, charge_pos);

        return constituent;
    };

    auto accumulateOxidePhase = [&](const std::map<std::string, double>& composition,
                                    double phase_molar_mass,
                                    double mass)
    {
        const double mo_stoichiometry = getElementAmount(composition, "Mo");
        if (mo_stoichiometry > 0.0 && phase_molar_mass > 0.0 && isOxidizedMo(composition))
        {
            const double mo_moles = mass * mo_stoichiometry / phase_molar_mass;
            oxide_mo_moles += mo_moles;
            oxide_mo_valence_sum += mo_moles * effectiveMoValence(composition);
        }

        const double ba_stoichiometry = getElementAmount(composition, "Ba");
        if (ba_stoichiometry > 0.0 && phase_molar_mass > 0.0 && isOxidizedElement(composition, "Ba"))
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
        const bool is_liquid_phase = phase == "liquid" || phase == "ionic_liquid" || phase == "liquid_ionic";
        if (phase != "condensed" && !is_liquid_phase)
            continue;

        const std::string variable_name = variable.getName();
        const std::map<std::string, double> composition = variable.getComposition();
        const std::map<int, std::map<std::string, double>> sublattice_composition = variable.getSublatticeComposition();
        const double phase_molar_mass = variable.getMolarMass();
        const double mass = variable.getMass();
        const double manifest_density = variable.getTheoreticalDensity();

        const double mo_stoichiometry = getElementAmount(composition, "Mo");
        if (mo_stoichiometry > 0.0 && phase_molar_mass > 0.0)
        {
            const double mo_moles = mass * mo_stoichiometry / phase_molar_mass;
            total_mo_moles += mo_moles;
        }

        const double ba_stoichiometry = getElementAmount(composition, "Ba");
        if (ba_stoichiometry > 0.0 && phase_molar_mass > 0.0)
        {
            const double ba_moles = mass * ba_stoichiometry / phase_molar_mass;
            total_ba_moles += ba_moles;
        }

        accumulateOxidePhase(composition, phase_molar_mass, mass);

        auto hasOxideSecondSublattice =
            [&normalizeSublatticeConstituent](
            const std::map<int, std::map<std::string, double>>& sublattice_composition)
        {
            const auto second_sublattice = sublattice_composition.find(2);
            if (second_sublattice == sublattice_composition.end())
                return false;

            for (const auto& constituent_entry : second_sublattice->second)
            {
                if (constituent_entry.second > 1.0e-3 &&
                    normalizeSublatticeConstituent(constituent_entry.first) == "VA")
                    return false;
            }
            return true;
        };

        const bool is_oxide_liquid =
            !sublattice_composition.empty()
                ? hasOxideSecondSublattice(sublattice_composition)
                : false;

        double liquid_density_estimate = 0.0;

        if (is_liquid_phase)
        {
            // Store every constituent of the liquid's two sublattices
            static const std::vector<std::string> first_sublattice_species = {
                "BA+2", "CS+", "MO+4", "PD+2", "RH+3", "RU+4", "TC+4"};
            static const std::vector<std::string> second_sublattice_species = {
                "MOO4-2", "O-2", "VA", "CSO2", "MOO3", "O"};

            auto sublatticeFraction = [&sublattice_composition](int sublattice_index, const std::string& species)
            {
                const auto sublattice = sublattice_composition.find(sublattice_index);
                if (sublattice == sublattice_composition.end())
                    return 0.0;

                const auto constituent = sublattice->second.find(species);
                return constituent != sublattice->second.end() ? constituent->second : 0.0;
            };

            auto storeSublatticeSpecies = [this, &sublatticeFraction](
                                               int sublattice_index, const std::vector<std::string>& species_names)
            {
                for (const auto& species : species_names)
                    thermochemistry_variable[species + " (liquid, derived)"].setFinalValue(
                        sublatticeFraction(sublattice_index, species));
            };

            storeSublatticeSpecies(1, first_sublattice_species);
            storeSublatticeSpecies(2, second_sublattice_species);

            // Exploratory: estimate the liquid density from its sublattice site
            // fractions via the volume-additivity mixing rule
            // (1/rho_mix = sum x_i/rho_i), treated as a 2x2 reciprocal system
            // of the two cations and two anions with a known condensed-phase
            // density on file: Cs2MoO4 (CS+ & MOO4-2, temperature-dependent
            // density from the Wallez et al. model above), BaMoO4 (BA+2 &
            // MOO4-2, fixed manifest density - Ba2+ pairs preferentially with
            // MoO4-2 over O-2, so this pair matters more than BaO/halite for a
            // Cs2MoO4-Ba(Mo)Ox liquid) and BaO/halite (BA+2 & O-2, fixed
            // manifest density). Each pair's "mole fraction" is approximated as
            // the product of its cation and anion site fractions (a
            // reciprocal-system approximation, not exact without full
            // charge-balance stoichiometry), renormalized over the three known
            // pairs so they sum to 1. Cs2O (CS+ & O-2) completes the reciprocal
            // square but has no known density and is left out of the mixture.
            const double y_cs   = sublatticeFraction(1, "CS+");
            const double y_ba   = sublatticeFraction(1, "BA+2");
            const double y_moo4 = sublatticeFraction(2, "MOO4-2");
            const double y_o    = sublatticeFraction(2, "O-2");

            const double halite_density =
                thermochemistry_variable["HALITE (condensed, at grain boundary)"].getTheoreticalDensity();
            const double bamoo4_density =
                thermochemistry_variable["BAMOO4 (condensed, at grain boundary)"].getTheoreticalDensity();

            const double cs2moo4_pair_fraction = y_cs * y_moo4;
            const double bamoo4_pair_fraction  = y_ba * y_moo4;
            const double bao_pair_fraction     = y_ba * y_o;
            const double pair_fraction_total =
                cs2moo4_pair_fraction + bamoo4_pair_fraction + bao_pair_fraction;

            if (pair_fraction_total > 0.0 && halite_density > 0.0 && bamoo4_density > 0.0)
            {
                const double x_cs2moo4 = cs2moo4_pair_fraction / pair_fraction_total;
                const double x_bamoo4  = bamoo4_pair_fraction / pair_fraction_total;
                const double x_bao     = bao_pair_fraction / pair_fraction_total;
                const double estimate =
                    1.0 / (x_cs2moo4 / theoretical_density + x_bamoo4 / bamoo4_density + x_bao / halite_density);

                // Sanity range for a Cs-Ba-Mo-O molten oxide, 1-10 g/cm3
                // (1e6-1e7 g/m3); outside this range the reciprocal-system
                // approximation is discarded in favour of the Cs2MoO4 density.
                const double min_plausible_density = 1.0e6;
                const double max_plausible_density = 1.0e7;
                if (estimate > min_plausible_density && estimate < max_plausible_density)
                    liquid_density_estimate = estimate;
            }
        }

        // Cs2MoO4 has no crystallographic unit-cell density on file (it is
        // derived from the temperature-dependent thermal-expansion model
        // above); the manifest can still override it if supplied.
        const double cs2moo4_density = manifest_density > 0.0 ? manifest_density : theoretical_density;

        if (is_liquid_phase && is_oxide_liquid)
        {
            const double liquid_density = liquid_density_estimate > 0.0 ? liquid_density_estimate : cs2moo4_density;
            variable.setTheoreticalDensity(liquid_density);
            JOG_liquid += mass / liquid_density;
        }
        else if (variable_name == "CS2MOO4_S1 (condensed, at grain boundary)")
        {
            variable.setTheoreticalDensity(cs2moo4_density);
            JOG_Cs2MoO4 += mass / cs2moo4_density;
        }
        else if (variable_name == "CS2MOO4_S2 (condensed, at grain boundary)")
        {
            variable.setTheoreticalDensity(cs2moo4_density);
            JOG_Cs2MoO4 += mass / cs2moo4_density;
        }
        else if (variable_name == "BAMOO4 (condensed, at grain boundary)")
        {
            if (manifest_density > 0.0)
                JOG_BaMoO4 += mass / manifest_density;
        }
        else if (variable_name == "BA3MOO6 (condensed, at grain boundary)")
        {
            if (manifest_density > 0.0)
                JOG_Ba3MoO6 += mass / manifest_density;
        }
        else if (variable_name == "BA2MOO5 (condensed, at grain boundary)")
        {
            if (manifest_density > 0.0)
                JOG_Ba2MoO5 += mass / manifest_density;
        }
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
        else if (manifest_density > 0.0)
            JOG_other += mass / manifest_density;
    }

    sciantix_variable["JOG (Cs2MoO4)"].setFinalValue(JOG_Cs2MoO4);
    sciantix_variable["JOG (BaMoO4)"].setFinalValue(JOG_BaMoO4);
    sciantix_variable["JOG (Ba3MoO6)"].setFinalValue(JOG_Ba3MoO6);
    sciantix_variable["JOG (Ba2MoO5)"].setFinalValue(JOG_Ba2MoO5);
    sciantix_variable["JOG (liquid)"].setFinalValue(JOG_liquid);
    sciantix_variable["JOG (other phases)"].setFinalValue(JOG_other);
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
