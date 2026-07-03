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

    // The polynomial fits give the mean relative linear expansion,
    // eps_l = Delta l / l0, as a function of temperature in degree Celsius.

    // Reference density from Wallez et al. at 675 °C:
    // rho = 3.89 g/cm3 = 3890 kg/m3

    // Transition temperature, °C
    const double T_transition = 568.0;

    // Reference state: h-Cs2MoO4 at 675 °C
    const double T_ref = 675.0;
    const double rho_ref = 3.89 ; // g/cm3

    auto epsilon_orthorhombic = [](double T)
    {
        return  - 7.12e-4
                + 2.57e-5 * T
                + 4.03e-8 * std::pow(T, 2.0);
    };

    auto epsilon_hexagonal = [](double T)
    {
        return -0.0102
               + 8.50e-5 * T
               - 2.13e-8 * std::pow(T, 2.0);
    };

    // Relative linear expansion at the reference temperature
    const double eps_ref = epsilon_hexagonal(T_ref);

    // Relative linear expansion at current temperature
    double eps_T;

    if (temperature_celsius < T_transition)
        eps_T = epsilon_orthorhombic(temperature_celsius);
    else
        eps_T = epsilon_hexagonal(temperature_celsius);

    // Density from mass conservation:
    // V(T) / V_ref = [(1 + eps_T) / (1 + eps_ref)]^3
    double theoretical_density =
        1e6 * rho_ref * std::pow((1.0 + eps_ref) / (1.0 + eps_T), 3.0); // g/m3

    sciantix_variable["Phase std density"].setFinalValue(theoretical_density); // g/m3
    std::cout << "JOGFormation: theoretical density = " << theoretical_density << " g/m3" << std::endl;
    
    // Theoretical densities from crystallographic data (Z = 4):
    // rho = Z * M / (N_A * V_cell)
    // BaMoO4: I41/a, a = 0.5571 nm, c = 1.2783 nm (ref. [31])
    // Ba2MoO5: Pnma, a = 0.7412 nm, b = 0.5769 nm, c = 1.1380 nm (ref. [32])
    // Ba3MoO6: Fm-3m, a = 0.8600 nm
    // https://doi.org/10.1016/j.jeurceramsoc.2021.01.010 Smith, 2021

    auto crystalDensityFromCell = [](double molar_mass_g_per_mol,
                                     double z_formula_units,
                                     double cell_volume_nm3)
    {
        constexpr double nm3_to_cm3 = 1.0e-21;
        return 1.0e6 * z_formula_units * molar_mass_g_per_mol /
               (avogadro_number * cell_volume_nm3 * nm3_to_cm3); // g/m3
    };

    const double theoretical_density_BaMoO4 = crystalDensityFromCell(
        137.327 + 95.95 + 4.0 * 15.999,
        4.0,
        0.5571 * 0.5571 * 1.2783);
    std::cout << "JOGFormation: theoretical density BaMoO4 = " << theoretical_density_BaMoO4 << " g/m3" << std::endl;

    const double theoretical_density_Ba2MoO5 = crystalDensityFromCell(
        2.0 * 137.327 + 95.95 + 5.0 * 15.999,
        4.0,
        0.7412 * 0.5769 * 1.1380);
    std::cout << "JOGFormation: theoretical density Ba2MoO5 = " << theoretical_density_Ba2MoO5 << " g/m3" << std::endl;

    const double theoretical_density_Ba3MoO6 = crystalDensityFromCell(
        3.0 * 137.327 + 95.95 + 6.0 * 15.999,
        4.0,
        0.8600 * 0.8600 * 0.8600);
    std::cout << "JOGFormation: theoretical density Ba3MoO6 = " << theoretical_density_Ba3MoO6 << " g/m3" << std::endl;
    
    double JOG_Cs2MoO4 = 0.0;
    double JOG_BaMoO4 = 0.0;
    double JOG_Ba3MoO6 = 0.0;
    double JOG_Ba2MoO5 = 0.0;
    double JOG_liquid = 0.0;

    double total_mo_moles = 0.0;
    double oxide_mo_moles = 0.0;
    double oxide_mo_valence_sum = 0.0;
    double total_ba_moles = 0.0;
    double oxide_ba_moles = 0.0;
    double oxide_ba_valence_sum = 0.0;
    constexpr double oxide_stoichiometry_tolerance = 1.0e-8;
    constexpr double minimum_mo_oxide_valence = 2.0;
    constexpr double minimum_molybdate_site_fraction = 1.0e-12;
    constexpr double minimum_molybdate_oxygen_to_mo = 3.0;
    constexpr double minimum_molybdate_valence = 5.0;
    constexpr double mo_valence_tolerance = 1.0e-6;

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
            {"I", 126.90447},
            {"Ba", 137.327},
            {"Mo", 95.95},
            {"O", 15.999},
            {"Te", 127.60},
            {"U", 238.02891},
            {"Pu", 239.052},
            {"Va", 0.0},
            {"Pd", 106.42},
            {"Rh", 102.91},
            {"Ru", 101.07},
            {"Tc", 98.906},
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

    auto isOxidizedElement = [&getElementAmount, oxide_stoichiometry_tolerance](
                                 const std::map<std::string, double>& composition,
                                 const std::string&                   element)
    {
        const double element_stoichiometry = getElementAmount(composition, element);
        if (element_stoichiometry <= 0.0)
            return false;

        // OpenCalphad can leave tiny oxygen site fractions in metallic phases.
        // Treat only meaningful O/element ratios as oxidized inventory.
        const double oxygen_stoichiometry = getElementAmount(composition, "O");
        return oxygen_stoichiometry / element_stoichiometry > oxide_stoichiometry_tolerance;
    };

    auto isOxidizedMo = [&effectiveMoValence, minimum_mo_oxide_valence, mo_valence_tolerance](
                            const std::map<std::string, double>& composition)
    {
        return effectiveMoValence(composition) >= minimum_mo_oxide_valence - mo_valence_tolerance;
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

    auto hasMolybdateSecondSublattice =
        [minimum_molybdate_site_fraction, &normalizeSublatticeConstituent](
            const std::map<int, std::map<std::string, double>>& sublattice_composition)
    {
        const auto second_sublattice = sublattice_composition.find(2);
        if (second_sublattice == sublattice_composition.end())
            return false;

        for (const auto& constituent_entry : second_sublattice->second)
        {
            if (constituent_entry.second > minimum_molybdate_site_fraction &&
                normalizeSublatticeConstituent(constituent_entry.first) == "MOO4")
                return true;
        }

        return false;
    };

    auto isMolybdateLiquidByStoichiometry =
        [&getElementAmount,
         &effectiveMoValence,
         minimum_molybdate_oxygen_to_mo,
         minimum_molybdate_valence,
         mo_valence_tolerance](const std::map<std::string, double>& composition)
    {
        const double mo_stoichiometry = getElementAmount(composition, "Mo");
        if (mo_stoichiometry <= 0.0)
            return false;

        const double oxygen_to_mo = getElementAmount(composition, "O") / mo_stoichiometry;
        if (oxygen_to_mo < minimum_molybdate_oxygen_to_mo)
            return false;

        return effectiveMoValence(composition) >= minimum_molybdate_valence - mo_valence_tolerance;
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
        if (phase != "condensed" && phase != "liquid" && phase != "ionic_liquid" && phase != "liquid_ionic")
            continue;

        const std::string variable_name = variable.getName();
        const bool is_liquid_phase = phase == "liquid" || phase == "ionic_liquid" || phase == "liquid_ionic";
        if (is_liquid_phase && variable_name.rfind("LIQUID (", 0) != 0)
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
            

        accumulateOxidePhase(composition, phase_molar_mass, mass);

        const std::map<int, std::map<std::string, double>> sublattice_composition =
            variable.getSublatticeComposition();
        const bool is_molybdate_liquid =
            !sublattice_composition.empty()
                ? hasMolybdateSecondSublattice(sublattice_composition)
                : isMolybdateLiquidByStoichiometry(composition);

        if (is_liquid_phase && is_molybdate_liquid)
            JOG_liquid += mass / theoretical_density;
        else if (variable_name == "CS2MOO4_S1 (condensed, at grain boundary)")
            JOG_Cs2MoO4 += mass / theoretical_density;
        else if (variable_name == "CS2MOO4_S2 (condensed, at grain boundary)")
            JOG_Cs2MoO4 += mass / theoretical_density;
        else if (variable_name == "BAMOO4 (condensed, at grain boundary)")
            JOG_BaMoO4 += mass / theoretical_density_BaMoO4;
        else if (variable_name == "BA3MOO6 (condensed, at grain boundary)")
            JOG_Ba3MoO6 += mass / theoretical_density_Ba3MoO6;
        else if (variable_name == "BA2MOO5 (condensed, at grain boundary)")
            JOG_Ba2MoO5 += mass / theoretical_density_Ba2MoO5;
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
    sciantix_variable["JOG (Ba3MoO6)"].setFinalValue(JOG_Ba3MoO6);
    sciantix_variable["JOG (Ba2MoO5)"].setFinalValue(JOG_Ba2MoO5);
    sciantix_variable["JOG (liquid)"].setFinalValue(JOG_liquid);
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
