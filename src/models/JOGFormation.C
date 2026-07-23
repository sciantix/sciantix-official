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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Constants.h"
#include "Simulation.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <iostream>
#include <map>
#include <string>
#include <vector>

namespace JOGFormationDetail
{
    /// Element composition maps coming from OpenCalphad may key elements as
    /// "MO", "Mo" or "mo" depending on the database; every lookup below goes
    /// through this normalized form ("Mo") so callers never have to worry
    /// about case.
    std::string normalizeElementName(std::string element)
    {
        if (element.empty())
            return element;

        std::transform(element.begin(),
                       element.end(),
                       element.begin(),
                       [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        element[0] = static_cast<char>(std::toupper(static_cast<unsigned char>(element[0])));
        return element;
    }

    double getElementAmount(const std::map<std::string, double>& composition, const std::string& element)
    {
        double amount = 0.0;
        for (const auto& entry : composition)
            if (normalizeElementName(entry.first) == element)
                amount += entry.second;
        return amount;
    }

    /// Charge-balance estimate of the Mo valence in a phase, with O=-2,
    /// Cs=+1 and Ba=+2. For Cs2MoO4 and BaMoO4 this gives Mo=+6; for MoO2 it
    /// gives Mo=+4.
    double effectiveMoValence(const std::map<std::string, double>& composition)
    {
        const double mo = getElementAmount(composition, "Mo");
        if (mo <= 0.0)
            return 0.0;

        const double oxygen             = getElementAmount(composition, "O");
        const double monovalent_cations = getElementAmount(composition, "Cs");
        const double divalent_cations   = getElementAmount(composition, "Ba");

        const double valence = (2.0 * oxygen - monovalent_cations - 2.0 * divalent_cations) / mo;
        return std::max(0.0, std::min(6.0, valence));
    }

    /// OpenCalphad can leave tiny oxygen site fractions in metallic phases;
    /// only a meaningful O/element ratio counts as oxidized inventory.
    bool isOxidizedElement(const std::map<std::string, double>& composition, const std::string& element)
    {
        const double element_stoichiometry = getElementAmount(composition, element);
        if (element_stoichiometry <= 0.0)
            return false;

        const double oxygen_stoichiometry = getElementAmount(composition, "O");
        return oxygen_stoichiometry / element_stoichiometry > 1.0e-8;
    }

    bool isOxidizedMo(const std::map<std::string, double>& composition)
    {
        return effectiveMoValence(composition) >= 2.0 - 1.0e-6;
    }

    std::string normalizeSublatticeConstituent(std::string constituent)
    {
        std::transform(constituent.begin(),
                       constituent.end(),
                       constituent.begin(),
                       [](unsigned char c) { return static_cast<char>(std::toupper(c)); });

        const size_t charge_pos = constituent.find_first_of("+-");
        if (charge_pos != std::string::npos)
            constituent = constituent.substr(0, charge_pos);

        return constituent;
    }

    /// Volume-additivity estimate of a non-stoichiometric metallic phase's
    /// density from its dissolved Ru-Mo-Tc-Rh-Pd content, weighted by the
    /// pure-element densities. Used for every metallic solid-solution phase
    /// (HCP_A3, HCP_DIS, HCP_ORD, FCC_A1, SIGMA) and for the metallic
    /// liquid.
    double estimateElementalMixDensity(const std::map<std::string, double>& composition)
    {
        // Atomic mass (g/mol) and density (g/cm3) of the stable
        // room-temperature elemental structures of the noble-metal fission
        // products dissolved in the metallic phases, from the Materials
        // Project: Mo mp-129 (BCC Im-3m), Tc mp-113 (HCP P6_3/mmc),
        // Ru mp-33 (HCP P6_3/mmc), Rh mp-74 (FCC Fm-3m), Pd mp-2 (FCC Fm-3m).
        static const std::vector<std::tuple<std::string, double, double>> metallic_elements = {{"Mo", 95.95, 10.02},
                                                                                               {"Tc", 98.00, 11.42},
                                                                                               {"Ru", 101.07, 12.38},
                                                                                               {"Rh", 102.91, 12.40},
                                                                                               {"Pd", 106.42, 11.76}};

        double total_mass      = 0.0;
        double volume_per_mass = 0.0;
        for (const auto& [element, atomic_mass, element_density] : metallic_elements)
        {
            const double moles = getElementAmount(composition, element);
            if (moles <= 0.0)
                continue;

            const double element_mass = moles * atomic_mass;
            total_mass += element_mass;
            volume_per_mass += element_mass / element_density;
        }

        if (total_mass <= 0.0 || volume_per_mass <= 0.0)
            return 0.0;

        return 1.0e6 * total_mass / volume_per_mass;  // g/m3
    }

    /// ThermochemistryVariable builds its variable name as "<PHASE_NAME>
    /// (condensed/liquid, at ...)"; miscibility-gap composition sets
    /// ("<PHASE>#<n>" in OpenCalphad) are already merged into a single
    /// phase entry upstream, in OCUtilsCoupling.C's normalizeSpeciesName(),
    /// so no "#" ever reaches this point. This just strips the trailing
    /// " (...)" annotation to recover the bare phase name.
    std::string basePhaseName(std::string variable_name)
    {
        const size_t space_pos = variable_name.find(' ');
        if (space_pos != std::string::npos)
            variable_name.resize(space_pos);

        return variable_name;
    }

    bool hasOxideSecondSublattice(const std::map<int, std::map<std::string, double>>& sublattice_composition)
    {
        const auto second_sublattice = sublattice_composition.find(2);
        if (second_sublattice == sublattice_composition.end())
            return false;

        for (const auto& constituent_entry : second_sublattice->second)
            if (constituent_entry.second > 1.0e-3 && normalizeSublatticeConstituent(constituent_entry.first) == "VA")
                return false;

        return true;
    }

    double sublatticeFraction(const std::map<int, std::map<std::string, double>>& sublattice_composition,
                              int                                                 sublattice_index,
                              const std::string&                                  species)
    {
        const auto sublattice = sublattice_composition.find(sublattice_index);
        if (sublattice == sublattice_composition.end())
            return 0.0;

        const auto constituent = sublattice->second.find(species);
        return constituent != sublattice->second.end() ? constituent->second : 0.0;
    }

    /// Exploratory estimate of the oxide liquid's density from its
    /// sublattice site fractions via the volume-additivity mixing rule
    /// (1/rho_mix = sum x_i/rho_i), treated as a 2x2 reciprocal system of the
    /// two cations and two anions with a known condensed-phase density on
    /// file: Cs2MoO4 (CS+ & MOO4-2, temperature-dependent density from the
    /// Wallez et al. model), BaMoO4 (BA+2 & MOO4-2, fixed manifest density -
    /// Ba2+ pairs preferentially with MoO4-2 over O-2, so this pair matters
    /// more than BaO/halite for a Cs2MoO4-Ba(Mo)Ox liquid) and BaO/halite
    /// (BA+2 & O-2, fixed manifest density). Each pair's "mole fraction" is
    /// approximated as the product of its cation and anion site fractions (a
    /// reciprocal-system approximation, not exact without full
    /// charge-balance stoichiometry), renormalized over the three known
    /// pairs so they sum to 1. Cs2O (CS+ & O-2) completes the reciprocal
    /// square but has no known density and is left out of the mixture.
    /// Returns 0.0 if the estimate cannot be formed or falls outside the
    /// plausible range for a Cs-Ba-Mo-O molten oxide, 1-10 g/cm3.
    double estimateOxideLiquidDensity(const std::map<int, std::map<std::string, double>>& sublattice_composition,
                                      double                                              cs2moo4_density,
                                      double                                              bamoo4_density,
                                      double                                              halite_density)
    {
        const double y_cs   = sublatticeFraction(sublattice_composition, 1, "CS+");
        const double y_ba   = sublatticeFraction(sublattice_composition, 1, "BA+2");
        const double y_moo4 = sublatticeFraction(sublattice_composition, 2, "MOO4-2");
        const double y_o    = sublatticeFraction(sublattice_composition, 2, "O-2");

        const double cs2moo4_pair_fraction = y_cs * y_moo4;
        const double bamoo4_pair_fraction  = y_ba * y_moo4;
        const double bao_pair_fraction     = y_ba * y_o;
        const double pair_fraction_total   = cs2moo4_pair_fraction + bamoo4_pair_fraction + bao_pair_fraction;

        if (pair_fraction_total <= 0.0 || halite_density <= 0.0 || bamoo4_density <= 0.0)
            return 0.0;

        const double x_cs2moo4 = cs2moo4_pair_fraction / pair_fraction_total;
        const double x_bamoo4  = bamoo4_pair_fraction / pair_fraction_total;
        const double x_bao     = bao_pair_fraction / pair_fraction_total;
        const double estimate =
            1.0 / (x_cs2moo4 / cs2moo4_density + x_bamoo4 / bamoo4_density + x_bao / halite_density);

        const double min_plausible_density = 1.0e6;  // g/m3
        const double max_plausible_density = 1.0e7;  // g/m3
        return (estimate > min_plausible_density && estimate < max_plausible_density) ? estimate : 0.0;
    }

    /// Theoretical density of h/beta-Cs2MoO4(s) from the thermal-expansion
    /// model of Wallez et al., Journal of Solid State Chemistry 215 (2014)
    /// 225-230. The polynomial fits give the mean relative linear expansion,
    /// eps_l = Delta l / l0, as a function of temperature in degree Celsius,
    /// with a reference density rho = 3.89 g/cm3 at 675 degC and a
    /// beta<->h transition at 568 degC.
    double cs2moo4TheoreticalDensity(double temperature_celsius)
    {
        const double T_transition = 568.0;  // degC
        const double T_ref        = 675.0;  // degC
        const double rho_ref      = 3.89;   // g/cm3

        const double eps_ref = -0.0102 + 8.50e-5 * T_ref - 2.13e-8 * std::pow(T_ref, 2.0);

        const double eps_T =
            temperature_celsius < T_transition
                ? -7.12e-4 + 2.57e-5 * temperature_celsius + 4.03e-8 * std::pow(temperature_celsius, 2.0)
                : -0.0102 + 8.50e-5 * temperature_celsius - 2.13e-8 * std::pow(temperature_celsius, 2.0);

        // Density from mass conservation: V(T) / V_ref = [(1 + eps_T) / (1 + eps_ref)]^3
        return 1e6 * rho_ref * std::pow((1.0 + eps_ref) / (1.0 + eps_T), 3.0);  // g/m3
    }

    /// Phases whose density is a fixed physical property, read from the
    /// optional density column of input_thermochemistry.txt.
    bool isManifestDensityPhase(const std::string& base_name)
    {
        static const std::vector<std::string> manifest_density_phases = {
            "BAMOO4", "BA3MOO6", "HALITE", "MOPD2", "MOO2", "PEROVSKITE"};
        return std::find(manifest_density_phases.begin(), manifest_density_phases.end(), base_name) !=
               manifest_density_phases.end();
    }

    /// Metallic solid-solution phases whose density is estimated from their
    /// dissolved Ru-Mo-Tc-Rh-Pd content. The hcp phase can appear under any
    /// of its order-disorder variants - "HCP_A3" (disordered, average
    /// lattice), "HCP_DIS" and "HCP_ORD" (explicit disordered/ordered
    /// contributions of a compound-energy-formalism split) - all of which
    /// are folded into the same JOG (HCP) accumulator.
    bool isHcpPhase(const std::string& base_name)
    {
        return base_name == "HCP_A3" || base_name == "HCP_DIS" || base_name == "HCP_ORD";
    }

    bool isMetallicSolidSolutionPhase(const std::string& base_name)
    {
        return isHcpPhase(base_name) || base_name == "FCC_A1" || base_name == "SIGMA";
    }
}  // namespace JOGFormationDetail

using namespace JOGFormationDetail;

void Simulation::JOGFormation()
{
    const int thermochimica_mode = (int)input_variable["iThermochimica"].getValue();
    if (thermochimica_mode == 0)
        return;

    // THERMOCHEMISTRY OUTER-NODE MODE
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

    const double temperature_celsius         = history_variable["Temperature"].getFinalValue() - 273.15;
    const double cs2moo4_theoretical_density = cs2moo4TheoreticalDensity(temperature_celsius);
    const double fallback_other_density      = 4.0e6;  // g/m3 (4 g/cm3 placeholder)

    // Theoretical densities for BaMoO4, Ba3MoO6, BaO (halite), MoPd2, MoO2
    // and perovskite are supplied via the optional density column of
    // input_thermochemistry.txt.

    double JOG_Cs2MoO4         = 0.0;
    double JOG_BaMoO4          = 0.0;
    double JOG_Ba3MoO6         = 0.0;
    double JOG_BaO             = 0.0;
    double JOG_liquid_oxide    = 0.0;
    double JOG_liquid_metallic = 0.0;
    double JOG_other           = 0.0;
    double JOG_HCP_A3          = 0.0;
    double JOG_FCC_A1          = 0.0;
    double JOG_Sigma           = 0.0;
    double JOG_MoPd2           = 0.0;
    double JOG_MoO2            = 0.0;
    double JOG_Perovskite      = 0.0;

    double total_mo_moles       = 0.0;
    double oxide_mo_moles       = 0.0;
    double oxide_mo_valence_sum = 0.0;
    double total_ba_moles       = 0.0;
    double oxide_ba_moles       = 0.0;
    double oxide_ba_valence_sum = 0.0;
    double hcp_mo_moles         = 0.0;
    double hcp_ru_moles         = 0.0;

    for (auto& variable : thermochemistry_variable)
    {
        if (variable.getLocation() != "at grain boundary")
            continue;

        if (variable.getFinalValue() <= 0.0)
            continue;

        const std::string phase           = variable.getPhase();
        const bool        is_liquid_phase = phase == "liquid" || phase == "ionic_liquid" || phase == "liquid_ionic";
        if (phase != "condensed" && !is_liquid_phase)
            continue;

        const std::string                                  variable_name          = variable.getName();
        const std::string                                  base_name              = basePhaseName(variable_name);
        const std::map<std::string, double>                composition            = variable.getComposition();
        const std::map<int, std::map<std::string, double>> sublattice_composition = variable.getSublatticeComposition();
        const double                                       phase_molar_mass       = variable.getMolarMass();
        const double                                       mass                   = variable.getMass();
        const double                                       manifest_density       = variable.getTheoreticalDensity();

        // Accumulates this phase's volume into `accumulator` using
        // `density_value` when available (>0), otherwise falls back to the
        // fixed placeholder density and routes the volume into
        // "JOG (other phases)" instead, so an untracked or unresolved
        // density never silently vanishes from the mixture.
        auto accumulateFixed = [&](double density_value, double& accumulator)
        {
            const double density = density_value > 0.0 ? density_value : fallback_other_density;
            variable.setTheoreticalDensity(density);
            if (density_value > 0.0)
                accumulator += mass / density;
            else
                JOG_other += mass / density;
        };

        const double mo_stoichiometry = getElementAmount(composition, "Mo");
        if (mo_stoichiometry > 0.0 && phase_molar_mass > 0.0)
            total_mo_moles += mass * mo_stoichiometry / phase_molar_mass;

        const double ba_stoichiometry = getElementAmount(composition, "Ba");
        if (ba_stoichiometry > 0.0 && phase_molar_mass > 0.0)
            total_ba_moles += mass * ba_stoichiometry / phase_molar_mass;

        if (mo_stoichiometry > 0.0 && phase_molar_mass > 0.0 && isOxidizedMo(composition))
        {
            const double mo_moles = mass * mo_stoichiometry / phase_molar_mass;
            oxide_mo_moles += mo_moles;
            oxide_mo_valence_sum += mo_moles * effectiveMoValence(composition);
        }

        if (ba_stoichiometry > 0.0 && phase_molar_mass > 0.0 && isOxidizedElement(composition, "Ba"))
        {
            const double ba_moles = mass * ba_stoichiometry / phase_molar_mass;
            oxide_ba_moles += ba_moles;
            oxide_ba_valence_sum += ba_moles * 2;  // Valence fixed to +2
        }

        const bool is_oxide_liquid =
            !sublattice_composition.empty() ? hasOxideSecondSublattice(sublattice_composition) : false;

        double liquid_density_estimate = 0.0;

        if (is_liquid_phase)
        {
            // Store every constituent of the liquid's two sublattices
            static const std::vector<std::string> first_sublattice_species = {
                "BA+2", "CS+", "MO+4", "PD+2", "RH+3", "RU+4", "TC+4"};
            static const std::vector<std::string> second_sublattice_species = {
                "MOO4-2", "O-2", "VA", "CSO2", "MOO3", "O"};

            for (const auto& species : first_sublattice_species)
                thermochemistry_variable[species + " (liquid, derived)"].setFinalValue(
                    sublatticeFraction(sublattice_composition, 1, species));
            for (const auto& species : second_sublattice_species)
                thermochemistry_variable[species + " (liquid, derived)"].setFinalValue(
                    sublatticeFraction(sublattice_composition, 2, species));

            const double halite_density =
                thermochemistry_variable["HALITE (condensed, at grain boundary)"].getTheoreticalDensity();
            const double bamoo4_density =
                thermochemistry_variable["BAMOO4 (condensed, at grain boundary)"].getTheoreticalDensity();

            liquid_density_estimate = estimateOxideLiquidDensity(
                sublattice_composition, cs2moo4_theoretical_density, bamoo4_density, halite_density);
        }

        // Cs2MoO4 has no crystallographic unit-cell density on file (it is
        // derived from the temperature-dependent thermal-expansion model
        // above); the manifest can still override it if supplied.
        const double cs2moo4_density = manifest_density > 0.0 ? manifest_density : cs2moo4_theoretical_density;

        if (is_liquid_phase && is_oxide_liquid)
        {
            const double liquid_density = liquid_density_estimate > 0.0 ? liquid_density_estimate : cs2moo4_density;
            accumulateFixed(liquid_density, JOG_liquid_oxide);
        }
        else if (is_liquid_phase && !is_oxide_liquid)
        {
            accumulateFixed(estimateElementalMixDensity(composition), JOG_liquid_metallic);
        }
        else if (base_name == "CS2MOO4_S1" || base_name == "CS2MOO4_S2")
        {
            accumulateFixed(cs2moo4_density, JOG_Cs2MoO4);
        }
        else if (isManifestDensityPhase(base_name))
        {
            static const std::map<std::string, double*> manifest_phase_accumulators = {{"BAMOO4", &JOG_BaMoO4},
                                                                                       {"BA3MOO6", &JOG_Ba3MoO6},
                                                                                       {"HALITE", &JOG_BaO},
                                                                                       {"MOPD2", &JOG_MoPd2},
                                                                                       {"MOO2", &JOG_MoO2},
                                                                                       {"PEROVSKITE", &JOG_Perovskite}};
            accumulateFixed(manifest_density, *manifest_phase_accumulators.at(base_name));
        }
        else if (isMetallicSolidSolutionPhase(base_name))
        {
            // "White metal" epsilon-phase metallic precipitates, tracked
            // separately from the oxide JOG so their volume can be included
            // or excluded from the mixture on demand. HCP_A3, HCP_DIS and
            // HCP_ORD are the disordered/ordered variants of the same hcp
            // phase and are folded into one JOG_HCP_A3 accumulator.
            if (isHcpPhase(base_name))
            {
                hcp_mo_moles += getElementAmount(composition, "Mo");
                hcp_ru_moles += getElementAmount(composition, "Ru");
                accumulateFixed(estimateElementalMixDensity(composition), JOG_HCP_A3);
            }
            else if (base_name == "FCC_A1")
            {
                accumulateFixed(estimateElementalMixDensity(composition), JOG_FCC_A1);
            }
            else  // SIGMA
            {
                accumulateFixed(estimateElementalMixDensity(composition), JOG_Sigma);
            }
        }
        else
        {
            accumulateFixed(0.0, JOG_other);
        }
    }

    sciantix_variable["JOG (Cs2MoO4)"].setFinalValue(JOG_Cs2MoO4);
    sciantix_variable["JOG (BaMoO4)"].setFinalValue(JOG_BaMoO4);
    sciantix_variable["JOG (Ba3MoO6)"].setFinalValue(JOG_Ba3MoO6);
    sciantix_variable["JOG (BaO)"].setFinalValue(JOG_BaO);
    sciantix_variable["JOG (liquid oxide)"].setFinalValue(JOG_liquid_oxide);
    sciantix_variable["JOG (liquid metallic)"].setFinalValue(JOG_liquid_metallic);
    sciantix_variable["JOG (other phases)"].setFinalValue(JOG_other);
    sciantix_variable["JOG (HCP)"].setFinalValue(JOG_HCP_A3);
    sciantix_variable["Mo/Ru in HCP_A3"].setFinalValue(hcp_ru_moles > 0.0 ? hcp_mo_moles / hcp_ru_moles : 0.0);
    sciantix_variable["JOG (FCC)"].setFinalValue(JOG_FCC_A1);
    sciantix_variable["JOG (Sigma)"].setFinalValue(JOG_Sigma);
    sciantix_variable["JOG (MoPd2)"].setFinalValue(JOG_MoPd2);
    sciantix_variable["JOG (MoO2)"].setFinalValue(JOG_MoO2);
    sciantix_variable["JOG (Perovskite)"].setFinalValue(JOG_Perovskite);
    sciantix_variable["Mo in oxide fraction"].setFinalValue(total_mo_moles > 0.0 ? oxide_mo_moles / total_mo_moles
                                                                                 : 0.0);
    sciantix_variable["Mo oxide valence"].setFinalValue(oxide_mo_moles > 0.0 ? oxide_mo_valence_sum / oxide_mo_moles
                                                                             : 0.0);
    sciantix_variable["Ba/Mo in oxide compounds"].setFinalValue(oxide_mo_moles > 0.0 ? oxide_ba_moles / oxide_mo_moles
                                                                                     : 0.0);
    sciantix_variable["Ba in oxide fraction"].setFinalValue(total_ba_moles > 0.0 ? oxide_ba_moles / total_ba_moles
                                                                                 : 0.0);
    sciantix_variable["Ba oxide valence"].setFinalValue(oxide_ba_moles > 0.0 ? oxide_ba_valence_sum / oxide_ba_moles
                                                                             : 0.0);
}
