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
#include "StoichiometryDeviation.h"

#include <algorithm>
#include <cmath>
#include <string>
#include <vector>

namespace GrainVaporisationDetail
{
    // Mean molar mass (g/mol) of an element from the amounts of its isotopes (at/m3)
    double meanMolarMass(const std::vector<std::pair<double, double>>& isotopes, double fallback)
    {
        double atoms = 0.0, mass = 0.0;
        for (const auto& isotope : isotopes)
        {
            atoms += std::max(0.0, isotope.first);
            mass += std::max(0.0, isotope.first) * isotope.second;
        }
        return atoms > 0.0 ? mass / atoms : fallback;
    }

    struct VapourSpecies
    {
        std::string name;              // thermochemistry variable "<name> (vapour, matrix)"
        double      partial_pressure;  // (atm)
        double      molar_mass;        // (g/mol)
        double      u_atoms;           // per molecule
        double      pu_atoms;          // per molecule
        double      o_atoms;           // per molecule
    };
}  // namespace GrainVaporisationDetail

using namespace GrainVaporisationDetail;

void Simulation::GrainVaporisation()
{
    // Matrix vaporisation of (U,Pu)O2, e.g. during annealing in a Knudsen cell.
    // Switched on in input_thermochemistry_settings.txt (matrix.vaporisation = true)
    // from matrix.vaporisation_start_time (h) on.
    if (thermochemistry_settings == nullptr || !thermochemistry_settings->matrix_vaporisation)
        return;
    if (history_variable["Time"].getFinalValue() < thermochemistry_settings->matrix_vaporisation_start_time)
        return;

    const double grain_radius_old = sciantix_variable["Grain radius"].getFinalValue();
    const double temperature      = history_variable["Temperature"].getFinalValue();
    const double time_step        = physics_variable["Time step"].getFinalValue();
    if (grain_radius_old <= 0.0 || temperature <= 0.0 || time_step <= 0.0)
        return;

    double       u_content     = std::max(0.0, sciantix_variable["U content"].getFinalValue());
    double       pu_content    = std::max(0.0, sciantix_variable["Pu content"].getFinalValue());
    double       o_content     = std::max(0.0, sciantix_variable["O content"].getFinalValue());
    const double metal_content = u_content + pu_content;
    if (metal_content <= 0.0 || o_content <= 0.0)
        return;

    const double total_moles_old = metal_content + o_content;
    const double q               = pu_content / metal_content;
    const double x               = sciantix_variable["Stoichiometry deviation"].getFinalValue();
    const double om              = 2.0 + x;

    // Oxygen partial pressure (atm): Kato et al. (MOX), with the Pu fraction of the matrix used for the valence
    // distribution below, or Blackburn (UO2), as in StoichiometryDeviation
    const double p_o2 = (q > 0.0) ? solver.BisectionKato({temperature, q, om, 0.0})
                                  : BlackburnThermochemicalModel(x, temperature, sciantix_variable);

    // Valence distribution of U and Pu (fractions of all metal atoms), constants from Olander (1976).
    // Without an oxygen potential (p_O2 = 0, e.g. hypo-stoichiometric UO2 with Blackburn) all U and Pu are 4+.
    double n_u2 = 0.0, n_u4 = 1.0 - q, n_u6 = 0.0;
    double n_pu2 = 0.0, n_pu3 = 0.0, n_pu4 = q;
    if (p_o2 > 0.0 && std::isfinite(p_o2))
    {
        const double K_u24    = std::exp(-78.3e3 / temperature + 13.6);
        const double K_u46    = std::exp(-16.4e3 / temperature + 5.0);
        const double K_pu34   = std::exp(-50.1e3 / temperature + 10.3);
        const double K_pu23   = std::exp(-92.5e3 / temperature + 21.3);
        const double sqrt_po2 = std::sqrt(p_o2);

        n_u4 = (1.0 - q) / (1.0 + (K_u24 * om) / sqrt_po2 + sqrt_po2 / (K_u46 * om));
        n_u2 = (K_u24 * om) / sqrt_po2 * n_u4;
        n_u6 = sqrt_po2 / (K_u46 * om) * n_u4;

        const double ratio_34 = std::sqrt((K_pu34 * om) / sqrt_po2);
        const double ratio_23 = std::sqrt((K_pu23 * om) / sqrt_po2);
        n_pu4                 = q / (1.0 + ratio_34 + ratio_23 * ratio_34);
        n_pu3                 = ratio_34 * n_pu4;
        n_pu2                 = ratio_23 * n_pu3;
    }

    // Partial pressures (atm) of the vapour species, Olander (1976), Table 11.1, proportional to the fraction of
    // the metal atoms in the corresponding valence state; the oxygen-to-metal factors use the actual O/M.
    // Metallic U is not considered. O2 effuses with the oxygen partial pressure from all the surface, which
    // reduces a hyper-stoichiometric fuel towards O/M = 2 in vacuum.
    const double K_UO   = std::exp(-49500.0 / temperature + 11.9);
    const double K_UO2  = std::exp(-74000.0 / temperature + 19.9);
    const double K_UO3  = std::exp(-44000.0 / temperature + 11.9);
    const double K_PuO  = std::exp(-44100.0 / temperature + 11.5);
    const double K_PuO2 = std::exp(-72500.0 / temperature + 18.8);

    const double molar_mass_U  = meanMolarMass({{sciantix_variable["U234"].getFinalValue(), 234.04095},
                                                {sciantix_variable["U235"].getFinalValue(), 235.04393},
                                                {sciantix_variable["U236"].getFinalValue(), 236.04557},
                                                {sciantix_variable["U237"].getFinalValue(), 237.04873},
                                                {sciantix_variable["U238"].getFinalValue(), 238.05079}},
                                              238.02891);
    const double molar_mass_Pu = meanMolarMass({{sciantix_variable["Pu238"].getFinalValue(), 238.04956},
                                                {sciantix_variable["Pu239"].getFinalValue(), 239.05216},
                                                {sciantix_variable["Pu240"].getFinalValue(), 240.05381},
                                                {sciantix_variable["Pu241"].getFinalValue(), 241.05685},
                                                {sciantix_variable["Pu242"].getFinalValue(), 242.05874}},
                                               239.05216);

    const std::vector<VapourSpecies> species = {
        {"UO",
         om * K_UO * n_u2,
         molar_mass_U + molar_mass_Oxygen,
         1.0,
         0.0,
         1.0},
        {"UO2",
         om * om * K_UO2 * n_u4,
         molar_mass_U + 2.0 * molar_mass_Oxygen,
         1.0,
         0.0,
         2.0},
        {"UO3",
         om * om * om * K_UO3 * n_u6,
         molar_mass_U + 3.0 * molar_mass_Oxygen,
         1.0,
         0.0,
         3.0},
        {"PuO",
         om * K_PuO * n_pu2,
         molar_mass_Pu + molar_mass_Oxygen,
         0.0,
         1.0,
         1.0},
        {"PuO2",
         om * om * K_PuO2 * n_pu4,
         molar_mass_Pu + 2.0 * molar_mass_Oxygen,
         0.0,
         1.0,
         2.0},
        {"O2", std::isfinite(p_o2) ? std::max(0.0, p_o2) : 0.0, 2.0 * molar_mass_Oxygen, 0.0, 0.0, 2.0},
    };

    for (const auto& vapour : species)
        sciantix_variable["Matrix vapour pressure " + vapour.name].setFinalValue(vapour.partial_pressure);

    // Only the porosity fraction of the grain surface is exposed to the vacuum..
    Matrix       fuel(matrices[0]);
    const double porosity                 = std::max(0.0, sciantix_variable["Porosity"].getFinalValue());
    const double surface_to_volume_sample = 3.0 / grain_radius_old * porosity;  // (1/m)

    // Knudsen cell: the vapour leaves the cell through an orifice of area A_o and Clausing factor C. Evaporation from
    // the sample surface A_s and effusion through the orifice act in series: the cell pressure is p A_s / (A_s + C A_o)
    // and the sample loses J A_s C A_o / (A_s + C A_o). Per unit volume of the sample, V = sample mass / fuel density.
    // Without orifice the matrix vaporises freely from the grain surface.
    double       surface_to_volume = surface_to_volume_sample;
    const double orifice_diameter  = thermochemistry_settings->matrix_vaporisation_orifice_diameter;
    const double sample_mass       = thermochemistry_settings->matrix_vaporisation_sample_mass;
    if (orifice_diameter > 0.0 && sample_mass > 0.0 && surface_to_volume_sample > 0.0)
    {
        const double sample_volume     = sample_mass / sciantix_variable["Fuel density"].getFinalValue();  // (m3)
        const double orifice_to_volume = thermochemistry_settings->matrix_vaporisation_clausing_factor * M_PI *
                                         orifice_diameter * orifice_diameter / 4.0 / sample_volume;  // (1/m)
        surface_to_volume =
            surface_to_volume_sample * orifice_to_volume / (surface_to_volume_sample + orifice_to_volume);
    }

    for (const auto& vapour : species)
    {
        if (!(vapour.partial_pressure > 0.0))
            continue;

        // Hertz-Knudsen effusion flux (mol/m2/s): J = p / sqrt(2 pi M R T), p in Pa
        const double flux = vapour.partial_pressure * 101325.0 /
                            std::sqrt(2.0 * M_PI * vapour.molar_mass * 1.0e-3 * gas_constant * temperature);

        // Molecules vaporised per unit fuel volume (mol/m3), limited by the available metal and oxygen.
        double vaporised = flux * surface_to_volume * time_step;
        if (vapour.u_atoms > 0.0)
            vaporised = std::min(vaporised, u_content / vapour.u_atoms);
        if (vapour.pu_atoms > 0.0)
            vaporised = std::min(vaporised, pu_content / vapour.pu_atoms);
        if (vapour.o_atoms > 0.0)
            vaporised = std::min(vaporised, o_content / vapour.o_atoms);
        // In UO2 the oxygen potential (Blackburn) is defined only for O/M > 2: O2 removes at most the excess oxygen
        if (vapour.name == "O2" && q <= 0.0)
            vaporised = std::min(vaporised, std::max(0.0, o_content - 2.0 * (u_content + pu_content)) / 2.0);
        if (vaporised <= 0.0)
            continue;

        u_content -= vapour.u_atoms * vaporised;
        pu_content -= vapour.pu_atoms * vaporised;
        o_content -= vapour.o_atoms * vaporised;

        const std::string variable_name = vapour.name + " (vapour, matrix)";
        if (thermochemistry_variable.isElementPresent(variable_name))
            thermochemistry_variable[variable_name].setFinalValue(
                thermochemistry_variable[variable_name].getFinalValue() + vaporised);
    }

    u_content  = std::max(0.0, u_content);
    pu_content = std::max(0.0, pu_content);
    o_content  = std::max(0.0, o_content);

    // The sample is completely vaporised when the metal or the oxygen is exhausted.
    double     metal_content_new = u_content + pu_content;
    const bool fully_vaporised   = metal_content_new <= 1.0e-9 * metal_content || o_content <= 1.0e-9 * metal_content;
    if (fully_vaporised)
        u_content = pu_content = o_content = metal_content_new = 0.0;

    sciantix_variable["U content"].setFinalValue(u_content);
    sciantix_variable["Pu content"].setFinalValue(pu_content);
    sciantix_variable["O content"].setFinalValue(o_content);

    // The O/M follows the composition of the matrix left, when part of it has vaporised
    const double total_moles_new = metal_content_new + o_content;
    if (!fully_vaporised && total_moles_new < total_moles_old)
        sciantix_variable["Stoichiometry deviation"].setFinalValue(o_content / metal_content_new - 2.0);

    // The grain shrinks with the vaporised matrix
    const double grain_radius_new = grain_radius_old * std::cbrt(total_moles_new / total_moles_old);
    if (!fully_vaporised)
        sciantix_variable["Grain radius"].setFinalValue(grain_radius_new);
    if (grain_radius_new >= grain_radius_old)
        return;

    // Fission products released by the vaporised matrix: the vaporised grain volume (in grain) and the
    // grain-boundary layer consumed by the shrinking grain (at grain boundary, reacted). The release is
    // then computed in FissionProductRelease from the remaining inventories.
    const double ig_fraction   = fully_vaporised ? 1.0 : 1.0 - std::pow(grain_radius_new / grain_radius_old, 3.0);
    const double bubble_height = (1.0 - std::cos(fuel.getSemidihedralAngle())) *
                                 sciantix_variable["Intergranular bubble radius"].getFinalValue();
    double gb_fraction =
        bubble_height > 0.0 ? 1.0 - std::exp(-(grain_radius_old - grain_radius_new) / bubble_height) : 0.0;
    if (fully_vaporised)
        gb_fraction = 1.0;

    for (auto& system : sciantix_system)
    {
        if (system.getRestructuredMatrix() != 0 || !system.isGasOrVolatileFP())
            continue;

        const std::string name = system.getFissionProductName();

        if (system.isGasFP())
        {
            for (const std::string variable : {" in grain", " in intragranular solution", " in intragranular bubbles"})
                sciantix_variable[name + variable].setFinalValue(sciantix_variable[name + variable].getFinalValue() *
                                                                 (1.0 - ig_fraction));

            double* modes          = getDiffusionModes(name);
            double* modes_solution = getDiffusionModesSolution(name);
            double* modes_bubbles  = getDiffusionModesBubbles(name);
            for (int i = 0; i < n_modes; ++i)
            {
                modes[i] *= (1.0 - ig_fraction);
                modes_solution[i] *= (1.0 - ig_fraction);
                modes_bubbles[i] *= (1.0 - ig_fraction);
            }
        }

        sciantix_variable[name + " at grain boundary"].setFinalValue(
            sciantix_variable[name + " at grain boundary"].getFinalValue() * (1.0 - gb_fraction));
        if (system.isVolatileFP())
            sciantix_variable[name + " reacted"].setFinalValue(sciantix_variable[name + " reacted"].getFinalValue() *
                                                               (1.0 - gb_fraction));
    }
}
