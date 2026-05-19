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
//  Version: 2.2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Constants.h"
#include "Simulation.h"
#include <algorithm>

namespace
{
    // COARSENING: numerical guard used by the Barani et al. two-size intragranular bubble extension.
    double coarseningPositive(const double value)
    {
        return std::max(value, 0.0);
    }

    // COARSENING: Setyawan-informed size-dependent re-solution rate used in Barani et al. (2020), Eq. (7).
    double coarseningResolutionRate(const double radius, const double fission_rate)
    {
        const double a  = 9.49e-24;  // COARSENING: m3, Barani et al. Table 1.
        const double b1 = 7.07e-2;   // COARSENING: 1/m, Barani et al. Table 1.
        const double b0 = 9.18e-23;  // COARSENING: m3, Barani et al. Table 1.
        const double c  = 7.982;     // COARSENING: 1/m2, Barani et al. Table 1.
        const double d  = 3.71e-2;   // COARSENING: 1/m2, Barani et al. Table 1.

        const double denominator = 1.0 + c * pow(radius, 2.0) * exp(-d * pow(radius, 2.0));
        const double volume_rate = (a * exp(-b1 * radius) + b0 - a) / denominator;

        return coarseningPositive(volume_rate * fission_rate);
    }

    // COARSENING: xenon pipe-diffusion coefficient close to the dislocation core, Barani et al. Table 1.
    double coarseningGasPipeDiffusivity(const double temperature)
    {
        if (temperature <= 0.0)
            return 0.0;

        return 7.74e-6 * exp(-1.21e-19 / (boltzmann_constant * temperature));
    }

    // COARSENING: vacancy pipe-diffusion coefficient along dislocations, Barani et al. Eq. (15).
    double coarseningVacancyPipeDiffusivity(const double temperature)
    {
        if (temperature <= 0.0)
            return 0.0;

        return 3.8e-2 * exp(-1.84 / (boltzmann_constant_eV * temperature));
    }

    // COARSENING: hard-sphere diameter for Xe in the Carnahan-Starling EOS, Barani et al. Eq. (11).
    double coarseningHardSphereDiameter(const double temperature)
    {
        if (temperature <= 0.0)
            return 0.0;

        return 4.45e-10 * (0.8542 - 0.03996 * log(temperature / 231.2));
    }

    // COARSENING: spherical radius from a per-bubble volume.
    double coarseningRadiusFromVolume(const double volume)
    {
        if (volume <= 0.0)
            return 0.0;

        return 0.620350491 * pow(volume, 1.0 / 3.0);
    }

    // COARSENING: spherical per-bubble volume from radius.
    double coarseningVolumeFromRadius(const double radius)
    {
        if (radius <= 0.0)
            return 0.0;

        return 4.188790205 * pow(radius, 3.0);
    }

    // COARSENING: Carnahan-Starling pressure for coarsened dislocation bubbles, Barani et al. Eq. (10).
    double coarseningBubblePressure(const double atoms_per_bubble, const double bubble_volume, const double temperature)
    {
        if (atoms_per_bubble <= 0.0 || bubble_volume <= 0.0 || temperature <= 0.0)
            return 0.0;

        const double hard_sphere_diameter = coarseningHardSphereDiameter(temperature);
        const double atomic_density       = atoms_per_bubble / bubble_volume;
        double       packing_fraction     = M_PI * pow(hard_sphere_diameter, 3.0) * atomic_density / 6.0;

        packing_fraction = std::min(std::max(packing_fraction, 0.0), 0.74);

        const double compressibility =
            (1.0 + packing_fraction + pow(packing_fraction, 2.0) - pow(packing_fraction, 3.0)) /
            pow(1.0 - packing_fraction, 3.0);

        return atoms_per_bubble * boltzmann_constant * temperature * compressibility / bubble_volume;
    }

    // COARSENING: Zullo 2026 dislocation-density correlation.
    double coarseningDislocationDensityZullo2026(const double burnup, const double temperature)
    {
        if (burnup <= 0.0 || temperature <= 0.0)
            return 0.0;

        const double A     = 6.545e12;
        const double n     = 1.151;
        const double A_inf = 0.608;
        const double T_c   = 1109.0;
        const double dT    = 25.8;
        const double fT    = A_inf + (1.0 - A_inf) / (1.0 + exp((temperature - T_c) / dT));
        return coarseningPositive(A * pow(burnup, n) * fT);
    }

    // COARSENING: Zullo - Nicodemo 2026 dislocation-density correlation with low-burnup incubation.
    double coarseningDislocationDensityZulloNicodemo2026(const double burnup, const double temperature)
    {
        const double zullo_density = coarseningDislocationDensityZullo2026(burnup, temperature);
        if (zullo_density <= 0.0)
            return 0.0;

        const double burnup_threshold = 10.0;  // COARSENING: MWd/kgUO2, no dislocation build-up below this value.
        const double burnup_width     = 10.0;  // COARSENING: MWd/kgUO2, slow sigmoid convergence to Zullo - Nicodemo 2026.
        if (burnup <= burnup_threshold)
            return 0.0;

        const double activation =
            2.0 / (1.0 + exp(-(burnup - burnup_threshold) / burnup_width)) - 1.0;
        return zullo_density * std::min(std::max(activation, 0.0), 1.0);
    }

    // COARSENING: select no/fixed/variable dislocation density for Barani-type dislocation bubbles.
    double coarseningDislocationDensity(const int option, const double burnup, const double temperature)
    {
        switch (option)
        {
            case 0:
                return 0.0;
            case 1:
                return 4.0e13;  // COARSENING: m/m3, fixed Barani et al. Table 1 value.
            case 2:
                return coarseningDislocationDensityZullo2026(burnup, temperature);
            case 3:
                return coarseningDislocationDensityZulloNicodemo2026(burnup, temperature);
            default:
                ErrorMessages::Switch(__FILE__, "iCoarseningDislocationDensity", option);
                return 0.0;
        }
    }
}  // namespace

void Simulation::IntraGranularBubbleBehavior()
{
    // Model declaration
    Model model_;

    model_.setName("Intragranular bubble behavior");

    std::string         reference;
    std::vector<double> parameter;

    switch (int(input_variable["iIntraGranularBubbleBehavior"].getValue()))
    {
        case 0:
        {
            reference += ": Constant bubble concentration and radius.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(7.0e23);
            sciantix_variable["Intragranular bubble radius"].setInitialValue(1.0e-9);

            sciantix_variable["Intragranular bubble concentration"].setFinalValue(7.0e23);
            sciantix_variable["Intragranular bubble radius"].setFinalValue(1.0e-9);

            parameter.push_back(0.);
            parameter.push_back(0.);

            break;
        }

        case 1:
        {
            reference += ": Pizzocri et al., JNM, 502 (2018) 323-330.";

            parameter.push_back(sciantix_system[0].getResolutionRate());
            parameter.push_back(sciantix_system[0].getNucleationRate());

            break;
        }

        case 2:
        {
            reference += "White and Tucker, JNM, 118 (1983), 1-38.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(
                1.52e+27 / history_variable["Temperature"].getFinalValue() - 3.3e+23);
            parameter.push_back(0.0);
            parameter.push_back(0.0);

            break;
        }

        case 3:
        {
            reference += "Case specific for annealing experiments and helium intragranular behaviour.";

            if (physics_variable["Time step"].getFinalValue() > 0.0)
                parameter.push_back((1.0 / sciantix_variable["Intragranular similarity ratio"].getFinalValue() - 1.0) /
                                    physics_variable["Time step"].getFinalValue());
            else
                parameter.push_back(0.);

            parameter.push_back(0.);

            break;
        }

        case 4:
        {
            // COARSENING: Barani et al. (2020) two-size model, retaining the bulk population and adding dislocation bubbles.
            reference += "Barani et al., JNM, 538 (2020) 152195. COARSENING.";

            // COARSENING: iResolutionRate = 4 selects the Setyawan-informed size-dependent resolution.
            const int resolution_model = int(input_variable["iResolutionRate"].getValue());
            if (resolution_model == 4)
                parameter.push_back(
                    coarseningResolutionRate(sciantix_variable["Intragranular bubble radius"].getInitialValue(),
                                             history_variable["Fission rate"].getFinalValue()));
            else
                parameter.push_back(sciantix_system[0].getResolutionRate());
            parameter.push_back(sciantix_system[0].getNucleationRate());

            break;
        }

        case 99:
        {
            reference += "No intragranular bubbles.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(0.0);
            sciantix_variable["Intragranular bubble radius"].setInitialValue(0.0);
            sciantix_variable["Intragranular atoms per bubble"].setInitialValue(0.0);

            sciantix_variable["Intragranular bubble concentration"].setFinalValue(0.0);
            sciantix_variable["Intragranular bubble radius"].setFinalValue(0.0);
            sciantix_variable["Intragranular atoms per bubble"].setFinalValue(0.0);

            parameter.push_back(0.);
            parameter.push_back(0.);

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__,
                                  "iIntraGranularBubbleBehavior",
                                  int(input_variable["iIntraGranularBubbleBehavior"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    // dN / dt = - getParameter().at(0) * N + getParameter().at(1)
    sciantix_variable["Intragranular bubble concentration"].setFinalValue(
        solver.Decay(sciantix_variable["Intragranular bubble concentration"].getInitialValue(),
                     model["Intragranular bubble behavior"].getParameter().at(0),
                     model["Intragranular bubble behavior"].getParameter().at(1),
                     physics_variable["Time step"].getFinalValue()));

    // Atom per bubbles and bubble radius
    for (auto& system : sciantix_system)
    {
        if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            if (sciantix_variable["Intragranular bubble concentration"].getFinalValue() > 0.0)
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].setFinalValue(
                    sciantix_variable[system.getGasName() + " in intragranular bubbles"].getFinalValue() /
                    sciantix_variable["Intragranular bubble concentration"].getFinalValue());

            else
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].setFinalValue(0.0);

            sciantix_variable["Intragranular bubble volume"].addValue(
                system.getVolumeInLattice() *
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].getFinalValue());
        }
    }

    // Intragranular bubble radius
    sciantix_variable["Intragranular bubble radius"].setFinalValue(
        0.620350491 * pow(sciantix_variable["Intragranular bubble volume"].getFinalValue(), (1.0 / 3.0)));

    // Intragranular gaseous swelling
    // 4/3 pi N R^3
    sciantix_variable["Intragranular gas bubble swelling"].setFinalValue(
        4.188790205 * pow(sciantix_variable["Intragranular bubble radius"].getFinalValue(), 3) *
        sciantix_variable["Intragranular bubble concentration"].getFinalValue());

    if (int(input_variable["iIntraGranularBubbleBehavior"].getValue()) == 4)
    {
        // COARSENING: initialize and evolve the dislocation-bubble population from Barani et al. (2020).
        const double time_step    = physics_variable["Time step"].getFinalValue();
        const double temperature  = history_variable["Temperature"].getFinalValue();
        const double fission_rate = history_variable["Fission rate"].getFinalValue();

        const int coarsening_nucleation_model =
            int(input_variable["iNucleationRate"].getValue());  // COARSENING: option 2 enables Barani nucleation.
        const int coarsening_resolution_model =
            int(input_variable["iResolutionRate"].getValue());  // COARSENING: option 4 enables Setyawan resolution.
        const int coarsening_trapping_model =
            int(input_variable["iTrappingRate"].getValue());  // COARSENING: option 2 enables Barani trapping.
        const int coarsening_dislocation_model =
            int(input_variable["iCoarseningDislocationDensity"].getValue());  // COARSENING.
>>>>>>> 6b6daddd (Disclocation law added)

        const double dislocation_density =
            coarseningDislocationDensity(coarsening_dislocation_model,
                                         sciantix_variable["Burnup"].getFinalValue(),
                                         temperature);
        const double bubbles_per_dislocation         = 1.0e6;   // COARSENING: bubble/m, Barani et al. Table 1.
        const double initial_dislocation_bubbles     = bubbles_per_dislocation * dislocation_density;
        const double burgers_vector                  = 3.85e-10;  // COARSENING: m, Barani et al. Table 1.
        const double dislocation_core_radius         = 5.0 * burgers_vector;
        const double uo2_gas_surface_energy          = 0.7;       // COARSENING: J/m2, Barani et al. Table 1.
        const double vacancy_volume                  = 4.09e-29;  // COARSENING: m3/vacancy, Barani et al. Table 1.
        const double gas_atom_volume_in_bulk_bubbles = 4.09e-29;  // COARSENING: m3/atom, Barani et al. Table 1.

        double dislocation_bubble_concentration =
            sciantix_variable["Intragranular coarsened bubble concentration"].getInitialValue();
        if (coarsening_nucleation_model == 2 && dislocation_density > 0.0 && fission_rate > 0.0)
            dislocation_bubble_concentration = std::max(dislocation_bubble_concentration, initial_dislocation_bubbles);

        sciantix_variable["Dislocation density"].setFinalValue(dislocation_density);

        if (dislocation_density <= 0.0 || dislocation_bubble_concentration <= 0.0)
        {
            // COARSENING: no dislocations means model 4 falls back to the legacy intragranular bulk-bubble solution.
            sciantix_variable["Intragranular bulk gas bubble swelling"].setFinalValue(
                sciantix_variable["Intragranular gas bubble swelling"].getFinalValue());
            sciantix_variable["Intragranular coarsened bubble concentration"].setFinalValue(0.0);
            sciantix_variable["Intragranular coarsened bubble radius"].setFinalValue(0.0);
            sciantix_variable["Intragranular coarsened atoms per bubble"].setFinalValue(0.0);
            sciantix_variable["Intragranular coarsened vacancies per bubble"].setFinalValue(0.0);
            sciantix_variable["Intragranular coarsened gas bubble swelling"].setFinalValue(0.0);
            sciantix_variable["Intragranular gas in coarsened bubbles"].setFinalValue(0.0);
            sciantix_variable["Intragranular coarsened bubble pressure"].setFinalValue(0.0);
            sciantix_variable["Intragranular coarsened bubble equilibrium pressure"].setFinalValue(0.0);
        }
        else
        {

            double coarsened_gas =
                coarseningPositive(sciantix_variable["Intragranular gas in coarsened bubbles"].getInitialValue());

            double total_gas_in_bubbles  = 0.0;
            double total_gas_in_solution = 0.0;
            for (auto& system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    total_gas_in_bubbles += coarseningPositive(
                        sciantix_variable[system.getGasName() + " in intragranular bubbles"].getFinalValue());
                    total_gas_in_solution += coarseningPositive(
                        sciantix_variable[system.getGasName() + " in intragranular solution"].getFinalValue());
                }
            }

            coarsened_gas = std::min(coarsened_gas, total_gas_in_bubbles);

            const double bulk_bubble_concentration =
                coarseningPositive(sciantix_variable["Intragranular bubble concentration"].getFinalValue());
            const double bulk_bubble_radius =
                coarseningPositive(sciantix_variable["Intragranular bubble radius"].getFinalValue());
            const double dislocation_bubble_radius_i =
                coarseningPositive(sciantix_variable["Intragranular coarsened bubble radius"].getInitialValue());

            const double gas_diffusivity =
                sciantix_system[0].getFissionGasDiffusivity() * sciantix_system[0].getGas().getPrecursorFactor();
            const double pipe_gas_diffusivity = coarseningGasPipeDiffusivity(temperature);
            const double wigner_seitz_dislocation_radius =
                (dislocation_density > 0.0) ? 1.0 / sqrt(M_PI * dislocation_density) : 0.0;
            const double pipe_trapping_denominator =
                std::max(log(wigner_seitz_dislocation_radius / dislocation_core_radius) - 0.75, 1.0);

            double bulk_trapping_rate                = 0.0;
            double dislocation_bubble_trapping_rate  = 0.0;
            double dislocation_line_trapping_rate    = 0.0;
            if (coarsening_trapping_model == 2)
            {
                // COARSENING: Barani et al. trapping to bulk bubbles, dislocation bubbles, and dislocation lines.
                bulk_trapping_rate = 4.0 * M_PI * gas_diffusivity * bulk_bubble_radius * bulk_bubble_concentration;
                dislocation_bubble_trapping_rate = 4.0 * M_PI * gas_diffusivity *
                                                   std::max(dislocation_bubble_radius_i, bulk_bubble_radius) *
                                                   dislocation_bubble_concentration;
                dislocation_line_trapping_rate =
                    2.0 * M_PI * pipe_gas_diffusivity * dislocation_density / pipe_trapping_denominator;
            }
            const double total_trapping_rate =
                bulk_trapping_rate + dislocation_bubble_trapping_rate + dislocation_line_trapping_rate;

            if (total_gas_in_bubbles > 0.0 && total_trapping_rate > 0.0 && time_step > 0.0)
            {
                // COARSENING: relax the split of trapped gas between bulk and dislocation bubbles using Eq. (19) rates.
                const double coarsened_fraction =
                    (dislocation_bubble_trapping_rate + dislocation_line_trapping_rate) / total_trapping_rate;
                const double target_coarsened_gas = total_gas_in_bubbles * coarsened_fraction;
                const double coarsening_resolution_rate =
                    (coarsening_resolution_model == 4) ? coarseningResolutionRate(dislocation_bubble_radius_i, fission_rate)
                                                       : 0.0;
                const double relaxation =
                    1.0 - exp(-(total_trapping_rate + coarsening_resolution_rate) * time_step);
                coarsened_gas += (target_coarsened_gas - coarsened_gas) * std::min(std::max(relaxation, 0.0), 1.0);
                coarsened_gas = std::min(std::max(coarsened_gas, 0.0), total_gas_in_bubbles);
            }

            double coarsened_atoms_per_bubble = 0.0;
            if (dislocation_bubble_concentration > 0.0)
                coarsened_atoms_per_bubble = coarsened_gas / dislocation_bubble_concentration;

            double coarsened_vacancies_per_bubble =
                coarseningPositive(sciantix_variable["Intragranular coarsened vacancies per bubble"].getInitialValue());
            double coarsened_bubble_volume = coarsened_atoms_per_bubble * gas_atom_volume_in_bulk_bubbles +
                                             coarsened_vacancies_per_bubble * vacancy_volume;

            double coarsened_bubble_radius = coarseningRadiusFromVolume(coarsened_bubble_volume);
            double coarsened_bubble_pressure =
                coarseningBubblePressure(coarsened_atoms_per_bubble, coarsened_bubble_volume, temperature);
            double equilibrium_pressure = 0.0;
            if (coarsened_bubble_radius > 0.0)
                equilibrium_pressure = 2.0 * uo2_gas_surface_energy / coarsened_bubble_radius -
                                       history_variable["Hydrostatic stress"].getFinalValue() * 1.0e6;

            if (time_step > 0.0 && dislocation_bubble_concentration > 0.0 &&
                coarsened_bubble_pressure > equilibrium_pressure)
            {
                // COARSENING: vacancy absorption along dislocations follows the Speight-Beere form, Eq. (8).
                const double dislocation_bubble_cell_radius =
                    pow(3.0 / (4.0 * M_PI * dislocation_bubble_concentration), 1.0 / 3.0);
                const double psi           = std::min(coarsened_bubble_radius / dislocation_bubble_cell_radius, 0.95);
                const double z_denominator = std::max(-pow(psi, 6.0) + 5.0 * pow(psi, 2.0) - 9.0 * psi + 5.0, 1.0e-12);
                const double sink_strength = 10.0 * psi * (1.0 + pow(psi, 3.0)) / z_denominator;
                const double vacancy_absorption_rate =
                    2.0 * M_PI * coarseningVacancyPipeDiffusivity(temperature) * dislocation_bubble_cell_radius *
                    (coarsened_bubble_pressure - equilibrium_pressure) /
                    (boltzmann_constant * temperature * std::max(sink_strength, 1.0e-12));

                const double previous_volume = coarsened_bubble_volume;
                coarsened_vacancies_per_bubble += coarseningPositive(vacancy_absorption_rate) * time_step;
                coarsened_bubble_volume = coarsened_atoms_per_bubble * gas_atom_volume_in_bulk_bubbles +
                                          coarsened_vacancies_per_bubble * vacancy_volume;

                // COARSENING: limit explicit vacancy-growth overshoot during sharp White power ramps.
                const double previous_radius      = coarseningRadiusFromVolume(previous_volume);
                const double max_radius_increment = 2.0e-10;  // COARSENING: stable explicit growth limiter per time step.
                const double max_coarsened_radius = 2.5e-7;
                coarsened_bubble_radius           = std::min(coarseningRadiusFromVolume(coarsened_bubble_volume),
                                                   std::min(previous_radius + max_radius_increment, max_coarsened_radius));
                coarsened_bubble_volume           = coarseningVolumeFromRadius(coarsened_bubble_radius);
                coarsened_vacancies_per_bubble    = coarseningPositive(
                    (coarsened_bubble_volume - coarsened_atoms_per_bubble * gas_atom_volume_in_bulk_bubbles) /
                    vacancy_volume);

                // COARSENING: coalescence between dislocation bubbles, Barani et al. Eq. (17).
                const double volume_increment = coarseningPositive(coarsened_bubble_volume - previous_volume);
                const double dislocation_porosity =
                    std::min(dislocation_bubble_concentration * coarsened_bubble_volume, 0.95);
                const double lambda = (2.0 - dislocation_porosity) / (2.0 * pow(1.0 - dislocation_porosity, 3.0));
                dislocation_bubble_concentration =
                    dislocation_bubble_concentration /
                    (1.0 + 4.0 * lambda * dislocation_bubble_concentration * volume_increment);

                // COARSENING: capture of bulk bubbles by expanding dislocation bubbles, Barani et al. Eq. (18).
                const double radius_increment = coarseningPositive(coarsened_bubble_radius - dislocation_bubble_radius_i);
                const double interaction_volume_increment =
                    4.0 * M_PI * pow(coarsened_bubble_radius + bulk_bubble_radius, 2.0) * radius_increment;
                const double captured_bulk_fraction =
                    std::min(dislocation_bubble_concentration * interaction_volume_increment, 1.0);
                const double bulk_gas_before_capture = total_gas_in_bubbles - coarsened_gas;
                coarsened_gas += bulk_gas_before_capture * captured_bulk_fraction;
                coarsened_gas = std::min(std::max(coarsened_gas, 0.0), total_gas_in_bubbles);

                if (dislocation_bubble_concentration > 0.0)
                    coarsened_atoms_per_bubble = coarsened_gas / dislocation_bubble_concentration;
                coarsened_bubble_volume = coarsened_atoms_per_bubble * gas_atom_volume_in_bulk_bubbles +
                                          coarsened_vacancies_per_bubble * vacancy_volume;
                coarsened_bubble_radius = coarseningRadiusFromVolume(coarsened_bubble_volume);
                coarsened_bubble_pressure =
                    coarseningBubblePressure(coarsened_atoms_per_bubble, coarsened_bubble_volume, temperature);
                if (coarsened_bubble_radius > 0.0)
                    equilibrium_pressure = 2.0 * uo2_gas_surface_energy / coarsened_bubble_radius -
                                           history_variable["Hydrostatic stress"].getFinalValue() * 1.0e6;
            }

            const double bulk_gas              = coarseningPositive(total_gas_in_bubbles - coarsened_gas);
            double       bulk_atoms_per_bubble = 0.0;
            if (bulk_bubble_concentration > 0.0)
                bulk_atoms_per_bubble = bulk_gas / bulk_bubble_concentration;

            const double bulk_bubble_volume   = bulk_atoms_per_bubble * gas_atom_volume_in_bulk_bubbles;
            const double updated_bulk_radius  = coarseningRadiusFromVolume(bulk_bubble_volume);
            const double bulk_bubble_swelling = bulk_bubble_concentration * bulk_bubble_volume;
            const double coarsened_swelling   = dislocation_bubble_concentration * coarsened_bubble_volume;

            // COARSENING: publish the two-size state while keeping legacy intragranular swelling as the total.
            sciantix_variable["Intragranular bubble radius"].setFinalValue(updated_bulk_radius);
            sciantix_variable["Intragranular atoms per bubble"].setFinalValue(bulk_atoms_per_bubble);
            sciantix_variable["Intragranular bulk gas bubble swelling"].setFinalValue(bulk_bubble_swelling);
            sciantix_variable["Intragranular coarsened bubble concentration"].setFinalValue(
                dislocation_bubble_concentration);
            sciantix_variable["Intragranular coarsened bubble radius"].setFinalValue(coarsened_bubble_radius);
            sciantix_variable["Intragranular coarsened atoms per bubble"].setFinalValue(coarsened_atoms_per_bubble);
            sciantix_variable["Intragranular coarsened vacancies per bubble"].setFinalValue(coarsened_vacancies_per_bubble);
            sciantix_variable["Intragranular coarsened gas bubble swelling"].setFinalValue(coarsened_swelling);
            sciantix_variable["Intragranular gas in coarsened bubbles"].setFinalValue(coarsened_gas);
            sciantix_variable["Intragranular coarsened bubble pressure"].setFinalValue(coarsened_bubble_pressure / 1.0e6);
            sciantix_variable["Intragranular coarsened bubble equilibrium pressure"].setFinalValue(equilibrium_pressure /
                                                                                                   1.0e6);
            sciantix_variable["Intragranular gas bubble swelling"].setFinalValue(bulk_bubble_swelling + coarsened_swelling);
        }
    }

    if (sciantix_variable["He in intragranular bubbles"].getInitialValue() > 0.0)
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(
            sqrt(sciantix_variable["He in intragranular bubbles"].getFinalValue() /
                 sciantix_variable["He in intragranular bubbles"].getInitialValue()));
    else
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(0.0);
}
