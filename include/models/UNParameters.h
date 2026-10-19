#ifndef SCIANTIX_UN_L3_PARAMETERS_H
#define SCIANTIX_UN_L3_PARAMETERS_H

#include <string>

namespace un_l3
{
// SI parameters of the approved L3 notebook, kept local to the new UN modules.
// No shared SCIANTIX/UO2 constant is changed. The test harness loads all these
// values explicitly from the frozen per-case manifests.
struct Parameters
{
    double temperature = 1600.0, fission_rate = 5.767844763153473e19, grain_radius = 6e-6;
    double xe_yield = 0.24, precursor_factor = 1.0;
    double kB_eV = 8.617333262e-5, kB_J = 1.380649e-23;
    double D10 = 1.56e-3, Q1 = 4.94, A20_xe = 1.21e-67;
    double B21_xe = 25.87, B22_xe = -1.49, B23_xe = 0.0, A30 = 1.85e-39;
    double D2_xe_scale = 0.0, Dg_scale = 1.0, Dg_D1_scale = 1.0, Dg_D3_scale = 1.0;
    double Dg_dislocation_scale = 17.0, Dg_extra_scale = 1.0;
    double D10_vU = 1.35e-2, Q1_vU = 5.66, A20_vU = 4.6304523933553033e-29;
    double B21_vU = -0.62, B22_vU = -0.04;
    double Dv_scale = 1.0, Dv_D1_scale = 1.0, Dv_D2_scale = 1.0, Dv_dislocation_scale = 10.0;
    double radius_in_lattice = 0.21e-9, omega_fg = 8.5e-29, lattice_parameter = 4.889e-10;
    double gamma_b = 1.11, hydrostatic_stress = 0.0, min_radius_for_pressure = 1e-15;
    double f_n = 4.2e-4, rho_d = 3.5e13, K_d = 4.1e5, r_d = 3.46e-10, Z_d = 5.0;
    double gb_scale = 1.0, gd_scale = 1.0, b_scale = 1.0, b_bulk_scale = 1.0, b_dislocation_scale = 1.0;
    double gd_bubble_scale = 1.0, gd_line_scale = 1.0, gd_line_alpha = 1.0;
    double coalescence_d_scale = 1.0, capture_scale = 1.0, min_number_density = 0.0, min_volume = 0.0;
    bool vacancy_absorption_only = false, update_bulk_vacancies = true;
    std::string xe_diffusivity_mode = "rizk2025_refit_plot";
    std::string vacancy_diffusivity_mode = "rizk2025_refit_full";
};

struct PhysicsOptions
{
    bool phi_gas_resolution = false, nucleation_mass_coupling = true, bulk_dislocation_capture = true;
    bool first_gas_pressure_seed = true, seed_bulk_on_first_gas = true, seed_dislocation_on_first_gas = true;
    double first_gas_pressure_factor = 1.0 / 3.0, phi_bulk_scale = 1.0, nucleation_cluster_size = 2.0;
    bool ws_trapping_bulk = false, ws_trapping_dislocation = false, zeta_overflow_guard = true;
    double ws_max_factor = 10.0;
    std::string gb_diffusivity_mode = "rizk_legacy_1e6_Dv1";
    double gb_multiplier = 1e6, gb_arrhenius_D0 = 325.0, gb_arrhenius_Q = 5.95;
    bool intergranular_model = true, lenticular_cos3 = true, gf_vacancy_absorption_only = false;
    double gf_number_areal_initial = 1e13, gf_delta_gb = 4e-10, gf_coverage_saturation = 0.5;
    double gf_theta_degrees = 59.0, gf_radius_initial = 2.42e-10;
};

// Frozen inputs to one exchange step. No transport or bubble physics is
// evaluated here. Sources may be signed (e.g. the dissolved nucleation sink).
struct ExchangeCoefficients
{
    double diffusivity;       // m^2/s
    double grain_radius;      // m
    double source_c;          // atoms/(m^3 s)
    double trapping_bulk;     // 1/s
    double trapping_dislocation;
    double resolution_bulk;   // 1/s; gas rate, already including the chosen closure
    double resolution_dislocation;
    double source_mb;         // atoms/(m^3 s)
    double source_md;
};
}  // namespace un_l3

#endif
