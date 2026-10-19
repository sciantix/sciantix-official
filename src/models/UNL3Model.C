#include "UNL3Model.h"
#include "UNBubblePhysics.h"
#include "UNGrainFace.h"
#include "UNTransport.h"
#include "Solver.h"
#include <algorithm>
#include <cmath>
#include <fstream>
#include <iomanip>
#include <stdexcept>

namespace un_l3
{
namespace
{
double average(const std::vector<double>& modes)
{
    const double projection = -2.0 * std::sqrt(2.0 / M_PI);
    double result = 0.0;
    for (std::size_t i = 0; i < modes.size(); ++i)
    {
        const double n = static_cast<double>(i + 1);
        result += projection * (std::pow(-1.0, n) / n) * modes[i] / ((4.0 / 3.0) * M_PI);
    }
    return result;
}

void writeTable(const std::string& name, const std::map<std::string,double>& values, bool initial)
{
    std::ofstream file(name, initial ? std::ios::out : std::ios::app);
    if (!file) throw std::runtime_error("Cannot open UN L3 output: " + name);
    file << std::setprecision(17);
    if (initial)
    {
        for (const auto& item : values) file << item.first << '\t';
        file << '\n';
    }
    for (const auto& item : values)
    {
        if (!std::isfinite(item.second)) throw std::runtime_error("Non-finite UN L3 output: " + item.first);
        file << item.second << '\t';
    }
    file << '\n';
    if (!file) throw std::runtime_error("Cannot write UN L3 output: " + name);
}

void writeModes(const std::string& name, const std::array<std::vector<double>,3>& modes, bool initial)
{
    std::ofstream file(name, std::ios::binary | (initial ? std::ios::out : std::ios::app));
    for (const auto& population : modes)
        file.write(reinterpret_cast<const char*>(population.data()), population.size() * sizeof(double));
    if (!file) throw std::runtime_error("Cannot write UN L3 modes: " + name);
}
}

Model::Model(const double history[], const double variables[], const double scaling[]) : state_(160)
{
    parameters_.temperature = history[1];
    parameters_.fission_rate = history[3];
    parameters_.hydrostatic_stress = history[5];
    parameters_.grain_radius = variables[0];
    if (!std::isfinite(parameters_.temperature) || parameters_.temperature <= 0.0 ||
        !std::isfinite(parameters_.fission_rate) || parameters_.fission_rate < 0.0 ||
        !std::isfinite(parameters_.grain_radius) || parameters_.grain_radius <= 0.0 ||
        !std::isfinite(parameters_.hydrostatic_stress))
        throw std::invalid_argument("Invalid UN L3 temperature, fission rate, stress or grain radius");
    // This selector exposes the frozen, fresh-fuel L3 configuration. Reject
    // unsupported initial data/scaling rather than silently ignoring it.
    for (int i = 1; i <= 20; ++i)
        if (variables[i] != 0.0) throw std::invalid_argument("UN L3 requires fresh-fuel gas/bubble initial conditions");
    if (variables[38] != 0.0 || variables[65] != 0.0)
        throw std::invalid_argument("UN L3 requires zero initial burnup and irradiation time");
    for (int i = 0; i < 9; ++i)
        if (scaling[i] != 1.0) throw std::invalid_argument("UN L3 uses its local frozen scaling factors");
    state_.dislocation.number_density = parameters_.K_d * parameters_.rho_d;
    state_.grain_face = initializeGrainFace(parameters_, options_);
    refreshHistory();
}

void Model::execute(Solver& solver, const double history[])
{
    if (history[1] != parameters_.temperature || history[3] != parameters_.fission_rate ||
        history[5] != parameters_.hydrostatic_stress)
        throw std::invalid_argument("UN L3 currently requires constant temperature, fission rate and stress");
    const double next_dt = history[6];
    if (!std::isfinite(next_dt) || next_dt < 0.0 || next_dt > nominalTimeStep())
        throw std::invalid_argument("Invalid UN L3 timestep");
    if (next_dt == 0.0)
    {
        if (step_count_ != 0 || history[8] != 0.0 || history[7] != 0.0)
            throw std::invalid_argument("UN L3 zero dt is only valid at initialization");
        return;
    }
    if (history[8] != static_cast<double>(step_count_ + 1) ||
        std::abs(history[7] * 3600.0 - (state_.time_s + next_dt)) >
            32.0 * 2.220446049250313e-16 * std::max(state_.time_s + next_dt, 1.0))
        throw std::invalid_argument("UN L3 duplicate, skipped or inconsistent temporal call");

    dt_ = next_dt;
    diagnostics_ = advanceIntraGranular(solver, state_, parameters_, options_, dt_);
    const double fraction = diagnostics_.values.at("f_cap");
    capture_fraction_sum_ += fraction;
    capture_raw_sum_ += diagnostics_.values.at("cap_raw_step");
    max_capture_ = std::max(max_capture_, fraction);
    captured_bubbles_ += diagnostics_.values.at("captured_bubbles");

    // Exact Python orchestration: production, gas deficit from the scalar
    // exchange/capture result, incoming increment, then grain-face evolution.
    state_.generated += productionRate(parameters_) * dt_;
    const double retained = std::max(state_.averages[0], 0.0) +
        std::max(state_.averages[1], 0.0) + std::max(state_.averages[2], 0.0);
    const double target = std::max(state_.initial_gas + state_.generated - retained, 0.0);
    const double before = state_.grain_face.gas + state_.grain_face.released;
    const double incoming = std::max(target - before, 0.0);
    diagnostics_.values["q_gb_total_target"] = target;
    diagnostics_.values["dq_to_gb"] = incoming;
    advanceGrainFace(parameters_, options_, state_.grain_face, incoming, dt_, diagnostics_.values.at("Dv1"));
    state_.gas_to_boundary = state_.grain_face.gas + state_.grain_face.released;
    state_.time_s += dt_;
    ++step_count_;
    refreshHistory();
}

void Model::refreshHistory()
{
    refreshGrainFace(parameters_, options_, state_.grain_face);
    auto& h = history_values_;
    const auto& s = state_;
    const auto& g = s.grain_face;
    const auto rate = [&](const std::string& key) {
        const auto it = diagnostics_.values.find(key);
        return it == diagnostics_.values.end() ? 0.0 : it->second;
    };
    const double c = average(s.modes[0]), mb = average(s.modes[1]), md = average(s.modes[2]);
    const double sb = s.bulk.number_density * s.bulk.volume, sd = s.dislocation.number_density * s.dislocation.volume;
    const double sg = g.number_volumetric * g.volume;
    h["time"] = s.time_s;
    h["burnup_percent_fima"] = 100.0 * parameters_.fission_rate * s.time_s / (4.0 / std::pow(parameters_.lattice_parameter, 3));
    h["rho_d"] = h["rho_d_next"] = parameters_.rho_d;
    h["delta_Nd_rho"] = 0.0;
    h["c"] = c; h["mb"] = mb; h["md"] = md;
    h["Nb"] = s.bulk.number_density; h["Nd"] = s.dislocation.number_density;
    h["Vb"] = s.bulk.volume; h["Vd"] = s.dislocation.volume;
    h["Rb"] = s.bulk.radius; h["Rd"] = s.dislocation.radius;
    h["nvb"] = s.bulk.vacancies; h["nvd"] = s.dislocation.vacancies;
    h["generated"] = s.generated; h["retained_ig"] = c + mb + md;
    h["retained"] = c + mb + md + g.gas;
    h["q_gb"] = s.gas_to_boundary; h["q_gf"] = g.gas; h["q_rel"] = g.released;
    h["swelling_b"] = sb; h["swelling_d"] = sd; h["swelling_gf"] = sg;
    h["swelling_ig"] = sb + sd; h["swelling_total_gas"] = sb + sd + sg;
    h["Ngf_areal"] = g.number_areal; h["Ngf_vol"] = g.number_volumetric;
    h["ng_gf"] = g.atoms_per_bubble; h["nv_gf"] = g.vacancies_per_bubble;
    h["V_gf"] = g.volume; h["R_gf"] = g.radius; h["A_gf"] = g.area; h["Fc_gf"] = g.coverage;
    h["p_gf"] = g.pressure; h["p_gf_eq"] = g.equilibrium_pressure; h["zeta_gf"] = g.zeta;
    h["gf_saturated"] = g.saturation_reached;
    h["p_b"] = internalPressure(parameters_, mb, s.bulk.vacancies);
    h["p_d"] = internalPressure(parameters_, md, s.dislocation.vacancies);
    h["p_b_eq"] = equilibriumPressure(parameters_, s.bulk.radius);
    h["p_d_eq"] = equilibriumPressure(parameters_, s.dislocation.radius);
    h["pressure_ratio_b"] = h["p_b"] / h["p_b_eq"];
    h["pressure_ratio_d"] = h["p_d"] / h["p_d_eq"];
    h["vacancies_per_bubble_b"] = s.bulk.number_density > 0.0 ? s.bulk.vacancies / s.bulk.number_density : 0.0;
    h["vacancies_per_bubble_d"] = s.dislocation.number_density > 0.0 ? s.dislocation.vacancies / s.dislocation.number_density : 0.0;
    h["lambda_d"] = rate("lambda_d"); h["nu_b"] = rate("nu_b");
    h["phi_b"] = rate("phi_b"); h["phi_d"] = rate("phi_d");
    h["f_cap_step"] = rate("f_cap"); h["cap_raw_step"] = rate("cap_raw_step");
    h["capture_fraction_sum"] = capture_fraction_sum_; h["capture_raw_sum"] = capture_raw_sum_;
    h["capture_bubbles_cumulative"] = captured_bubbles_; h["max_f_cap_step"] = max_capture_;
    h["matrix_gas_percent"] = s.generated > 0.0 ? 100.0 * c / s.generated : 0.0;
    h["bulk_gas_percent"] = s.generated > 0.0 ? 100.0 * mb / s.generated : 0.0;
    h["dislocation_gas_percent"] = s.generated > 0.0 ? 100.0 * md / s.generated : 0.0;
    h["qgb_gas_percent"] = s.generated > 0.0 ? 100.0 * s.gas_to_boundary / s.generated : 0.0;
    h["grainface_gas_percent"] = s.generated > 0.0 ? 100.0 * g.gas / s.generated : 0.0;
    h["release_gas_percent"] = s.generated > 0.0 ? 100.0 * g.released / s.generated : 0.0;
    h["mass_balance_residual"] = s.initial_gas + s.generated - (c + mb + md + g.gas + g.released);
    h["qv_gf"] = g.vacancies;
    h["bulk_first_gas_seed_done"] = s.bulk_first_gas_seed_done;
    h["dislocation_first_gas_seed_done"] = s.dislocation_first_gas_seed_done;
    h["step_count"] = static_cast<double>(step_count_); h["initialization_count"] = 1.0;
    h["dt"] = dt_;
}

void Model::exportVariables(double v[]) const
{
    const auto& h = history_values_;
    v[0] = parameters_.grain_radius; v[1] = h.at("generated"); v[2] = h.at("retained_ig");
    v[3] = h.at("c"); v[4] = h.at("mb") + h.at("md"); v[5] = h.at("q_gf"); v[6] = h.at("q_rel");
    v[19] = h.at("Nb"); v[20] = h.at("Rb"); v[24] = h.at("swelling_ig");
    v[25] = h.at("Ngf_areal"); v[26] = h.at("ng_gf"); v[30] = h.at("nv_gf");
    v[31] = h.at("R_gf"); v[32] = h.at("A_gf"); v[33] = h.at("V_gf");
    v[34] = h.at("Fc_gf"); v[35] = options_.gf_coverage_saturation; v[36] = h.at("swelling_gf");
    v[65] = state_.time_s / 3600.0; v[69] = h.at("burnup_percent_fima");
    v[170] = h.at("md"); v[171] = h.at("Nd"); v[172] = h.at("Rd"); v[173] = h.at("Vd");
    v[176] = h.at("swelling_d"); v[177] = h.at("swelling_b");
    v[179] = h.at("p_b"); v[180] = h.at("p_b_eq"); v[181] = h.at("p_d"); v[182] = h.at("p_d_eq");
    v[183] = h.at("rho_d"); v[184] = h.at("q_gb"); v[193] = h.at("q_gf"); v[194] = h.at("q_rel");
    v[195] = h.at("release_gas_percent") / 100.0; v[196] = h.at("swelling_total_gas");
}

void Model::output(const std::string& directory)
{
    const bool initial = step_count_ == 0;
    writeTable(directory + "output.txt", history_values_, initial);
    writeModes(directory + "un_l3_modes.bin", state_.modes, initial);
    if (!initial)
    {
        writeTable(directory + "un_l3_diagnostics.txt", diagnostics_.values, step_count_ == 1);
        writeModes(directory + "un_l3_exchange_modes.bin", diagnostics_.after_exchange, step_count_ == 1);
    }
}
}
