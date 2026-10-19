#include "UNIntraGranular.h"
#include "Solver.h"
#include "UNTransport.h"
#include "UNBubblePhysics.h"

#include <cmath>
#include <algorithm>
#include <stdexcept>
#include <vector>

namespace un_l3
{
void advanceExchange(Solver& solver, State& state, const ExchangeCoefficients& c, double dt)
{
    const double inputs[] = {dt, c.diffusivity, c.grain_radius, c.source_c,
                             c.trapping_bulk, c.trapping_dislocation,
                             c.resolution_bulk, c.resolution_dislocation, c.source_mb, c.source_md};
    for (double value : inputs)
        if (!std::isfinite(value))
            throw std::invalid_argument("Non-finite UN exchange input");
    if (dt < 0.0 || c.diffusivity < 0.0 || c.grain_radius <= 0.0 ||
        c.trapping_bulk < 0.0 || c.trapping_dislocation < 0.0 ||
        c.resolution_bulk < 0.0 || c.resolution_dislocation < 0.0)
        throw std::invalid_argument("Invalid UN exchange coefficient or timestep");
    for (const auto& population : state.modes)
    {
        if (population.size() != state.numberOfModes())
            throw std::invalid_argument("UN spectral vector size does not match state mode count");
        for (double value : population)
            if (!std::isfinite(value))
                throw std::invalid_argument("Non-finite UN spectral coefficient");
    }

    const std::vector<double> parameters = {
        static_cast<double>(state.numberOfModes()), c.diffusivity, c.grain_radius, c.source_c,
        c.trapping_bulk, c.trapping_dislocation, c.resolution_bulk, c.resolution_dislocation,
        c.source_mb, c.source_md};
    solver.SpectralDiffusion3equationsExchange(
        state.averages[0], state.averages[1], state.averages[2],
        state.modes[0].data(), state.modes[1].data(), state.modes[2].data(), parameters, dt);

    for (double value : state.averages)
        if (!std::isfinite(value))
            throw std::runtime_error("Non-finite UN exchange result");
    for (const auto& population : state.modes)
        for (double value : population)
            if (!std::isfinite(value))
                throw std::runtime_error("Non-finite UN exchange mode result");
}

IntraGranularDiagnostics advanceIntraGranular(Solver& solver, State& s,const Parameters& p,const PhysicsOptions& o,double dt)
{
    if(!std::isfinite(dt) || dt<=0.0)throw std::invalid_argument("UN physical step requires positive finite dt");
    IntraGranularDiagnostics diagnostics;
    auto& d=diagnostics.values;
    const auto average=[](const std::vector<double>& modes) {
        const double projection=-2.0*std::sqrt(2.0/M_PI);double value=0.0;
        for(std::size_t i=0;i<modes.size();++i) {
            const double n=static_cast<double>(i+1),weight=std::pow(-1.0,n)/n;
            value+=projection*weight*modes[i]/((4.0/3.0)*M_PI);
        }
        return value;
    };
    for(std::size_t pop=0;pop<3;++pop)s.averages[pop]=average(s.modes[pop]);
    const double c_old=s.averages[0],mb_old=s.averages[1],md_old=s.averages[2];
    const double nb_old=s.bulk.number_density,nd_old=s.dislocation.number_density;
    const double rb_old=s.bulk.radius,rd_old=s.dislocation.radius,vd_old=s.dislocation.volume;
    const auto dg=xeDiffusivity(p),dv=vacancyDiffusivity(p);
    const auto b=resolutionRates(p,rb_old,rd_old);
    const auto g=trappingRates(p,o,dg.total,rb_old,nb_old,rd_old,nd_old);
    const double nu=nucleationRate(p,dg.total,c_old);
    const double phi_b=std::max(o.phi_bulk_scale,0.0)*phiPopulation(mb_old,nb_old),phi_d=phiPopulation(md_old,nd_old);
    s.bulk.number_density=std::max((nb_old+std::max(dt*nu,0.0))/(1.0+dt*b.first*phi_b),p.min_number_density);
    const double n0=std::max(o.nucleation_cluster_size,0.0);
    const double source_b=o.nucleation_mass_coupling ? n0*nu : 0.0;
    const ExchangeCoefficients coefficients={dg.total,p.grain_radius,productionRate(p)-source_b,g.bulk,g.dislocation,
        o.phi_gas_resolution ? b.first*phi_b : b.first,o.phi_gas_resolution ? b.second*phi_d : b.second,source_b,0.0};
    advanceExchange(solver,s,coefficients,dt);
    diagnostics.after_exchange=s.modes;
    d["c_new_exchange"]=s.averages[0];d["mb_new_exchange"]=s.averages[1];d["md_new_exchange"]=s.averages[2];
    if(s.averages[0]<0.0 || s.averages[1]<0.0 || s.averages[2]<0.0) {
        d["negative_reset"]=1.0;
        for(std::size_t pop=0;pop<3;++pop) {
            s.averages[pop]=std::max(s.averages[pop],0.0);
            std::fill(s.modes[pop].begin(),s.modes[pop].end(),0.0);
            solver.modeInitialization(static_cast<int>(s.numberOfModes()),s.averages[pop],s.modes[pop].data());
        }
    } else d["negative_reset"]=0.0;
    const double dmb=(s.averages[1]-mb_old)/dt,dmd=(s.averages[2]-md_old)/dt;
    const double old_vb=s.bulk.vacancies,old_vd=s.dislocation.vacancies;
    bool seed_b=false,seed_d=false;
    if(o.first_gas_pressure_seed && o.seed_bulk_on_first_gas && !s.bulk_first_gas_seed_done &&
       s.bulk.number_density>0.0 && mb_old<=0.0 && s.averages[1]>0.0) {
        const auto seed=seedVacancyInventory(p,s.averages[1],s.bulk.number_density,o.first_gas_pressure_factor);
        s.bulk.vacancies=std::max(s.bulk.vacancies,seed.first);
        s.bulk_first_gas_seed_done=true;seed_b=true;
    }
    if(o.first_gas_pressure_seed && o.seed_dislocation_on_first_gas && !s.dislocation_first_gas_seed_done &&
       nd_old>0.0 && md_old<=0.0 && s.averages[2]>0.0) {
        const auto seed=seedVacancyInventory(p,s.averages[2],nd_old,o.first_gas_pressure_factor);
        s.dislocation.vacancies=std::max(s.dislocation.vacancies,seed.first);
        s.dislocation_first_gas_seed_done=true;seed_d=true;
    }
    d["nvb_after_seed"]=s.bulk.vacancies;d["nvd_after_seed"]=s.dislocation.vacancies;
    double rate_b,rate_d;
    if(p.update_bulk_vacancies && !seed_b) {
        auto v=advanceVacancies(p,o,dv.total,rb_old,s.bulk.number_density,s.averages[1],s.bulk.vacancies,dt);
        s.bulk.vacancies=v.inventory;rate_b=v.rate;
    } else rate_b=(s.bulk.vacancies-old_vb)/dt;
    if(!seed_d) {
        auto v=advanceVacancies(p,o,dv.dislocation,rd_old,nd_old,s.averages[2],s.dislocation.vacancies,dt);
        s.dislocation.vacancies=v.inventory;rate_d=v.rate;
    } else rate_d=(s.dislocation.vacancies-old_vd)/dt;
    d["nvb_after_vacancy"]=s.bulk.vacancies;d["nvd_after_vacancy"]=s.dislocation.vacancies;
    d["V_b_growth"]=s.bulk.number_density>0.0 ? std::max(s.bulk.volume+dt*(p.omega_fg/s.bulk.number_density*dmb+matrixVolume(p)/s.bulk.number_density*rate_b),p.min_volume) : 0.0;
    const double growth_rate=nd_old>0.0 ? p.omega_fg/nd_old*dmd+matrixVolume(p)/nd_old*rate_d : 0.0;
    const double grown=nd_old>0.0 ? std::max(vd_old+dt*growth_rate,p.min_volume) : 0.0;
    const double lambda=coalescenceLambda(vd_old,nd_old),positive_growth=std::max(grown-vd_old,0.0);
    s.dislocation.number_density=nd_old;
    if(positive_growth>0.0 && nd_old>0.0)
        s.dislocation.number_density=nd_old/(1.0+p.coalescence_d_scale*4.0*lambda*nd_old*positive_growth);
    s.dislocation.number_density=std::max(s.dislocation.number_density,p.min_number_density);
    // L3 constant rho: no dynamic site nucleation. No legacy density law is substituted.
    const auto refresh=[&]() {
        s.bulk.volume=s.bulk.number_density>0.0 ? (p.omega_fg*std::max(s.averages[1],0.0)+matrixVolume(p)*s.bulk.vacancies)/s.bulk.number_density : 0.0;
        s.dislocation.volume=s.dislocation.number_density>0.0 ? (p.omega_fg*std::max(s.averages[2],0.0)+matrixVolume(p)*s.dislocation.vacancies)/s.dislocation.number_density : 0.0;
        s.bulk.volume=std::max(s.bulk.volume,p.min_volume);s.dislocation.volume=std::max(s.dislocation.volume,p.min_volume);
        s.bulk.radius=radiusFromVolume(s.bulk.volume);s.dislocation.radius=radiusFromVolume(s.dislocation.volume);
    };
    refresh();
    const double delta_r=std::max(s.dislocation.radius-rd_old,0.0);
    const double swept=4.0*M_PI*std::pow(rd_old+rb_old,2)*delta_r;
    const double raw=o.bulk_dislocation_capture ? p.capture_scale*s.dislocation.number_density*swept : 0.0;
    const double fraction=std::max(0.0,std::min(raw,1.0));
    if(fraction>0.0 && s.bulk.number_density>0.0) {
        const double gas=std::max(s.averages[1],0.0),vacancies=std::max(s.bulk.vacancies,0.0);
        s.averages[1]=(1.0-fraction)*gas;s.averages[2]=std::max(s.averages[2],0.0)+fraction*gas;
        s.bulk.vacancies=(1.0-fraction)*vacancies;s.dislocation.vacancies=std::max(s.dislocation.vacancies,0.0)+fraction*vacancies;
        s.bulk.number_density=(1.0-fraction)*s.bulk.number_density;
        for(std::size_t i=0;i<s.numberOfModes();++i) {
            const double captured=fraction*s.modes[1][i];s.modes[1][i]-=captured;s.modes[2][i]+=captured;
        }
        refresh();
    }
    d["Dg"]=dg.total;d["Dv"]=dv.total;d["Dv_dislocation"]=dv.dislocation;d["Dv1"]=dv.first;
    d["b_b"]=b.first;d["b_d"]=b.second;d["g_b"]=g.bulk;d["g_d"]=g.dislocation;
    d["nu_b"]=nu;d["phi_b"]=phi_b;d["phi_d"]=phi_d;d["lambda_d"]=lambda;
    d["dVd_growth_dt"]=growth_rate;d["dnvb_dt"]=rate_b;d["dnvd_dt"]=rate_d;
    d["f_cap"]=fraction;d["cap_raw_step"]=raw;d["beta"]=productionRate(p);
    d["source_c"]=coefficients.source_c;d["source_mb"]=source_b;
    return diagnostics;
}
}  // namespace un_l3
