#include "UNTransport.h"
#include "UNBubblePhysics.h"
#include "UNGrainFace.h"
#include "UNLookupTables.h"
#include "UNIntraGranular.h"
#include "Solver.h"
#include "parameter_loader.h"
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <vector>

int main(int argc, char** argv)
{
    try {
        if (argc != 5) throw std::runtime_error("component_driver component parameters input output");
        un_l3::Parameters p;
        loadParameters(argv[2],p);
        un_l3::PhysicsOptions o;
        std::ifstream input(argv[3]);
        std::ofstream output(argv[4],std::ios::binary);
        const std::string component=argv[1];
        std::size_t rows; input>>rows;
        un_l3::State state(160);Solver solver;
        if (!input || !output) throw std::runtime_error("Cannot open component fixture");
        for (std::size_t i=0;i<rows;++i) {
            std::vector<double> result;
            if (component=="transport") {
                double rb,nb,rd,nd,c; input>>rb>>nb>>rd>>nd>>c;
                auto dg=un_l3::xeDiffusivity(p), dv=un_l3::vacancyDiffusivity(p);
                auto b=un_l3::resolutionRates(p,rb,rd);
                auto g=un_l3::trappingRates(p,o,dg.total,rb,nb,rd,nd);
                result={dg.total,dg.first,dg.second,dg.third,dv.total,dv.first,dv.second,dv.dislocation,
                        b.first,b.second,g.bulk,g.dislocation,g.gamma,g.denominator,g.free_line,g.bubble_term,g.line_term,
                        un_l3::productionRate(p),un_l3::nucleationRate(p,dg.total,c),un_l3::grainBoundaryDiffusivity(p,o,dv.first)};
            }
            else if(component=="lookup") {
                double kind,x;input>>kind>>x;
                const auto& table=kind==0.0 ? un_l3::xeLookupTable() : un_l3::vacancyLookupTable();
                result={std::log10(un_l3::lookupDiffusivity(1e4/x,table))};
            }
            else if(component=="vacancy") {
                double dv,r,n,gas,old,dt;input>>dv>>r>>n>>gas>>old>>dt;
                auto step=un_l3::advanceVacancies(p,o,dv,r,n,gas,old,dt);
                result={step.inventory,step.rate};
            }
            else if(component=="seed") {
                double gas,factor;input>>gas>>factor;
                auto seed=un_l3::pressureSeed(p,gas,factor);
                result={seed.first,seed.second,un_l3::internalPressure(p,gas,seed.first)/un_l3::equilibriumPressure(p,seed.second)};
            }
            else if(component=="grainface_limited") {
                double nv,growth,eq,dt;input>>nv>>growth>>eq>>dt;
                result={un_l3::grainFaceLimitedGrowth(nv,growth,eq,dt)};
            }
            else if(component=="grainface_3x3") {
                double v,a,n,fc,g0,g1,delta,omega,kappa,sat;input>>v>>a>>n>>fc>>g0>>g1>>delta>>omega>>kappa>>sat;
                auto value=un_l3::solveGrainFace3x3(v,a,n,fc,g0,g1,delta,omega,kappa,sat!=0.0);
                result.assign(value.begin(),value.end());
            }
            else if(component=="grainface" || component=="grainface_chain" || component=="grainface_initial") {
                double n,q,rel,nv,sat,incoming,dt,first;input>>n>>q>>rel>>nv>>sat>>incoming>>dt>>first;
                if(component=="grainface_initial") {
                    state.grain_face=un_l3::initializeGrainFace(p,o);
                    un_l3::refreshGrainFace(p,o,state.grain_face);
                } else if(component=="grainface" || i==0) {
                    state.grain_face.number_areal=n;state.grain_face.gas=q;state.grain_face.released=rel;
                    state.grain_face.vacancies=nv;state.grain_face.saturation_reached=sat!=0.0;
                }
                if(component!="grainface_initial") un_l3::advanceGrainFace(p,o,state.grain_face,incoming,dt,first);
                const auto& s=state.grain_face;
                result={s.gas,s.released,s.vacancies,s.number_areal,s.number_volumetric,s.atoms_per_bubble,s.vacancies_per_bubble,
                    s.volume,s.radius,s.area,s.coverage,s.pressure,s.equilibrium_pressure,s.zeta,
                    s.vacancy_rate,s.area_rate,s.number_rate,s.gas_in_step,s.gas_released_step,static_cast<double>(s.saturation_reached)};
            }
            else if(component=="intragranular" || component=="intragranular_chain") {
                double nb,nd,rb,rd,vb,vd,nvb,nvd,seedb,seedd,dt;
                input>>nb>>nd>>rb>>rd>>vb>>vd>>nvb>>nvd>>seedb>>seedd>>dt;
                un_l3::State recorded(160);
                for(auto& population:recorded.modes)for(double& value:population)input>>value;
                if(component=="intragranular" || i==0) {
                    state.modes=recorded.modes;
                    state.bulk.number_density=nb;state.dislocation.number_density=nd;
                    state.bulk.radius=rb;state.dislocation.radius=rd;state.bulk.volume=vb;state.dislocation.volume=vd;
                    state.bulk.vacancies=nvb;state.dislocation.vacancies=nvd;
                    state.bulk_first_gas_seed_done=seedb!=0.0;state.dislocation_first_gas_seed_done=seedd!=0.0;
                }
                auto diagnostics=un_l3::advanceIntraGranular(solver,state,p,o,dt);
                result={state.averages[0],state.averages[1],state.averages[2],state.bulk.number_density,state.dislocation.number_density,
                    state.bulk.radius,state.dislocation.radius,state.bulk.volume,state.dislocation.volume,state.bulk.vacancies,state.dislocation.vacancies,
                    static_cast<double>(state.bulk_first_gas_seed_done),static_cast<double>(state.dislocation_first_gas_seed_done)};
                for(const std::string name:{"Dg","Dv","Dv_dislocation","b_b","b_d","g_b","g_d","nu_b","phi_b","phi_d","lambda_d","dVd_growth_dt","dnvb_dt","dnvd_dt","f_cap","cap_raw_step","source_c","source_mb","nvb_after_seed","nvd_after_seed","nvb_after_vacancy","nvd_after_vacancy","V_b_growth"})
                    result.push_back(diagnostics.values.at(name));
                for(const auto& population:diagnostics.after_exchange)for(double value:population)result.push_back(value);
                for(const auto& population:state.modes)for(double value:population)result.push_back(value);
            }
            else throw std::runtime_error("Unknown component");
            if (!input) throw std::runtime_error("Incomplete component fixture row");
            output.write(reinterpret_cast<const char*>(result.data()),result.size()*sizeof(double));
        }
        if (!output) throw std::runtime_error("Cannot write component output");
        return 0;
    } catch(const std::exception& e) { std::cerr<<e.what()<<"\n"; return 1; }
}
