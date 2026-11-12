#pragma once
#include <algorithm>
#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <string>


// # COARSENING: experimental birth laws; none is enabled in the frozen base.
namespace nucleation_tests {
constexpr double pi=3.14159265358979323846;
constexpr double initial_density=4e13, K=1e6, embryo_atoms=2;
constexpr double core_radius=5*3.85e-10;
constexpr double oxide_to_heavy_metal=(238.02891+2*15.9994)/238.02891;
inline const std::string& mode() {
    static const std::string value=[](){const char* p=std::getenv("SCIANTIX_NUCLEATION");return std::string(p?p:"vesh_matrix_network");}();
    return value;
}
inline bool capture() { return mode().find("capture")!=std::string::npos; }
inline bool stableCapture() { return mode().find("stable")!=std::string::npos; }
inline bool connected() { return mode().find("network")!=std::string::npos || mode()=="geometry_only"; }
inline bool linear() { return mode().find("linear")!=std::string::npos; }

// User power-law/sigmoid fit of digitized Veshchunov-Shestak Fig.4.
constexpr double fit_A=6544878893406.6982,fit_n=1.1513567102867877,fit_inf=0.60761803375190093,fit_Tc=1109.0189319357949,fit_dT=25.805562621603691;
inline double densityCurve(double B,double T) {
    const double x=(T-fit_Tc)/fit_dT;
    const double sigmoid=x>=0?std::exp(-x)/(1+std::exp(-x)):1/(1+std::exp(x));
    return B>0?fit_A*std::pow(B,fit_n)*(fit_inf+(1-fit_inf)*sigmoid):0.;
}
inline double densityIncrement(double old_b,double new_b,double T,double F,double dt) {
    if(mode()=="control" || F<=0 || dt<=0 || new_b<=old_b)return 0.;
    return std::max(0.,densityCurve(new_b*oxide_to_heavy_metal,T)-densityCurve(old_b*oxide_to_heavy_metal,T));
}
struct State { double rho_sites=initial_density, cumulative_births=0; };
inline State& state(){static State s;return s;}
struct Packet {
    double increment=0, requested=0, born=0, gas=0;
    double old_number=0, birth_number=0, old_vacancy_total=0, birth_vacancy_total=0;
    double old_bulk_number=0, mode_balance_error=0;
};
// Diagnostic occupation distribution: Poisson with mean gas/bulk bubble.
// This is an explicit closure, not a measured cluster distribution in UO2.
inline double stableProbability(double mean) {
    if(mean<=0)return 0;
    if(mean<1e-3)return mean*mean*(.5-mean/3+mean*mean/8-mean*mean*mean/30);
    return -std::expm1(-mean)-mean*std::exp(-mean);
}
inline double stableMean(double mean) {
    const double p=stableProbability(mean);
    return p>0?mean*(-std::expm1(-mean))/p:embryo_atoms;
}
inline double requestedBirths(double increment,double bulk_number,double bulk_radius,double mean_gas) {
    if(mode()=="geometry_only")return 0;
    double per_length=K;
    if(capture()) per_length=std::min(K,pi*std::pow(core_radius+bulk_radius,2)*bulk_number*
                                   (stableCapture()?stableProbability(mean_gas):1.));
    return std::max(0.,increment*per_length);
}
inline double mixedVacancies(double old_number,double old_vacancies,double added_number) {
    return added_number>0 ? old_number*old_vacancies/(old_number+added_number) : old_vacancies;
}
inline void trace(double time,double T,double burnup,double pipe_density,const Packet& p,
                  double final_number,double final_radius) {
    const char* filename=std::getenv("SCIANTIX_NUCLEATION_TRACE");if(!filename)return;
    static bool first=true;
    static std::ofstream file(filename);
    if(first)file<<"time_h,T_K,B_GWd_tHM,rho_sites_m2,rho_pipe_m2,delta_rho_m2,requested_births_m3,births_m3,cumulative_births_m3,gas_transfer_m3,Nd_before_m3,Nd_after_birth_m3,vacancies_before_m3,vacancies_after_birth_m3,Nb_before_m3,mode_balance_error,Nd_final_m3,R_final_m\n";
    first=false;
    file<<std::setprecision(17)<<time<<','<<T<<','<<burnup*oxide_to_heavy_metal<<','<<state().rho_sites<<','
        <<pipe_density<<','<<p.increment<<','<<p.requested<<','<<p.born<<','<<state().cumulative_births<<','
        <<p.gas<<','<<p.old_number<<','<<p.birth_number<<','<<p.old_vacancy_total<<','<<p.birth_vacancy_total<<','
        <<p.old_bulk_number<<','<<p.mode_balance_error<<','<<final_number<<','<<final_radius<<'\n';
}
}
