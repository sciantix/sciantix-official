#pragma once
#include <fstream>
#include <iomanip>
#include "BaraniCoarsening.h"
namespace hart_trace {
inline void write(double time,double T,double Db,double rho,double beta) {
const char* path=std::getenv("SCIANTIX_HART_TRACE");if(!path)return;
static std::ofstream file(path);static bool first=true;
if(first)file<<"time_h,T_K,Dbulk_m2s,rho_m2,core_fraction,Dcore_m2s,Dmean_m2s,beta_line_s_inv\n";
first=false;
file<<std::setprecision(17)<<time<<','<<T<<','<<Db<<','<<rho<<','
    <<barani_coarsening::pi*std::pow(barani_coarsening::gas_core_radius,2)*rho<<','
    <<barani_coarsening::gasCoreDiffusivity(T)<<','
    <<barani_coarsening::meanGasDiffusivity(T,Db)<<','<<beta<<'\n';
}
}
