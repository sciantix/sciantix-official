#include "UNParameters.h"
#include <fstream>
#include <stdexcept>
inline void loadParameters(const char* path, un_l3::Parameters& p) {
std::ifstream f(path); std::string name,value; while(f>>name>>value) {
if(name=="temperature") p.temperature=std::stod(value);
else if(name=="fission_rate") p.fission_rate=std::stod(value);
else if(name=="grain_radius") p.grain_radius=std::stod(value);
else if(name=="xe_yield") p.xe_yield=std::stod(value);
else if(name=="precursor_factor") p.precursor_factor=std::stod(value);
else if(name=="kB_eV") p.kB_eV=std::stod(value);
else if(name=="kB_J") p.kB_J=std::stod(value);
else if(name=="D10") p.D10=std::stod(value);
else if(name=="Q1") p.Q1=std::stod(value);
else if(name=="A20_xe") p.A20_xe=std::stod(value);
else if(name=="B21_xe") p.B21_xe=std::stod(value);
else if(name=="B22_xe") p.B22_xe=std::stod(value);
else if(name=="B23_xe") p.B23_xe=std::stod(value);
else if(name=="A30") p.A30=std::stod(value);
else if(name=="D2_xe_scale") p.D2_xe_scale=std::stod(value);
else if(name=="Dg_scale") p.Dg_scale=std::stod(value);
else if(name=="Dg_D1_scale") p.Dg_D1_scale=std::stod(value);
else if(name=="Dg_D3_scale") p.Dg_D3_scale=std::stod(value);
else if(name=="Dg_dislocation_scale") p.Dg_dislocation_scale=std::stod(value);
else if(name=="Dg_extra_scale") p.Dg_extra_scale=std::stod(value);
else if(name=="D10_vU") p.D10_vU=std::stod(value);
else if(name=="Q1_vU") p.Q1_vU=std::stod(value);
else if(name=="A20_vU") p.A20_vU=std::stod(value);
else if(name=="B21_vU") p.B21_vU=std::stod(value);
else if(name=="B22_vU") p.B22_vU=std::stod(value);
else if(name=="Dv_scale") p.Dv_scale=std::stod(value);
else if(name=="Dv_D1_scale") p.Dv_D1_scale=std::stod(value);
else if(name=="Dv_D2_scale") p.Dv_D2_scale=std::stod(value);
else if(name=="Dv_dislocation_scale") p.Dv_dislocation_scale=std::stod(value);
else if(name=="radius_in_lattice") p.radius_in_lattice=std::stod(value);
else if(name=="omega_fg") p.omega_fg=std::stod(value);
else if(name=="lattice_parameter") p.lattice_parameter=std::stod(value);
else if(name=="gamma_b") p.gamma_b=std::stod(value);
else if(name=="hydrostatic_stress") p.hydrostatic_stress=std::stod(value);
else if(name=="min_radius_for_pressure") p.min_radius_for_pressure=std::stod(value);
else if(name=="f_n") p.f_n=std::stod(value);
else if(name=="rho_d") p.rho_d=std::stod(value);
else if(name=="K_d") p.K_d=std::stod(value);
else if(name=="r_d") p.r_d=std::stod(value);
else if(name=="Z_d") p.Z_d=std::stod(value);
else if(name=="gb_scale") p.gb_scale=std::stod(value);
else if(name=="gd_scale") p.gd_scale=std::stod(value);
else if(name=="b_scale") p.b_scale=std::stod(value);
else if(name=="b_bulk_scale") p.b_bulk_scale=std::stod(value);
else if(name=="b_dislocation_scale") p.b_dislocation_scale=std::stod(value);
else if(name=="gd_bubble_scale") p.gd_bubble_scale=std::stod(value);
else if(name=="gd_line_scale") p.gd_line_scale=std::stod(value);
else if(name=="gd_line_alpha") p.gd_line_alpha=std::stod(value);
else if(name=="coalescence_d_scale") p.coalescence_d_scale=std::stod(value);
else if(name=="capture_scale") p.capture_scale=std::stod(value);
else if(name=="min_number_density") p.min_number_density=std::stod(value);
else if(name=="min_volume") p.min_volume=std::stod(value);
else if(name=="vacancy_absorption_only") p.vacancy_absorption_only=std::stod(value)!=0.0;
else if(name=="update_bulk_vacancies") p.update_bulk_vacancies=std::stod(value)!=0.0;
else if(name=="xe_diffusivity_mode") p.xe_diffusivity_mode=value;
else if(name=="vacancy_diffusivity_mode") p.vacancy_diffusivity_mode=value;
else throw std::runtime_error("Unknown fixture parameter: "+name);
} }
