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
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "UpdateVariables.h"

/// UpdateVariables

void UpdateVariables(double Sciantix_variables[], double Sciantix_diffusion_modes[])
{
	Sciantix_variables[0] = sciantix_variable[sv["Grain radius"]].getFinalValue();
	Sciantix_variables[1] = sciantix_variable[sv["Xe produced"]].getFinalValue();
	Sciantix_variables[2] = sciantix_variable[sv["Xe in grain"]].getFinalValue();
	Sciantix_variables[3] = sciantix_variable[sv["Xe in intragranular solution"]].getFinalValue();
	Sciantix_variables[4] = sciantix_variable[sv["Xe in intragranular bubbles"]].getFinalValue();
	Sciantix_variables[5] = sciantix_variable[sv["Xe at grain boundary"]].getFinalValue();
	Sciantix_variables[6] = sciantix_variable[sv["Xe released"]].getFinalValue();
	Sciantix_variables[7] = sciantix_variable[sv["Kr produced"]].getFinalValue();
	Sciantix_variables[8] = sciantix_variable[sv["Kr in grain"]].getFinalValue();
	Sciantix_variables[9] = sciantix_variable[sv["Kr in intragranular solution"]].getFinalValue();
	Sciantix_variables[10] = sciantix_variable[sv["Kr in intragranular bubbles"]].getFinalValue();
	Sciantix_variables[11] = sciantix_variable[sv["Kr at grain boundary"]].getFinalValue();
	Sciantix_variables[12] = sciantix_variable[sv["Kr released"]].getFinalValue();
	Sciantix_variables[13] = sciantix_variable[sv["He produced"]].getFinalValue();
	Sciantix_variables[14] = sciantix_variable[sv["He in grain"]].getFinalValue();
	Sciantix_variables[15] = sciantix_variable[sv["He in intragranular solution"]].getFinalValue();
	Sciantix_variables[16] = sciantix_variable[sv["He in intragranular bubbles"]].getFinalValue();
	Sciantix_variables[17] = sciantix_variable[sv["He at grain boundary"]].getFinalValue();
	Sciantix_variables[18] = sciantix_variable[sv["He released"]].getFinalValue();
	Sciantix_variables[19] = sciantix_variable[sv["Intragranular bubble concentration"]].getFinalValue();
	Sciantix_variables[20] = sciantix_variable[sv["Intragranular bubble radius"]].getFinalValue();
	Sciantix_variables[21] = sciantix_variable[sv["Intragranular Xe atoms per bubble"]].getFinalValue();
	Sciantix_variables[22] = sciantix_variable[sv["Intragranular Kr atoms per bubble"]].getFinalValue();
	Sciantix_variables[23] = sciantix_variable[sv["Intragranular He atoms per bubble"]].getFinalValue();
	Sciantix_variables[24] = sciantix_variable[sv["Intragranular gas swelling"]].getFinalValue();
	Sciantix_variables[25] = sciantix_variable[sv["Intergranular bubble concentration"]].getFinalValue();
	Sciantix_variables[26] = sciantix_variable[sv["Intergranular Xe atoms per bubble"]].getFinalValue();
	Sciantix_variables[27] = sciantix_variable[sv["Intergranular Kr atoms per bubble"]].getFinalValue();
	Sciantix_variables[28] = sciantix_variable[sv["Intergranular He atoms per bubble"]].getFinalValue();
	Sciantix_variables[29] = sciantix_variable[sv["Intergranular atoms per bubble"]].getFinalValue();
	Sciantix_variables[30] = sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue();
	Sciantix_variables[31] = sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue();
	Sciantix_variables[32] = sciantix_variable[sv["Intergranular bubble area"]].getFinalValue();
	Sciantix_variables[33] = sciantix_variable[sv["Intergranular bubble volume"]].getFinalValue();
	Sciantix_variables[34] = sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue();
	Sciantix_variables[35] = sciantix_variable[sv["Intergranular saturation fractional coverage"]].getFinalValue();
	Sciantix_variables[36] = sciantix_variable[sv["Intergranular gas swelling"]].getFinalValue();
	Sciantix_variables[37] = sciantix_variable[sv["Intergranular fractional intactness"]].getFinalValue();
	Sciantix_variables[38] = sciantix_variable[sv["Burnup"]].getFinalValue();
	Sciantix_variables[39] = sciantix_variable[sv["Effective burnup"]].getFinalValue();
	Sciantix_variables[40] = sciantix_variable[sv["Fuel density"]].getFinalValue();
	Sciantix_variables[41] = sciantix_variable[sv["U234"]].getFinalValue();
	Sciantix_variables[42] = sciantix_variable[sv["U235"]].getFinalValue();
	Sciantix_variables[43] = sciantix_variable[sv["U236"]].getFinalValue();
	Sciantix_variables[44] = sciantix_variable[sv["U237"]].getFinalValue();
	Sciantix_variables[45] = sciantix_variable[sv["U238"]].getFinalValue();
	Sciantix_variables[46] = sciantix_variable[sv["Intergranular vented fraction"]].getFinalValue();
	Sciantix_variables[47] = sciantix_variable[sv["Intergranular venting probability"]].getFinalValue();
	Sciantix_variables[48] = sciantix_variable[sv["Xe133 produced"]].getFinalValue();
	Sciantix_variables[49] = sciantix_variable[sv["Xe133 in grain"]].getFinalValue();
	Sciantix_variables[50] = sciantix_variable[sv["Xe133 in intragranular solution"]].getFinalValue();
	Sciantix_variables[51] = sciantix_variable[sv["Xe133 in intragranular bubbles"]].getFinalValue();
	Sciantix_variables[52] = sciantix_variable[sv["Xe133 decayed"]].getFinalValue();
	Sciantix_variables[53] = sciantix_variable[sv["Xe133 at grain boundary"]].getFinalValue();
	Sciantix_variables[54] = sciantix_variable[sv["Xe133 released"]].getFinalValue();
	Sciantix_variables[55] = sciantix_variable[sv["Restructured volume fraction"]].getFinalValue();
	Sciantix_variables[56] = sciantix_variable[sv["HBS porosity"]].getFinalValue();
	Sciantix_variables[57] = sciantix_variable[sv["Kr85m produced"]].getFinalValue();
	Sciantix_variables[58] = sciantix_variable[sv["Kr85m in grain"]].getFinalValue();
	Sciantix_variables[59] = sciantix_variable[sv["Kr85m in intragranular solution"]].getFinalValue();
	Sciantix_variables[60] = sciantix_variable[sv["Kr85m in intragranular bubbles"]].getFinalValue();
	Sciantix_variables[61] = sciantix_variable[sv["Kr85m decayed"]].getFinalValue();
	Sciantix_variables[62] = sciantix_variable[sv["Kr85m at grain boundary"]].getFinalValue();
	Sciantix_variables[63] = sciantix_variable[sv["Kr85m released"]].getFinalValue();
	Sciantix_variables[64] = sciantix_variable[sv["Intragranular similarity ratio"]].getFinalValue();
	Sciantix_variables[65] = sciantix_variable[sv["Irradiation time"]].getFinalValue();
	Sciantix_variables[66] = sciantix_variable[sv["Stoichiometry deviation"]].getFinalValue();
	Sciantix_variables[67] = sciantix_variable[sv["Fuel oxygen partial pressure"]].getFinalValue();
	Sciantix_variables[80] = sciantix_variable[sv["HBS pore density"]].getFinalValue();
	Sciantix_variables[81] = sciantix_variable[sv["HBS pore volume"]].getFinalValue();
	Sciantix_variables[82] = sciantix_variable[sv["HBS pore radius"]].getFinalValue();
	Sciantix_variables[83] = sciantix_variable[sv["Xe in HBS pores"]].getFinalValue();
	Sciantix_variables[85] = sciantix_variable[sv["Xe in HBS pores - variance"]].getFinalValue();
	Sciantix_variables[86] = sciantix_variable[sv["Xe atoms per HBS pore"]].getFinalValue();
	Sciantix_variables[88] = sciantix_variable[sv["Xe atoms per HBS pore - variance"]].getFinalValue();

	for (int i = 0; i < n_modes; ++i)
	{
		Sciantix_diffusion_modes[i] = modes_initial_conditions[i];
		Sciantix_diffusion_modes[1 * n_modes + i] = modes_initial_conditions[1 * n_modes + i];
		Sciantix_diffusion_modes[2 * n_modes + i] = modes_initial_conditions[2 * n_modes + i];
		Sciantix_diffusion_modes[3 * n_modes + i] = modes_initial_conditions[3 * n_modes + i];
		Sciantix_diffusion_modes[4 * n_modes + i] = modes_initial_conditions[4 * n_modes + i];
		Sciantix_diffusion_modes[5 * n_modes + i] = modes_initial_conditions[5 * n_modes + i];
		Sciantix_diffusion_modes[6 * n_modes + i] = modes_initial_conditions[6 * n_modes + i];
		Sciantix_diffusion_modes[7 * n_modes + i] = modes_initial_conditions[7 * n_modes + i];
		Sciantix_diffusion_modes[8 * n_modes + i] = modes_initial_conditions[8 * n_modes + i];
		Sciantix_diffusion_modes[9 * n_modes + i] = modes_initial_conditions[9 * n_modes + i];
		Sciantix_diffusion_modes[10 * n_modes + i] = modes_initial_conditions[10 * n_modes + i];
		Sciantix_diffusion_modes[11 * n_modes + i] = modes_initial_conditions[11 * n_modes + i];
		Sciantix_diffusion_modes[12 * n_modes + i] = modes_initial_conditions[12 * n_modes + i];
		Sciantix_diffusion_modes[13 * n_modes + i] = modes_initial_conditions[13 * n_modes + i];
		Sciantix_diffusion_modes[14 * n_modes + i] = modes_initial_conditions[14 * n_modes + i];
	}
}
