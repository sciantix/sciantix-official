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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Initialization.h"

void Initialization(
	double Sciantix_history[],
	double Sciantix_variables[],
	double Sciantix_diffusion_modes[],
	double Sciantix_thermochemistry[],
	std::vector<std::vector<std::string>> Sciantix_thermochemistry_options,
	std::vector<double> Temperature_input,
	std::vector<double> Fissionrate_input,
	std::vector<double> Hydrostaticstress_input,
	std::vector<double> Steampressure_input,
	std::vector<double> THERMOCHIMICApressure_input
)
{
	// Sciantix_history initialization
	Sciantix_history[0] = Temperature_input[0];
	Sciantix_history[1] = Temperature_input[0];
	Sciantix_history[2] = Fissionrate_input[0];
	Sciantix_history[3] = Fissionrate_input[0];
	Sciantix_history[4] = Hydrostaticstress_input[0];
	Sciantix_history[5] = Hydrostaticstress_input[0];
	Sciantix_history[6] = 0.0;
	
	Sciantix_history[9] = Steampressure_input[0];
	Sciantix_history[10] = Steampressure_input[0];

	Sciantix_history[11] = THERMOCHIMICApressure_input[0];
	Sciantix_history[12] = THERMOCHIMICApressure_input[0];

	// Sciantix_variables initialization
	Sciantix_variables[25] = 2.0e+13;  // Intergranular_bubble_concentration[0]
	Sciantix_variables[35] = 0.5;      // Intergranular_saturation_fractional_coverage[0]
	Sciantix_variables[37] = 1.0;      // Intergranular_fractional_intactness[0]

	// https://pubchem.ncbi.nlm.nih.gov/compound/Uranium-235
	Sciantix_variables[41] *= Sciantix_variables[40] * 6.022e+24 * 0.8815 / 234.04095; // U-234
	Sciantix_variables[42] *= Sciantix_variables[40] * 6.022e+24 * 0.8815 / 235.04393; // U-235
	Sciantix_variables[43] *= Sciantix_variables[40] * 6.022e+24 * 0.8815 / 236.04557; // U-236
	Sciantix_variables[44] *= Sciantix_variables[40] * 6.022e+24 * 0.8815 / 237.04873; // U-237
	Sciantix_variables[45] *= Sciantix_variables[40] * 6.022e+24 * 0.8815 / 238.05079; // U-238
	double total_U = Sciantix_variables[41] + Sciantix_variables[42] + Sciantix_variables[43] + Sciantix_variables[44] + Sciantix_variables[45];

    // https://pubchem.ncbi.nlm.nih.gov/compound/Plutonium-239
    Sciantix_variables[171] *= Sciantix_variables[40] * Sciantix_variables[177] * 6.022e24 * 0.882 / 238; // Pu-238
	Sciantix_variables[172] *= Sciantix_variables[40] * Sciantix_variables[177] * 6.022e24 * 0.882 / 239; // Pu-239
	Sciantix_variables[173] *= Sciantix_variables[40] * Sciantix_variables[177] * 6.022e24 * 0.882 / 240; // Pu-240
	Sciantix_variables[174] *= Sciantix_variables[40] * Sciantix_variables[177] * 6.022e24 * 0.882 / 241; // Pu-241
	Sciantix_variables[175] *= Sciantix_variables[40] * Sciantix_variables[177] * 6.022e24 * 0.882 / 242; // Pu-242
	double total_Pu = Sciantix_variables[171] + Sciantix_variables[172] + Sciantix_variables[173] + Sciantix_variables[174] + Sciantix_variables[175];

	// Intragranular similarity ratio
	Sciantix_variables[64] = 1.0;

	Sciantix_variables[101] = Sciantix_variables[102] = Sciantix_variables[103] = Sciantix_variables[104] = Sciantix_variables[105] = Sciantix_variables[106] = Sciantix_variables[107] = Sciantix_variables[108] = Sciantix_variables[109] = Sciantix_variables[110] =  0.0;
	Sciantix_variables[111] = Sciantix_variables[112] = Sciantix_variables[113] = Sciantix_variables[114] = Sciantix_variables[115] = Sciantix_variables[116] = Sciantix_variables[117] = Sciantix_variables[118] = Sciantix_variables[119] = Sciantix_variables[120] =  0.0;
	Sciantix_variables[121] = Sciantix_variables[122] = Sciantix_variables[123] = Sciantix_variables[124] = Sciantix_variables[125] = Sciantix_variables[126] = Sciantix_variables[127] = Sciantix_variables[128] = Sciantix_variables[129] = Sciantix_variables[130] =  0.0;
	// Fabrication porosity = Porosity
	Sciantix_variables[71] = Sciantix_variables[70] = 1.0 - Sciantix_variables[40] / 10960.0;
	
	// Residual porosity
	Sciantix_variables[73] = 0.75 * Sciantix_variables[71];

	// U, O, Pu content (mol/m3)
	double avogadro_number = 6.02214076e23;
	Sciantix_variables[161] = total_U / avogadro_number; // U content in mol/m3
	Sciantix_variables[163] = total_Pu / avogadro_number; // Pu content in mol/m3
	Sciantix_variables[162] = (2.0 + Sciantix_variables[66]) * (Sciantix_variables[161] + Sciantix_variables[163]); // O content in mol/m3
	
	// Projection on diffusion modes of the initial conditions
	double initial_condition(0.0);
	double projection_remainder(0.0);
	double reconstructed_solution(0.0);
	int iteration(0), iteration_max(20), n(0), np1(1), n_modes(40), k(0), K(27);
	double projection_coeff(0.0);
	projection_coeff = -sqrt(8.0 / M_PI);

	for (k = 0; k < K; ++k)
	{
		switch (k)
		{
		case 0: initial_condition = Sciantix_variables[2];  break;  // Xe in grain
		case 1: initial_condition = Sciantix_variables[3];  break;  // Xe in grain - solution
		case 2: initial_condition = Sciantix_variables[4];  break;  // Xe in grain - bubbles

		case 3: initial_condition = Sciantix_variables[8];  break;  // Kr in grain
		case 4: initial_condition = Sciantix_variables[9];  break;  // Kr in grain - solution
		case 5: initial_condition = Sciantix_variables[10]; break;  // Kr in grain - bubbles

		case 6: initial_condition = Sciantix_variables[14]; break;  // He in grain
		case 7: initial_condition = Sciantix_variables[15]; break;  // He in grain - solution
		case 8: initial_condition = Sciantix_variables[16]; break;  // He in grain - bubbles

		case 9: initial_condition = Sciantix_variables[49]; break;  // Xe133 in grain
		case 10: initial_condition = Sciantix_variables[50]; break;  // Xe133 in grain - solution
		case 11: initial_condition = Sciantix_variables[51]; break;  // Xe133 in grain - bubbles

		case 12: initial_condition = Sciantix_variables[58]; break;  // Kr85m in grain
		case 13: initial_condition = Sciantix_variables[59]; break;  // Kr85m in grain - solution
		case 14: initial_condition = Sciantix_variables[60]; break;  // Kr85m in grain - bubbles
		
		case 15: initial_condition = Sciantix_variables[92]; break;  // Xe in UO2 HBS
		case 16: initial_condition = Sciantix_variables[93]; break;  // Xe in UO2 HBS - solution
		case 17: initial_condition = Sciantix_variables[94]; break;  // Xe in UO2 HBS - bubbles

		case 18: initial_condition = Sciantix_variables[102]; break;  // Cs in grain
		case 19: initial_condition = Sciantix_variables[103]; break;  // Cs in grain - solution
		case 20: initial_condition = Sciantix_variables[104]; break;  // Cs in grain - bubbles

		case 21: initial_condition = Sciantix_variables[112]; break;  // I in grain
		case 22: initial_condition = Sciantix_variables[113]; break;  // I in grain - solution
		case 23: initial_condition = Sciantix_variables[114]; break;  // I in grain - bubbles
		
		case 24: initial_condition = Sciantix_variables[122]; break;  // Te in grain
		case 25: initial_condition = Sciantix_variables[123]; break;  // Te in grain - solution
		case 26: initial_condition = Sciantix_variables[124]; break;  // Te in grain - bubbles

		default: initial_condition = 0.0; break;
		}

		projection_remainder = initial_condition;
		for (iteration = 0; iteration < iteration_max; ++iteration)
		{
			reconstructed_solution = 0.0;
			for (n = 0; n < n_modes; ++n)
			{
				if (iteration == 0) Sciantix_diffusion_modes[k * n_modes + n] = 0;
				np1 = n + 1;
				const double n_coeff = pow(-1.0, np1) / np1;
				Sciantix_diffusion_modes[k * n_modes + n] += projection_coeff * n_coeff * projection_remainder;
				reconstructed_solution += projection_coeff * n_coeff * Sciantix_diffusion_modes[k * n_modes + n] * 3.0 / (4.0 * M_PI);
			}
			projection_remainder = initial_condition - reconstructed_solution;
		}
	}

	// Warnings
	if(Sciantix_variables[38] > 0.0 && Sciantix_variables[65] == 0.0)
		std::cout << "WARNING - FIMA calculation: Initial fuel burnup > 0 but null initial irradiation time. Check initial irradiation time." << std::endl;
}
