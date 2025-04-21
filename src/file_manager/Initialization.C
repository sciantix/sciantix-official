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
#include "Source.h"
#include "MainVariables.h"
#include "Solver.h"
#include "SourceHandler.h"

void Initialization(
	double Sciantix_history[],
	double Sciantix_variables[],
	double Sciantix_diffusion_modes[],
	double Sciantix_diffusion_modes_NUS[],
	std::vector<double> Temperature_input,
	std::vector<double> Fissionrate_input,
	std::vector<double> Hydrostaticstress_input,
	std::vector<double> Steampressure_input
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

	// Intragranular similarity ratio
	Sciantix_variables[64] = 1.0;

	// Projection on diffusion modes of the initial conditions
	double initial_condition(0.0);
	
	//initial_condition_NUS is a Source which is why initialization is like this
	Source null_source;
	null_source.time = 0;
	null_source.NormalizedDomain.resize(2, 0.0);
	null_source.Slopes.resize(1, 0.0);
	null_source.Intercepts.resize(1, 0.0);
	
	Source initial_condition_NUS = null_source;

	//Uniform
	double projection_remainder(0.0);
	double reconstructed_solution(0.0);
	
	//NUS
	double projection_remainder_NUS(0.0);
	double reconstructed_solution_NUS(0.0);

	int iteration(0), iteration_max(20), n(0), np1(1), n_modes(40), k(0), K(1);
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

	//NUS SOLVER
	for (k = 0; k < K; ++k)
	{
		switch (k)
		{
		case 0: initial_condition_NUS = initial_distribution.at(0) ;  break;  // Xe in grain
		case 1: initial_condition_NUS = initial_distribution.at(1);  break;  // Xe in grain - solution
		case 2: initial_condition_NUS = initial_distribution.at(2);  break;  // Xe in grain - bubbles 

		case 3: initial_condition_NUS = initial_distribution.at(3);  break;  // Kr in grain
		case 4: initial_condition_NUS = initial_distribution.at(4);  break;  // Kr in grain - solution
		case 5: initial_condition_NUS = initial_distribution.at(5); break;  // Kr in grain - bubbles

		case 6: initial_condition_NUS = initial_distribution.at(6); break;  // He in grain
		case 7: initial_condition_NUS = initial_distribution.at(7); break;  // He in grain - solution
		case 8: initial_condition_NUS = initial_distribution.at(8); break;  // He in grain - bubbles

		case 9: initial_condition_NUS = initial_distribution.at(9); break;  // Xe133 in grain
		case 10: initial_condition_NUS = initial_distribution.at(10); break;  // Xe133 in grain - solution
		case 11: initial_condition_NUS = initial_distribution.at(11); break;  // Xe133 in grain - bubbles

		case 12: initial_condition_NUS = initial_distribution.at(12); break;  // Kr85m in grain
		case 13: initial_condition_NUS = initial_distribution.at(13); break;  // Kr85m in grain - solution
		case 14: initial_condition_NUS = initial_distribution.at(14); break;  // Kr85m in grain - bubbles
		
		case 15: initial_condition_NUS = initial_distribution.at(15); break;  // Xe in UO2 HBS
		case 16: initial_condition_NUS = initial_distribution.at(16); break;  // Xe in UO2 HBS - solution
		case 17: initial_condition_NUS = initial_distribution.at(17); break;  // Xe in UO2 HBS - bubbles

		default: initial_condition_NUS = null_source; break;
		}
		
		double NumberofRegions = initial_condition_NUS.Slopes.size();
		std::vector<std::vector<double>> domain(NumberofRegions, std::vector<double>(2)); 
		std::vector<std::vector<double>> source(NumberofRegions, std::vector<double>(2));
		Solver solver;

	
		// Fill source
		for (size_t i = 0; i < NumberofRegions; ++i) 
		{
			source[i][0] = initial_condition_NUS.Slopes[i];     // A
			source[i][1] = initial_condition_NUS.Intercepts[i]; // B
		}

		// Fill domain
		for (size_t i = 0; i < NumberofRegions; ++i) 
		{
			domain[i][0] = Sciantix_variables[0] * initial_condition_NUS.NormalizedDomain[i]; // edge1
			domain[i][1] = Sciantix_variables[0] * initial_condition_NUS.NormalizedDomain[i+1]; // edge2
		}

		for (n = 0; n < n_modes; ++n)
		{
			np1 = n + 1;
			double n_c = 0;
			for (size_t x = 0; x < NumberofRegions; ++x)
			{
				n_c += solver.SourceProjection_i(Sciantix_variables[0], domain[x], source[x], np1);
			}
			const double n_coeff = pow(-1.0, np1) / np1;
			Sciantix_diffusion_modes_NUS[k * n_modes + n] = -projection_coeff * n_c;
		}
	}		

	
	// Warnings
	if(Sciantix_variables[38] > 0.0 && Sciantix_variables[65] == 0.0)
		std::cout << "WARNING - FIMA calculation: Initial fuel burnup > 0 but null initial irradiation time. Check initial irradiation time." << std::endl;
}


