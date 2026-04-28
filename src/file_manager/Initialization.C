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
//  Version: 2.2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Initialization.h"

void Initialization(double Sciantix_history[],
                    double Sciantix_variables[],
                    double Sciantix_diffusion_modes[],
                    // CODE DEVELOPMENT : THERMOCHEMISTRY VARIABLES/OPTIONS
                    double Sciantix_thermochemistry[],
                    //
                    std::vector<double> Temperature_input,
                    std::vector<double> Fissionrate_input,
                    std::vector<double> Hydrostaticstress_input,
                    std::vector<double> Steampressure_input,
                    // CODE DEVELOPMENT : SYSTEM PRESSURE AND O/M RATIO INPUTS
                    std::vector<double> Systempressure_input,
                    std::vector<double> OMratio_input)
{
    // Sciantix_history initialization
    Sciantix_history[0] = Temperature_input[0];
    Sciantix_history[1] = Temperature_input[0];
    Sciantix_history[2] = Fissionrate_input[0];
    Sciantix_history[3] = Fissionrate_input[0];
    Sciantix_history[4] = Hydrostaticstress_input[0];
    Sciantix_history[5] = Hydrostaticstress_input[0];
    Sciantix_history[6] = 0.0;

    Sciantix_history[9]  = Steampressure_input[0];
    Sciantix_history[10] = Steampressure_input[0];

    // CODE DEVELOPMENT : SYSTEM PRESSURE AND O/M RATIO INPUTS
    Sciantix_history[11] = Systempressure_input[0];
    Sciantix_history[12] = Systempressure_input[0];
    Sciantix_history[13] = OMratio_input[0];
    Sciantix_history[14] = OMratio_input[0];
    //

    // Sciantix_variables initialization
    Sciantix_variables[25] = 2.0e+13;  // Intergranular_bubble_concentration[0]
    Sciantix_variables[35] = 0.5;      // Intergranular_saturation_fractional_coverage[0]
    Sciantix_variables[37] = 1.0;      // Intergranular_fractional_intactness[0]

    // CODE DEVELOPMENT : GENERALIZATION FROM UO2 TO ALL MATRICES
    const double density_mix     = Sciantix_variables[40];
    const double q               = Sciantix_variables[177];
    const double avogadro_number = 6.022e23;

    // const double molar_mass_oxygen = 15.999;

    // const double molar_mass_uranium =
    //     Sciantix_variables[41] * 234.04095 +
    //     Sciantix_variables[42] * 235.04393 +
    //     Sciantix_variables[43] * 236.04557 +
    //     Sciantix_variables[44] * 237.04873 +
    //     Sciantix_variables[45] * 238.05079;

    // const double molar_mass_plutonium =
    //     Sciantix_variables[171] * 238.04956 +
    //     Sciantix_variables[172] * 239.05216 +
    //     Sciantix_variables[173] * 240.05381 +
    //     Sciantix_variables[174] * 241.05685 +
    //     Sciantix_variables[175] * 242.05874;

    // const double molar_mass_mix =
    //     (2.0 + Sciantix_variables[66]) * molar_mass_oxygen +
    //     (1.0 - q) * molar_mass_uranium +
    //     q * molar_mass_plutonium;

    // Correct: Sciantix_variables[i] *= density_mix * q * avogadro_number * 10.0 / molar_mass_mix;

    Sciantix_variables[41] *= density_mix * (1.0 - q) * avogadro_number * 10.0 * 0.8815 / 234.04095;
    Sciantix_variables[42] *= density_mix * (1.0 - q) * avogadro_number * 10.0 * 0.8815 / 235.04393;
    Sciantix_variables[43] *= density_mix * (1.0 - q) * avogadro_number * 10.0 * 0.8815 / 236.04557;
    Sciantix_variables[44] *= density_mix * (1.0 - q) * avogadro_number * 10.0 * 0.8815 / 237.04873;
    Sciantix_variables[45] *= density_mix * (1.0 - q) * avogadro_number * 10.0 * 0.8815 / 238.05079;

    Sciantix_variables[171] *= density_mix * q * avogadro_number * 10.0 * 0.8815 / 238.04956;
    Sciantix_variables[172] *= density_mix * q * avogadro_number * 10.0 * 0.8815 / 239.05216;
    Sciantix_variables[173] *= density_mix * q * avogadro_number * 10.0 * 0.8815 / 240.05381;
    Sciantix_variables[174] *= density_mix * q * avogadro_number * 10.0 * 0.8815 / 241.05685;
    Sciantix_variables[175] *= density_mix * q * avogadro_number * 10.0 * 0.8815 / 242.05874;

    double total_U = Sciantix_variables[41] + Sciantix_variables[42] + Sciantix_variables[43] + Sciantix_variables[44] +
                     Sciantix_variables[45];
    double total_Pu = Sciantix_variables[171] + Sciantix_variables[172] + Sciantix_variables[173] +
                      Sciantix_variables[174] + Sciantix_variables[175];
    //

    // Intragranular similarity ratio
    Sciantix_variables[64] = 1.0;

    // CODE DEVELOPMENT : FISSION PRODUCTS
    Sciantix_variables[101] = Sciantix_variables[102] = Sciantix_variables[103] = Sciantix_variables[104] =
        Sciantix_variables[105] = Sciantix_variables[106] = Sciantix_variables[107] = Sciantix_variables[108] =
            Sciantix_variables[109] = Sciantix_variables[110] = 0.0;
    Sciantix_variables[111] = Sciantix_variables[112] = Sciantix_variables[113] = Sciantix_variables[114] =
        Sciantix_variables[115] = Sciantix_variables[116] = Sciantix_variables[117] = Sciantix_variables[118] =
            Sciantix_variables[119] = Sciantix_variables[120] = 0.0;
    Sciantix_variables[121] = Sciantix_variables[122] = Sciantix_variables[123] = Sciantix_variables[124] =
        Sciantix_variables[125] = Sciantix_variables[126] = Sciantix_variables[127] = Sciantix_variables[128] =
            Sciantix_variables[129] = Sciantix_variables[130] = 0.0;
    //

    // Fabrication porosity = Porosity
    Sciantix_variables[71] = Sciantix_variables[70] = 1.0 - Sciantix_variables[40] / 10960.0;

    // Residual porosity
    Sciantix_variables[73] = 0.75 * Sciantix_variables[71];

    // CODE DEVELOPMENT : U and O content
    Sciantix_variables[161] = total_U / avogadro_number;
    Sciantix_variables[163] = total_Pu / avogadro_number;
    Sciantix_variables[162] = (2.0 + Sciantix_variables[66]) * (Sciantix_variables[161] + Sciantix_variables[163]);
    //

    // Projection on diffusion modes of the initial conditions
    double initial_condition(0.0);
    double projection_remainder(0.0);
    double reconstructed_solution(0.0);
    int    iteration(0), iteration_max(20), n(0), np1(1), n_modes(40), k(0), K(18);
    double projection_coeff(0.0);
    projection_coeff = -sqrt(8.0 / M_PI);

    for (k = 0; k < K; ++k)
    {
        switch (k)
        {
            case 0:
                initial_condition = Sciantix_variables[2];
                break;  // Xe in grain
            case 1:
                initial_condition = Sciantix_variables[3];
                break;  // Xe in grain - solution
            case 2:
                initial_condition = Sciantix_variables[4];
                break;  // Xe in grain - bubbles

            case 3:
                initial_condition = Sciantix_variables[8];
                break;  // Kr in grain
            case 4:
                initial_condition = Sciantix_variables[9];
                break;  // Kr in grain - solution
            case 5:
                initial_condition = Sciantix_variables[10];
                break;  // Kr in grain - bubbles

            case 6:
                initial_condition = Sciantix_variables[14];
                break;  // He in grain
            case 7:
                initial_condition = Sciantix_variables[15];
                break;  // He in grain - solution
            case 8:
                initial_condition = Sciantix_variables[16];
                break;  // He in grain - bubbles

            case 9:
                initial_condition = Sciantix_variables[49];
                break;  // Xe133 in grain
            case 10:
                initial_condition = Sciantix_variables[50];
                break;  // Xe133 in grain - solution
            case 11:
                initial_condition = Sciantix_variables[51];
                break;  // Xe133 in grain - bubbles

            case 12:
                initial_condition = Sciantix_variables[58];
                break;  // Kr85m in grain
            case 13:
                initial_condition = Sciantix_variables[59];
                break;  // Kr85m in grain - solution
            case 14:
                initial_condition = Sciantix_variables[60];
                break;  // Kr85m in grain - bubbles

            case 15:
                initial_condition = Sciantix_variables[92];
                break;  // Xe in UO2 HBS
            case 16:
                initial_condition = Sciantix_variables[93];
                break;  // Xe in UO2 HBS - solution
            case 17:
                initial_condition = Sciantix_variables[94];
                break;  // Xe in UO2 HBS - bubbles

            default:
                initial_condition = 0.0;
                break;
        }

        projection_remainder = initial_condition;
        for (iteration = 0; iteration < iteration_max; ++iteration)
        {
            reconstructed_solution = 0.0;
            for (n = 0; n < n_modes; ++n)
            {
                if (iteration == 0)
                    Sciantix_diffusion_modes[k * n_modes + n] = 0;
                np1                  = n + 1;
                const double n_coeff = pow(-1.0, np1) / np1;
                Sciantix_diffusion_modes[k * n_modes + n] += projection_coeff * n_coeff * projection_remainder;
                reconstructed_solution +=
                    projection_coeff * n_coeff * Sciantix_diffusion_modes[k * n_modes + n] * 3.0 / (4.0 * M_PI);
            }
            projection_remainder = initial_condition - reconstructed_solution;
        }
    }

    // Warnings
    if (Sciantix_variables[38] > 0.0 && Sciantix_variables[65] == 0.0)
        std::cout << "WARNING - FIMA calculation: Initial fuel burnup > 0 but null initial "
                     "irradiation time. Check initial irradiation time."
                  << std::endl;
}
