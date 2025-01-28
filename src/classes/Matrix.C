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

#include "Matrix.h"

void Matrix::setGrainBoundaryMobility(int input_value, SciantixArray<SciantixVariable> &history_variable)
{
    switch (input_value)
    {
    case 0:
    {
        reference += "Null grain-boundary mobility.\n\t";
        grain_boundary_mobility = 0.0;

        break;
    }

    case 1:
    {
        reference += "Ainscough et al., JNM, 49 (1973) 117-128.\n\t";
        grain_boundary_mobility = 1.455e-8 * exp(- 32114.5 / history_variable["Temperature"].getFinalValue());
        break;
    }

    case 2 :
    {
        reference += "Van Uffelen et al. JNM, 434 (2013) 287-29.\n\t";
        grain_boundary_mobility = 1.360546875e-15 * exp(- 46524.0 / history_variable["Temperature"].getFinalValue());
        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "iGrainGrowth", input_value);
        break;
    }
}

void Matrix::setGrainBoundaryVacancyDiffusivity(int input_value, SciantixArray<SciantixVariable> &history_variable)
{
    switch (input_value)
    {
        case 0:
        {
            grain_boundary_diffusivity = 1e-30;
            reference += "iGrainBoundaryVacancyDiffusivity: constant value (1e-30 m^2/s).\n\t";

            break;
        }

        case 1:
        {
            grain_boundary_diffusivity = 6.9e-04 * exp(- 5.35e-19 / (boltzmann_constant * history_variable["Temperature"].getFinalValue()));
            reference += "iGrainBoundaryVacancyDiffusivity: from Reynolds and Burton, JNM, 82 (1979) 22-25.\n\t";

            break;
        }

        case 2:
        {
            grain_boundary_diffusivity = 3.5/5 * 8.86e-6 * exp(- 4.17e4 / history_variable["Temperature"].getFinalValue());
            reference += "iGrainBoundaryVacancyDiffusivity: from White, JNM, 325 (2004), 61-77.\n\t";

            break;
        }

        case 5:
        {
            grain_boundary_diffusivity = (1.3e-7 * exp(-4.52e-19 /
                    (boltzmann_constant * history_variable["Temperature"].getFinalValue()))
            );

            reference += "iGrainBoundaryVacancyDiffusivity: HBS case, from Barani et al., JNM 563 (2022) 153627.\n\t";
            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iGrainBoundaryVacancyDiffusivity", input_value);
            break;
    }
}

void Matrix::setPoreNucleationRate(SciantixArray<SciantixVariable> &sciantix_variable)
{
    /**
     * @brief nucleation rate of HBS pores.
     * This model is from @ref *Barani et al., JNM 563 (2022) 153627*.
     *
     */

    double sf_nucleation_rate_porosity = 1.25e-6; // from dburnup to dtime

    pore_nucleation_rate =
            (5.0e17 * 2.77e-7 * 3.54 * (1.0-sciantix_variable["Restructured volume fraction"].getFinalValue()) *
        pow(sciantix_variable["Effective burnup"].getFinalValue(), 2.54));

    pore_nucleation_rate *= sf_nucleation_rate_porosity;
}

void Matrix::setPoreResolutionRate(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable)
{
    double correction_coefficient = (1.0 - exp(pow(-sciantix_variable["HBS pore radius"].getFinalValue() / (9.0e-9), 3)));
    double b0(2.0e-23 * history_variable["Fission rate"].getFinalValue());

    pore_resolution_rate =
        b0 * correction_coefficient *
        (3.0 * 1.0e-9 / (3.0 * 1.0e-9 + sciantix_variable["HBS pore radius"].getFinalValue())) *
        (1.0e-9 / (1.0e-9 + sciantix_variable["HBS pore radius"].getFinalValue()));
}

void Matrix::setPoreTrappingRate(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable)
{
    pore_trapping_rate = 4.0 * M_PI * grain_boundary_diffusivity *
        sciantix_variable["Xe at grain boundary"].getFinalValue() *
        sciantix_variable["HBS pore radius"].getFinalValue() *
        (1.0 + 1.8 * pow(sciantix_variable["HBS porosity"].getFinalValue(), 1.3));
}

void Matrix::setElasticModulus(int input_value, SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable)
{
    switch (input_value)
	{
        case 0:
        {
            /// @brief
            /// iElasticModulus == 0
            /// ----------------------------------
            ///
            /// This case assumes constant trial value for the fuel elastic modulus

            const double E0 = 223700; //(MPa) 

            elastic_modulus = E0;

            break;
        }

        case 1:
        {
            /// @brief
            /// iElasticModulus == 1
            /// ----------------------------------
            ///
            /// This case assumes a value of the fuel elastic modulus accordingly to Lassmann and Moreno (1977)

            const double E0 = 223700; //(MPa) 

            double porosity = 1 - sciantix_variable["Fuel density"].getFinalValue()/matrix_density;

            elastic_modulus = E0 * (1 - 2.6 * porosity) * (1 - 1.394e-4 * (history_variable["Temperature"].getFinalValue() - 273.15 - 20)) * 
                                    (1 - 0.1506 * (1 - exp(-0.035 * sciantix_variable["Burnup"].getFinalValue())));

            break;
        }
    }
}

void Matrix::setPoissonRatio(int input_value)
{
    switch (input_value)
	{
        case 0:
        {
            /// @brief
            /// iElasticModulus == 0
            /// ----------------------------------
            ///
            /// This case assumes constant value for the fuel poisson ratio accordingly to MATPRO (1977)

            poisson_ratio = 0.316;

            break;
        }
    }
}

void Matrix::setThermalConductivity(int input_value, SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable)
{
    switch (input_value)
	{
        case 0:
        {
            /// @brief
            /// iThermalConductivity == 0
            /// ----------------------------------
            ///
            /// This case assumes constant trial value for the fuel thermal conductivity

            thermal_conductivity = 2;

            break;
        }

        case 1:
        {
            /// @brief
            /// iThermalConductivity == 1
            /// ----------------------------------
            ///
            /// This case assumes constant trial value for the fuel thermal conductivity accordingly to MATPRO (1977)

            thermal_conductivity = 0.316;

            break;
        }

        case 2:
        {
            /// @brief
            /// iThermalConductivity == 2
            /// ----------------------------------
            ///
            /// This case assumes constant trial value for the MOX fuel thermal conductivity accordingly to Magni et al. (2020)

            double A_0  = 0.01926;
            double A_x  = 1.06e-6;
            double A_pu = 2.63e-8;
            double A    = A_0 + A_x * sciantix_variable["Stoichiometry deviation"].getFinalValue() + A_pu * sciantix_variable["Plutonium fraction"].getFinalValue();

            double B_0  = 2.39e-4;
            double B_pu = 1.37e-13;
            double B    = B_0 + B_pu * sciantix_variable["Plutonium fraction"].getFinalValue(); 

            double D    = 5.27e9;
            double E    = 17109.5;

            double porosity = 1 - matrix_density / sciantix_variable["Fuel density"].getFinalValue();

            double k_0 = (1/(A + B * history_variable["Temperature"].getFinalValue()) + D/pow(history_variable["Temperature"].getFinalValue(),2) * exp(-E/history_variable["Temperature"].getFinalValue())) * pow((1 - porosity), 2.5);
            double k_inf = 1.755;

            thermal_conductivity = k_inf + (k_0 - k_inf)*exp(- sciantix_variable["Burnup"].getFinalValue()/128.75);

            break;
        }

    }
}
