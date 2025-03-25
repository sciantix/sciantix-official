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

void Matrix::setGrainBoundaryVacancyDiffusivity(int input_value, SciantixArray<SciantixVariable> &history_variable, SciantixArray<SciantixVariable> &sciantix_variable)
{
    switch (input_value)
    {
        case 0:
        {
            grain_boundary_vacancy_diffusivity = 1e-30;
            reference += "iGrainBoundaryVacancyDiffusivity: constant value (1e-30 m^2/s).\n\t";

            break;
        }

        case 1:
        {
            grain_boundary_vacancy_diffusivity = 6.9e-04 * exp(- 5.35e-19 / (boltzmann_constant * history_variable["Temperature"].getFinalValue()));
            reference += "iGrainBoundaryVacancyDiffusivity: from Reynolds and Burton, JNM, 82 (1979) 22-25.\n\t";

            double alpha = sciantix_variable["Restructured volume fraction"].getFinalValue();
            double hbs_correction = sin(4.0 * (1. - alpha) * M_PI / 180.0 + 40.0 * alpha * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);

            grain_boundary_vacancy_diffusivity *= hbs_correction;

            break;
        }


        case 2:
        {
            grain_boundary_vacancy_diffusivity = 3.5/5.0 * 8.86e-6 * exp(- 5.75e-19 / ( boltzmann_constant * history_variable["Temperature"].getFinalValue()));
            reference += "iGrainBoundaryVacancyDiffusivity: from White, JNM, 325 (2004)\n\t";

            double alpha = sciantix_variable["Restructured volume fraction"].getFinalValue();
            double hbs_correction = sin(4.0 * (1. - alpha) * M_PI / 180.0 + 40.0 * alpha * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);

            grain_boundary_vacancy_diffusivity *= hbs_correction;

            break;
        }

        case 3:
        {
            grain_boundary_vacancy_diffusivity = 8.86e-6 * exp(- 5.75e-19 / ( boltzmann_constant * history_variable["Temperature"].getFinalValue())) + 1e-39 * history_variable["Fission rate"].getFinalValue();
            reference += "iGrainBoundaryVacancyDiffusivity: from White, JNM, 325 (2004), 61-77 / BARANI HBS part 2, 2022\n\t";

            double alpha = sciantix_variable["Restructured volume fraction"].getFinalValue();
            double hbs_correction = sin(4.0 * (1. - alpha) * M_PI / 180.0 + 40.0 * alpha * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);

            grain_boundary_vacancy_diffusivity *= hbs_correction;

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iGrainBoundaryVacancyDiffusivity", input_value);
            break;
    }
}

void Matrix::setGrainBoundarySingleAtomDiffusivity(int input_value, SciantixArray<SciantixVariable> &history_variable, SciantixArray<SciantixVariable> &sciantix_variable)
{
    switch (input_value)
    {
        case 0:
        {
            grain_boundary_single_atom_diffusivity = 1e-30;
            reference += "GrainBoundarySingleAtomDiffusivity: constant value (1e-30 m^2/s).\n\t";

            break;
        }

        case 1:
        {
            // SINGLE-ATOM
            grain_boundary_single_atom_diffusivity = (1.3e-7 * exp(-4.52e-19 /
                    (boltzmann_constant * history_variable["Temperature"].getFinalValue()))
            );

            double alpha = sciantix_variable["Restructured volume fraction"].getFinalValue();
            double hbs_correction = sin(4.0 * (1. - alpha) * M_PI / 180.0 + 40.0 * alpha * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);

            grain_boundary_single_atom_diffusivity *= hbs_correction;

            reference += "iGrainBoundaryVacancyDiffusivity: HBS case, from Barani et al., JNM 563 (2022) 153627.\n\t";
            break;
        }

        default:
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

    pore_nucleation_rate = (5.0e17 * 2.77e-7 * 3.54 * (1.0 - sciantix_variable["Restructured volume fraction"].getFinalValue()) * pow(sciantix_variable["Effective burnup"].getFinalValue()/0.8814, 2.54));

    pore_nucleation_rate *= sf_nucleation_rate_porosity;
}

void Matrix::setPoreResolutionRate(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    double correction_coefficient = (1.0 - exp(pow(-sciantix_variable["HBS pore radius"].getFinalValue() / (9e-9), 3)));
    double b0(2.0e-23 * history_variable["Fission rate"].getFinalValue());

    pore_resolution_rate =
        b0 *
        (3.0 * 1.0e-9 / (3.0 * 1.0e-9 + sciantix_variable["HBS pore radius"].getFinalValue())) *
        (1.0e-9 / (1.0e-9 + sciantix_variable["HBS pore radius"].getFinalValue()));

    pore_resolution_rate *= scaling_factors["Cent parameter"].getValue();
}
 
void Matrix::setPoreTrappingRate(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<InputVariable> &scaling_factors)
{
    pore_trapping_rate = 4.0 * M_PI * grain_boundary_single_atom_diffusivity *
        sciantix_variable["Xe at grain boundary"].getFinalValue() *
        sciantix_variable["HBS pore radius"].getFinalValue() *
        (1.0 + 1.8 * pow(sciantix_variable["HBS porosity"].getFinalValue(), 1.3));

    pore_trapping_rate *= scaling_factors["Dummy"].getValue();
}
