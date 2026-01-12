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

void Matrix::setLatticeParameter(SciantixArray<SciantixVariable> &sciantix_variable, double Cr_lattice_parameter)
{
    if (Cr_lattice_parameter > 0.0) //  lattice parameter for chromium-doped fuel in Microstructure.C
    {
        lattice_parameter = Cr_lattice_parameter;
        return;
    }

    lattice_parameter = 5.47e-10; // m for pure UO2
    matrix_density = 10960.0 ; // kg/m3 for pure UO2

    // MA-MOX fuel, NEA (2025), Recommendations on Fuel Properties for Fuel Performance Codes, OECD Publishing, Paris
    // The range of parameters for the validity of the recommendation is: O/M: 1.94-2.0, Pu: 10-45%, Am: 0-5%. 
    double q = sciantix_variable["q"].getFinalValue();
    double x = sciantix_variable["Stoichiometry deviation"].getFinalValue();
    double x_hypo = 0.0;
    // only for hypostoichiometric MOX
    if (x < 0.0) x_hypo = - x;

    if (q > 0.10) 
    {
        double U234 = sciantix_variable["U234"].getFinalValue();
        double U235 = sciantix_variable["U235"].getFinalValue();
        double U236 = sciantix_variable["U236"].getFinalValue();
        double U237 = sciantix_variable["U237"].getFinalValue();
        double U238 = sciantix_variable["U238"].getFinalValue();
        double U_tot = U234 + U235 + U236 + U237 + U238;

        double Pu238 = sciantix_variable["Pu238"].getFinalValue();
        double Pu239 = sciantix_variable["Pu239"].getFinalValue();
        double Pu240 = sciantix_variable["Pu240"].getFinalValue();
        double Pu241 = sciantix_variable["Pu241"].getFinalValue();
        double Pu242 = sciantix_variable["Pu242"].getFinalValue();
        double Pu_tot = Pu238 + Pu239 + Pu240 + Pu241 + Pu242;

        double Am_tot = 0.0;
        double Np_tot = 0.0;

        double HM_tot = U_tot + Pu_tot + Am_tot + Np_tot;

        // (U_{1-z-yAm-yNp} Pu_z Am_yAm Np_yNp) O2
        double zPu  = 0.0;
        double yAm  = 0.0;
        double yNp  = 0.0;
        if (HM_tot > 0.0)
        {
            zPu = Pu_tot / HM_tot;
            yAm = Am_tot / HM_tot;
            yNp = Np_tot / HM_tot;
        }
        double yU = std::max(0.0, 1.0 - zPu - yAm - yNp);
        
        // Ionic radii (Å), coordination number (CN): U4+(CN8), Pu4+(CN8), Am3+(CN8), Np4+(CN8), O2-(CN4)
        const double rU = 0.9972;
        const double rPu = 0.9642;
        const double rAm = 1.0900;
        const double rNp = 0.9806;
        const double rO  = 1.3720;

        double r_cation = rU*yU + rPu*zPu + rAm*yAm + rNp*yNp;
        // a = 4/sqrt(3) * ( r_cation*(1 + 0.112*x) + rO )
        double a_A = (4.0 / std::sqrt(3.0)) * (r_cation * (1.0 + 0.112 * x_hypo) + rO); // Å
        lattice_parameter = a_A * 1.0e-10; // m
    }
}

void Matrix::setTheoreticalDensity(SciantixArray<SciantixVariable> &sciantix_variable, double Cr_theoretical_density)
{
    if (Cr_theoretical_density > 0.0) //  density for chromium-doped fuel in Microstructure.C
    {
        matrix_density = Cr_theoretical_density;
        return;
    }

    lattice_parameter = 5.47e-10; // m for pure UO2
    matrix_density = 10960.0 ; // kg/m3 for pure UO2

    // MA-MOX fuel, NEA (2025), Recommendations on Fuel Properties for Fuel Performance Codes, OECD Publishing, Paris
    // The range of parameters for the validity of the recommendation is: O/M: 1.94-2.0, Pu: 10-45%, Am: 0-5%. 
    double q = sciantix_variable["q"].getFinalValue();
    double x = sciantix_variable["Stoichiometry deviation"].getFinalValue();
    double x_hypo = 0.0;
    // only for hypostoichiometric MOX
    if (x < 0.0) x_hypo = - x;

    if (q > 0.10) 
    {
        double U234 = sciantix_variable["U234"].getFinalValue();
        double U235 = sciantix_variable["U235"].getFinalValue();
        double U236 = sciantix_variable["U236"].getFinalValue();
        double U237 = sciantix_variable["U237"].getFinalValue();
        double U238 = sciantix_variable["U238"].getFinalValue();
        double U_tot = U234 + U235 + U236 + U237 + U238;

        double Pu238 = sciantix_variable["Pu238"].getFinalValue();
        double Pu239 = sciantix_variable["Pu239"].getFinalValue();
        double Pu240 = sciantix_variable["Pu240"].getFinalValue();
        double Pu241 = sciantix_variable["Pu241"].getFinalValue();
        double Pu242 = sciantix_variable["Pu242"].getFinalValue();
        double Pu_tot = Pu238 + Pu239 + Pu240 + Pu241 + Pu242;

        double Am_tot = 0.0;
        double Np_tot = 0.0;

        double HM_tot = U_tot + Pu_tot + Am_tot + Np_tot;

        // (U_{1-z-yAm-yNp} Pu_z Am_yAm Np_yNp) O2
        double zPu  = 0.0;
        double yAm  = 0.0;
        double yNp  = 0.0;
        if (HM_tot > 0.0)
        {
            zPu = Pu_tot / HM_tot;
            yAm = Am_tot / HM_tot;
            yNp = Np_tot / HM_tot;
        }
        double yU = std::max(0.0, 1.0 - zPu - yAm - yNp);


        // Ionic radii (Å), coordination number (CN): U4+(CN8), Pu4+(CN8), Am3+(CN8), Np4+(CN8), O2-(CN4)
        const double rU = 0.9972;
        const double rPu = 0.9642;
        const double rAm = 1.0900;
        const double rNp = 0.9806;
        const double rO  = 1.3720;

        double r_cation = rU*yU + rPu*zPu + rAm*yAm + rNp*yNp;
        // a = 4/sqrt(3) * ( r_cation*(1 + 0.112*x) + rO )
        double a_A = (4.0 / std::sqrt(3.0)) * (r_cation * (1.0 + 0.112 * x_hypo) + rO); // Å
        lattice_parameter = a_A * 1.0e-10; // m

        // Molar mass (g/mol)
        double M_U  = 238.02891; 
        double M_Pu = 239.05216; 
        double M_Am = 243.06138; // not used
        double M_Np = 237.04817; // not used
        double M_O = 15.999;

        if (U_tot > 0.0)
        {
            double mU234 = 234.04095;
            double mU235 = 235.04393;
            double mU236 = 236.04557;
            double mU237 = 237.04817;
            double mU238 = 238.05079;
            M_U = (U234*mU234 + U235*mU235 + U236*mU236 + U237*mU237 + U238*mU238) / U_tot;
        }

        if (Pu_tot > 0.0)
        {
            double mPu238 = 238.04956;
            double mPu239 = 239.05216;
            double mPu240 = 240.05381;
            double mPu241 = 241.05685;
            double mPu242 = 242.05874;
            M_Pu = (Pu238*mPu238 + Pu239*mPu239 + Pu240*mPu240 + Pu241*mPu241 + Pu242*mPu242) / Pu_tot;
        }

        // M_fu = yU*M_U + z*M_Pu + yAm*M_Am + yNp*M_Np + (2-x)*M_O
        double M_fu = yU*M_U + zPu*M_Pu + yAm*M_Am + yNp*M_Np + (2.0 - x_hypo)*M_O;
        
        // Fluorite: Z = 4 formula units per cella
        // rho = Z * M_fu / (NA * a^3) 
        matrix_density = (4.0 * M_fu * 1e-3) / (avogadro_number * std::pow(lattice_parameter, 3)); // kg/m3
    }
}