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

#include "System.h"

void System::setRestructuredMatrix(bool y)
{
    restructured_matrix = y;
}

bool System::getRestructuredMatrix()
{
    return restructured_matrix;
}

void System::setYield(double y)
{
    /// Member function to set the (cumulative) yield of the fission gas (at/fiss).
    yield = y;
}

double System::getYield()
{
    /// Member function to get the (cumulative) yield of the fission gas (at/fiss).
    return yield;
}

void System::setRadiusInLattice(double r)
{
    /// Member function to set the radius of the fission gas atom in the matrix lattice (m).
    radius_in_lattice = r;
}

double System::getRadiusInLattice()
{
    /// Member function to get the radius of the fission gas atom in the matrix lattice (m).
    return radius_in_lattice;
}

void System::setGas(Gas g)
{
    /// Member function to set the name of the gas in the matrix
    gas = g;
}

Gas System::getGas()
{
    return gas;
}

std::string System::getGasName()
{
    /// Member function to get the name of the gas in the matrix
    return gas.getName();
}

void System::setMatrix(Matrix m)
{
    /// Member function to set the name of the matrix
    matrix = m;
}

Matrix System::getMatrix()
{
    return matrix;
}

std::string System::getMatrixName()
{
    /// Member function to get the name of the matrix
    return matrix.getName();
}

double System::getVolumeInLattice()
{
    /// Member function to get the volume occupied by the gas in matrix
    return volume_in_lattice;
}

void System::setVolumeInLattice(double v)
{
    /// Member function to set the volume occupied by the gas in matrix
    volume_in_lattice = v;
}

void System::setBubbleDiffusivity(int input_value, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<Matrix> &matrices)
{
    switch (input_value)
    {
    case 0:
    {
        bubble_diffusivity = 0;
        break;
    }

    case 1:
    {
        if (sciantix_variable["Intragranular bubble radius"].getInitialValue() == 0)
            bubble_diffusivity = 0;

        else
        {

            /**
             *
             * Assuming that the bubble motion during irradiation is controlled by the volume diffusion mechanism,
             * the bubble diffusivty takes the form Db = (V_atom_in_lattice/bubble_volume) * volume_self_diffusivity
             *
             * @see <a href="../../references/pdf_link/Evans_1994.pdf" target="_blank">Evans, JNM 210 (1994) 21-29</a>,
             * <a href="../../references/pdf_link/Van_Uffelen_et_al_2013.pdf" target="_blank">Van Uffelen et al. JNM, 434 (2013) 287–29</a>.
             */

            double volume_self_diffusivity = 3.0e-5 * exp(-4.5 / (boltzmann_constant * history_variable["Temperature"].getFinalValue()));
            double bubble_radius = sciantix_variable["Intragranular bubble radius"].getInitialValue();

            bubble_diffusivity = 3 * matrices["UO2"].getSchottkyVolume() * volume_self_diffusivity / (4.0 * M_PI * pow(bubble_radius, 3.0));
        }

        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "iBubbleDiffusivity", input_value);
        break;
    }
}

double System::getBubbleDiffusivity()
{
    /// Member function to get the bubble diffusivity of the isotope in the fuel matrix
    return bubble_diffusivity;
}

void System::setHeliumDiffusivity(int input_value, SciantixArray<SciantixVariable> &history_variable)
{

    /**
     * ### setHeliumDiffusivity
     * @brief The intra-granular helium diffusivity within the fuel grain is set according to the input_variable iHeDiffusivity
     *
     */
    switch (input_value)
    {
    case 0:
    {
        /**
         * @brief iHeDiffusivity = 0 corresponds to a constant intra-granular diffusivity value
         *
         */

        reference += "iHeDiffusivity: constant intragranular diffusivity.\n\t";
        diffusivity = 7e-19;
        break;
    }

    case 1:
    {
        /**
         * @brief iHeDiffusivity = 1 is the best-estimate correlation, from data available in literature, for samples with no or very limited lattice damage.
         * This correlation is also recommended for simulations of helium in UO<sub>2</sub> samples in which **infusion** technique has been adopted.
         * @see The correlation is from <a href="../../references/pdf_link/Luzzi_et_al_2018.pdf" target="_blank">L. Luzzi et al., Nuclear Engineering and Design, 330 (2018) 265-271</a>.
         *
         */

        reference += "(no or very limited lattice damage) L. Luzzi et al., Nuclear Engineering and Design, 330 (2018) 265-271.\n\t";
        diffusivity = 2.0e-10 * exp(-24603.4 / history_variable["Temperature"].getFinalValue());
        break;
    }

    case 2:
    {
        /**
         * @brief iHeDiffusivity = 2 is the best-estimate correlation, from data available in literature, for samples with significant lattice damage.
         * This correlation is also recommended for simulations of helium in UO<sub>2</sub> samples in which **implantation** technique has been adopted.
         * @see The correlation is from <a href="../../references/pdf_link/Luzzi_et_al_2018.pdf" target="_blank">L. Luzzi et al., Nuclear Engineering and Design, 330 (2018) 265-271</a>.
         *
         */

        reference += "(significant lattice damage) L. Luzzi et al., Nuclear Engineering and Design, 330 (2018) 265-271.\n\t";
        diffusivity = 3.3e-10 * exp(-19032.8 / history_variable["Temperature"].getFinalValue());
        break;
    }

    case 3:
    {
        /**
         * @brief iHeDiffusivity = 2 sets the single gas-atom intra-granular diffusivity equal to the correlation reported in <a href="../../references/pdf_link/Talip_et_al_2014.pdf" target="_blank">Z. Talip et al. JNM 445 (2014) 117-127</a>.
         *
         */

        reference += "iHeDiffusivity: Z. Talip et al. JNM 445 (2014) 117-127.\n\t";
        diffusivity = 1.0e-7 * exp(-30057.9 / history_variable["Temperature"].getFinalValue());
        break;
    }

    case 99:
    {
        /**
         * @brief iHeDiffusivity = 4 corresponds to a null intra-granular diffusivity value
         *
         */

        reference += "iHeDiffusivity: null intragranular diffusivity.\n\t";
        diffusivity = 0.0;
        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "iHeDiffusivity", input_value);
        break;
    }
}

double System::getHeliumDiffusivity()
{
    /// Member function to get the bubble diffusivity of the isotope in the fuel matrix
    return diffusivity;
}

void System::setFissionGasDiffusivity(int input_value, SciantixArray<SciantixVariable> &sciantix_variable,
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors)
{
    /**
     * ### setFissionGasDiffusivity
     * @brief The intra-granular fission gas (xenon and krypton) diffusivity within the fuel grain is set according to the input_variable iFissionGasDiffusivity
     *
     */
    switch (input_value)
    {
    case 0:
    {
        /**
         * @brief iFissionGasDiffusivity = 0 corresponds to a constant intra-granular diffusivity value, equal to 7e-19 m^2/s.
         *
         */

        reference += "iFissionGasDiffusivity: constant diffusivity (7e-19 m2/s).\n\t";
        diffusivity = 7e-19;
        diffusivity *= scaling_factors["Diffusivity"].getValue();

        break;
    }

    case 1:
    {
        /**
         * @brief iFissionGasDiffusivity = 1 set the fission gas (xenon and krypton) single-atom intragranular diffusivity equal to the expression
         * in <a href="../../references/pdf_link/Turnbull_et_al_1988.pdf" target="_blank">Turnbull et al (1988), IWGFPT-32, Preston, UK, Sep 18-22</a>.
         *
         */

        reference += "iFissionGasDiffusivity: Turnbull et al (1988), IWGFPT-32, Preston, UK, Sep 18-22.\n\t";

        double temperature = history_variable["Temperature"].getFinalValue();
        double fission_rate = history_variable["Fission rate"].getFinalValue();

        double d1 = 7.6e-10 * exp(-4.86e-19 / (boltzmann_constant * temperature));
        double d2 = 4.0 * 1.41e-25 * sqrt(fission_rate) * exp(-1.91e-19 / (boltzmann_constant * temperature));
        double d3 = 8.0e-40 * fission_rate;

        diffusivity = d1 + d2 + d3;
        diffusivity *= scaling_factors["Diffusivity"].getValue();

        break;
    }

    case 2:
    {
        /**
         * @brief iFissionGasDiffusivity = 2 set the xenon effective intragranular diffusivity equal to the expression
         * in <a href="../../references/pdf_link/Matzke_1980.pdf" target="_blank">Matzke (1980), Radiation Effects, 53, 219-242</a>.
         *
         */

        reference += "iFissionGasDiffusivity: Matzke (1980), Radiation Effects, 53, 219-242.\n\t";
        diffusivity = 5.0e-08 * exp(-40262.0 / history_variable["Temperature"].getFinalValue());
        diffusivity *= scaling_factors["Diffusivity"].getValue();

        break;
    }

    case 3:
    {
        /**
         * @brief iFissionGasDiffusivity = 3 set the xenon single-atom intragranular diffusivity equal to the expression
         * in <a href="../../references/pdf_link/Turnbull_et_al_2010.pdf" target="_blank">Turnbull et al., (2010), Background and Derivation of ANS-5.4 Standard Fission Product Release Model</a>.
         *
         */

        reference += "iFissionGasDiffusivity: Turnbull et al., (2010), Background and Derivation of ANS-5.4 Standard Fission Product Release Model.\n\t";

        double temperature = history_variable["Temperature"].getFinalValue();
        double fission_rate = history_variable["Fission rate"].getFinalValue();

        double d1 = 7.6e-11 * exp(-4.86e-19 / (boltzmann_constant * temperature));
        double d2 = 1.41e-25 * sqrt(fission_rate) * exp(-1.91e-19 / (boltzmann_constant * temperature));
        double d3 = 2.0e-40 * fission_rate;

        diffusivity = d1 + d2 + d3;
        diffusivity *= scaling_factors["Diffusivity"].getValue();

        break;
    }

    case 4:
    {
        /**
         * @brief iFissionGasDiffusivity = 4 set the xenon single-atom intragranular diffusivity equal to the expression
         * in <a href="../../references/pdf_link/Ronchi_2007.pdf" target="_blank">Ronchi, C. High Temp 45, 552-571 (2007)</a>.
         *
         */

        reference += "iFissionGasDiffusivity: Ronchi, C. High Temp 45, 552-571 (2007).\n\t";

        double temperature = history_variable["Temperature"].getFinalValue();
        double fission_rate = history_variable["Fission rate"].getFinalValue();

        double d1 = 7.6e-10 * exp(-4.86e-19 / (boltzmann_constant * temperature));
        double d2 = 6.64e-25 * sqrt(fission_rate) * exp(-1.91e-19 / (boltzmann_constant * temperature));
        double d3 = 1.2e-39 * fission_rate;

        diffusivity = d1 + d2 + d3;
        diffusivity *= scaling_factors["Diffusivity"].getValue();

        break;
    }

    case 5:
    {
        /**
         * @brief this case is for the UO2HBS.
         * @see This value is from <a href="../../references/pdf_link/Barani_et_al_2020.pdf" target="_blank">Barani et al. Journal of Nuclear Materials 539 (2020) 152296</a>.
         *
         */

        diffusivity = 4.5e-42 * history_variable["Fission rate"].getFinalValue();
        diffusivity *= scaling_factors["Diffusivity"].getValue();

        reference += "HBS : Inert fission gas diffusivity in UO2-HBS.\n\t";
        break;
    }

    case 6:
    {
        /**
         * @brief this case is for
         *
         */
        double x = sciantix_variable["Stoichiometry deviation"].getFinalValue();
        double temperature = history_variable["Temperature"].getFinalValue();
        double fission_rate = history_variable["Fission rate"].getFinalValue();

        double d1 = 7.6e-10 * exp(-4.86e-19 / (boltzmann_constant * temperature));
        double d2 = 4.0 * 1.41e-25 * sqrt(fission_rate) * exp(-1.91e-19 / (boltzmann_constant * temperature));
        double d3 = 8.0e-40 * fission_rate;

        double S = exp(-74100 / temperature);
        double G = exp(-35800 / temperature);
        double uranium_vacancies = 0.0;

        uranium_vacancies = S / pow(G, 2.0) * (0.5 * pow(x, 2.0) + G + 0.5 * pow((pow(x, 4.0) + 4 * G * pow(x, 2.0)), 0.5));

        double d4 = pow(3e-10, 2) * 1e13 * exp(-27800 / temperature) * uranium_vacancies;

        diffusivity = d1 + d2 + d3 + d4;

        diffusivity *= scaling_factors["Diffusivity"].getValue();

        break;
    }

    case 99:
    {
        /**
         * @brief iFissionGasDiffusivity = 99 set the xenon single-atom intragranular diffusivity to zero.
         *
         */

        reference += "iFissionGasDiffusivity: Test case: zero diffusion coefficient.\n\t";
        diffusivity = 0.0;

        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "iFissionGasDiffusivity", input_value);
        break;
    }
}

double System::getFissionGasDiffusivity()
{
    /// Member function to get the diffusivity of the isotope in the fuel matrix
    return diffusivity;
}

void System::setHenryConstant(double h)
{
    /// Member function to set the value of the Henry constant
    henry_constant = h;
}

double System::getHenryConstant()
{
    /// Member function to get the value of the Henry constant
    return henry_constant;
}

void System::setResolutionRate(int input_value, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors, SciantixArray<Matrix> &matrices)
{
    /**
     * ### setResolutionRate
     * @brief The helium intra-granular resolution rate is set according to the input_variable iResolutionRate.
     *
     */

    switch (input_value)
    {
    case 0:
    {
        /**
         * @brief iResolutionRate = 0 corresponds to a constant intra-granular resolution rate, equal to 0.0001 1/s.
         * @see This value is from <a href="../../references/pdf_link/Olander_and_Wongsawaeng_2006.pdf" target="_blank">Olander, Wongsawaeng, JNM, 354 (2006), 94-109</a>.
         *
         */

        reference += "iResolutionRate: Constant resolution rate from Olander, Wongsawaeng, JNM, 354 (2006), 94-109.\n\t";
        resolution_rate = 1.0e-4;
        resolution_rate *= scaling_factors["Resolution rate"].getValue();
        break;
    }

    case 1:
    {
        /**
         * @brief iResolutionRate = 1 corresponds to the irradiation-induced intra-granular resolution rate from <a href="../../references/pdf_link/Turnbull_1971.pdf" target="_blank">J.A. Turnbull, JNM, 38 (1971), 203</a>.
         *
         */

        reference += "iResolutionRate: J.A. Turnbull, JNM, 38 (1971), 203.\n\t";
        resolution_rate = 2.0 * M_PI * matrices["UO2"].getFissionFragmentRange() * pow(matrices["UO2"].getFissionFragmentInfluenceRadius() + sciantix_variable["Intragranular bubble radius"].getFinalValue(), 2) * history_variable["Fission rate"].getFinalValue();
        resolution_rate *= scaling_factors["Resolution rate"].getValue();

        break;
    }

    case 2:
    {
        /**
         * @brief iResolutionRate = 2 corresponds to the irradiation-induced intra-granular resolution rate from <a href="../../references/pdf_link/Losonen_2002.pdf" target="_blank">P. Losonen, JNM 304 (2002) 29-49</a>.

         *
         */

        reference += "iResolutionRate: P. Losonen, JNM 304 (2002) 29�49.\n\t";
        resolution_rate = 3.0e-23 * history_variable["Fission rate"].getFinalValue();
        resolution_rate *= scaling_factors["Resolution rate"].getValue();

        break;
    }

    case 3:
    {
        /**
         * @brief iResolutionRate = 3 corresponds to the intra-granular resolution rate from <a href="../../references/pdf_link/Cognini_et_al_2021.pdf" target="_blank">Cognini et al. NET 53 (2021) 562-571</a>.
         *
         * iResolutionRate = 3 includes the helium solubility in the resolution rate, with a thermal resolution term.
         *
         */

        reference += "iResolutionRate: Cognini et al. NET 53 (2021) 562-571.\n\t";

        /// irradiation_resolution_rate
        double irradiation_resolution_rate = 2.0 * M_PI * matrices["UO2"].getFissionFragmentRange() * pow(matrices["UO2"].getFissionFragmentInfluenceRadius() + sciantix_variable["Intragranular bubble radius"].getFinalValue(), 2) * history_variable["Fission rate"].getFinalValue();


        /// compressibility_factor
        double helium_hard_sphere_diameter = 2.973e-10 * (0.8414 - 0.05 * log(history_variable["Temperature"].getFinalValue() / 10.985)); // (m)
        double helium_volume_in_bubble = matrices["UO2"].getOctahedralInterstitialSite();                                                                         // 7.8e-30, approximation of saturated nanobubbles
        double y = M_PI * pow(helium_hard_sphere_diameter, 3) / (6.0 * helium_volume_in_bubble);
        double compressibility_factor = (1.0 + y + pow(y, 2) - pow(y, 3)) / (pow(1.0 - y, 3));

        /// thermal_resolution_rate
        // thermal_resolution_rate = 3 D k_H k_B T Z / R_b^2
        double thermal_resolution_rate;
        if (sciantix_variable["Intragranular bubble radius"].getFinalValue() > 0.0)
        {
            thermal_resolution_rate = 3.0 * diffusivity * henry_constant * boltzmann_constant * history_variable["Temperature"].getFinalValue() * compressibility_factor / pow(sciantix_variable["Intragranular bubble radius"].getFinalValue(), 2);
            if (sciantix_variable["Intragranular bubble radius"].getFinalValue() < (2.0 * radius_in_lattice))
                thermal_resolution_rate = 3 * diffusivity * henry_constant * boltzmann_constant * history_variable["Temperature"].getFinalValue() * compressibility_factor / pow(sciantix_variable["Intragranular bubble radius"].getFinalValue(), 2) - 2.0 * 3.0 * diffusivity * henry_constant * boltzmann_constant * history_variable["Temperature"].getFinalValue() * compressibility_factor * (sciantix_variable["Intragranular bubble radius"].getFinalValue() - radius_in_lattice) / pow(radius_in_lattice, 3) + 3.0 * 3.0 * diffusivity * henry_constant * boltzmann_constant * history_variable["Temperature"].getFinalValue() * compressibility_factor * pow(sciantix_variable["Intragranular bubble radius"].getFinalValue() - radius_in_lattice, 2) / pow(radius_in_lattice, 4);
        }
        else
            thermal_resolution_rate = 0.0;

        resolution_rate = irradiation_resolution_rate + thermal_resolution_rate;
        resolution_rate *= scaling_factors["Resolution rate"].getValue();


        break;
    }

    case 99:
    {
        /**
         * @brief iResolutionRate = 99 corresponds to a null intra-granular resolution rate.
         *
         */

        reference += "iResolutionRate: Null resolution rate.\n\t";
        resolution_rate = 0.0;
        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "iResolutionRate", input_value);
        break;
    }
    resolution_rate *= scaling_factors["Resolution rate"].getValue();
}

double System::getResolutionRate()
{
    /// Member function to get the value of the resolution rate of the isotope from fuel matrix nanobubbles
    return resolution_rate;
}

void System::setTrappingRate(int input_value, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<InputVariable> &scaling_factors)
{
    /**
     * ### setTrappingRate
     * @brief The krypton intra-granular trapping rate is set according to the input_variable iTrappingRate.
     *
     */

    switch (input_value)
    {
    case 0:
    {
        /**
         * @brief iTrappingRate = 0 corresponds to a constant intra-granular trapping rate, equal to 9.35e-6 1/s.
         * @see This value is from <a href="../../references/pdf_link/Olander_and_Wongsawaeng_2006.pdf" target="_blank">Olander, Wongsawaeng, JNM, 354 (2006), 94-109</a>.
         *
         */

        reference += "iTrappingRate: constant value from Olander, Wongsawaeng, JNM, 354 (2006), 94-109.\n\t";
        trapping_rate = 9.35e-6;
        trapping_rate *= scaling_factors["Trapping rate"].getValue();

        break;
    }

    case 1:
    {
        /**
         * @brief iTrappingRate = 1 corresponds to the intra-granular trapping rate
         * @see From <a href="../../references/pdf_link/Ham_1958.pdf" target="_blank">F.S. Ham, Journal of Physics and Chemistry of Solids, 6 (1958) 335-351</a>.
         *
         * This formula is based on the assumptions that the trapping centre density is dilute enough.
         * g = 4 pi D_s R_b N_b
         *
         */

        reference += "iTrappingRate: F.S. Ham, Journal of Physics and Chemistry of Solids, 6 (1958) 335-351.\n\t";

        if (sciantix_variable["Intragranular bubble concentration"].getFinalValue() == 0.0)
            trapping_rate = 0.0;

        else
            trapping_rate = 4.0 * M_PI * diffusivity *
                            (sciantix_variable["Intragranular bubble radius"].getFinalValue() + radius_in_lattice) *
                            sciantix_variable["Intragranular bubble concentration"].getFinalValue();

        trapping_rate *= scaling_factors["Trapping rate"].getValue();

        break;
    }

    case 99:
    {
        /**
         * @brief iTrappingRate = 99 stands for the case with zero trapping rate.
         *
         */
        reference += "iTrappingRate: Null trapping rate.\n\t";

        trapping_rate = 0.0;
        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "setTrappingRate", input_value);
        break;
    }
}

double System::getTrappingRate()
{
    /// Member function to get the value of the trapping rate of the isotope in the fuel matrix nanobubbles
    return trapping_rate;
}

void System::setNucleationRate(int input_value, SciantixArray<SciantixVariable> &history_variable, 
    SciantixArray<InputVariable> &scaling_factors)
{
    /**
     * ### setNucleationRate
     * @brief Evaluation of the nucleation rate of intragranular gas bubble inside the UO<sub>2</sub> matrix
     *
     */
    /// nucleation_rate
    switch (input_value)
    {
    case 0:
    {
        /**
         * @brief iNucleationRate = 0 correspond to the case with constant nucleation rate.
         *
         */

        reference += "iNucleationRate: constant value.\n\t";
        nucleation_rate = 4e20;
        nucleation_rate *= scaling_factors["Nucleation rate"].getValue();

        break;
    }

    case 1:
    {
        /**
         * @brief iNucleationRate = 1 correspond to expression for intragranular bubble nucleation rate from
         * @see <a href="../../references/pdf_link/Olander_and_Wongsawaeng_2006.pdf" target="_blank">Olander, Wongsawaeng, JNM, 354 (2006), 94-109</a>.
         *
         */

        reference += "iNucleationRate: Olander, Wongsawaeng, JNM, 354 (2006), 94-109.\n\t";
        nucleation_rate = 2.0 * history_variable["Fission rate"].getFinalValue() * 25;
        nucleation_rate *= scaling_factors["Nucleation rate"].getValue();

        break;
    }

    case 99:
    {
        /**
         * @brief iNucleationRate = 99 correspond to case with zero nucleation rate.
         */

        reference += "iNucleationRate: Null nucleation rate.\n\t";
        nucleation_rate = 0.0;

        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "setNucleationRate", input_value);
        break;
    }
}

double System::getNucleationRate()
{
    return nucleation_rate;
}

void System::setPoreNucleationRate(double t)
{
    pore_nucleation_rate = t;
}

double System::getPoreNucleationRate()
{
    return pore_nucleation_rate;
}

void System::setProductionRate(int input_value, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable,
    SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<InputVariable> &scaling_factors)
{
    /**
     * ### setProductionRate
     *
     */
    switch (input_value)
    {
    case 0:
    {
        reference += "No production rate.\n\t";
        production_rate = 0.0;
        break;
    }
    case 1:
    {
        /**
         * @brief Production rate = cumulative yield * fission rate density
         *
         */

        double alpha = sciantix_variable["Restructured volume fraction"].getFinalValue();

        double sf(1.00);
        if (input_variable["iFuelMatrix"].getValue() == 1)
        {
            sf = 1.25;
        }

        reference += "Production rate = cumulative yield * fission rate density * (1 - alpha).\n\t";
        production_rate = sf * (1.0 - alpha) * yield * history_variable["Fission rate"].getFinalValue(); // (at/m3s)
        break;
    }

    case 2:
    {
        /**
         * @brief Surrogate model derived from **helium production in fast reactor conditions**.
         * The helium production rate is fitted with a function linearly dependent on the local burnup.
         * @see The default fit is from <a href="../../references/pdf_link/Cechet_et_al_2021.pdf" target="_blank">A. Cechet et al., Nuclear Engineering and Technology, 53 (2021) 1893-1908</a>.
         *
         * **Default range of utilization of the default fit**
         * - Fast reactor conditions: (U,Pu)O<sub>2</sub> MOX fuel in SFR conditions
         * - Up to 200 GWd/tHM
         * - Pu/HM concentration of 20-40%
         *
         * The default fit (hence the helium production rate) can be calibrated by using the dedicated
         * scaling factor (to be set in input_scaling_factors.txt).
         *
         */

        reference += "Case for helium production rate: Cechet et al., Nuclear Engineering and Technology, 53 (2021) 1893-1908.\n\t";

        // specific power = dburnup / dt
        sciantix_variable["Specific power"].setFinalValue(
            history_variable["Fission rate"].getFinalValue() * 3.12e-17 / sciantix_variable["Fuel density"].getFinalValue()
        );

        // production rate in dproduced / dburnup -> dproduced / dtime
        production_rate = 2.0e+21 * sciantix_variable["Burnup"].getFinalValue() + 3.0e+23; // (at/m3 burnup)
        production_rate *= sciantix_variable["Specific power"].getFinalValue() / 86400;    // (at/m3s)

        production_rate *= scaling_factors["Helium production rate"].getValue();

        break;
    }

    case 3:
    {
        /**
         * @brief Constant production rate
         *
         */

        reference += "Constant production rate.\n\t";
        production_rate = 1e18;

        break;
    }

    case 5:
    {
        /**
         * @brief Production rate = cumulative yield * fission rate density * HBS volume fraction
         *
         */

        double alpha = sciantix_variable["Restructured volume fraction"].getFinalValue();

        reference += "Production rate = cumulative yield * fission rate density * alpha.\n\t";
        production_rate = 1.25 * yield * history_variable["Fission rate"].getFinalValue() * alpha; // (at/m3s)
        break;
    }

    default:
        ErrorMessages::Switch(__FILE__, "setProductionRate", input_value);
        break;
    }
}

double System::getProductionRate()
{
    return production_rate;
}
