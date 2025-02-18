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

#include "StoichiometryDeviation.h"
#include "Simulation.h"

void Simulation::StoichiometryDeviation()
{
    if (!input_variable["iStoichiometryDeviation"].getValue()) return;

    // Model declaration
    Model model_;

    std::string reference;
    std::vector<double> parameter;

    double surface_to_volume = 3 / sciantix_variable["Grain radius"].getFinalValue(); // (1/m)

    model_.setName("Stoichiometry deviation");
  
    switch (int(input_variable["iStoichiometryDeviation"].getValue()))
    {
        case 0:
        {
            reference += " : not considered.";

            parameter.push_back(0);
            parameter.push_back(0);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described with a semi-empirical model.
         * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
         * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
         *
         * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by experimments.
         * It is expressed as alpha = A exp(−Q/T).
         * S/V = 3/a (1/m), surface-to-volume ratio of the fuel sample
         * x_eq : equilbrium stoichiometry deviation (evaluated by UO2Thermochemistry model)
         *
         * ### iStoichiometryDeviation = 1
         *
         * Range of utilization:
         * - Pure steam
         * - Temperature range: 1273-1923
         * @ref Cox et al. NUREG/CP-0078 (1986), U.S. NRC
         *
         */
        case 1:
        {

            reference += " : Carter and Lay, J. Nucl. Mater., 36:77-86, Cox et al. NUREG/CP-0078 (1986), U.S. NRC.";

            double surface_exchange_coefficient = 0.365 * exp(-23500 / history_variable["Temperature"].getFinalValue());
            double decay_rate = surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue()) * surface_to_volume;
            double source_rate = surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue()) * sciantix_variable["Equilibrium stoichiometry deviation"].getFinalValue() * surface_to_volume;

            parameter.push_back(decay_rate);
            parameter.push_back(source_rate);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described with a semi-empirical model.
         * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
         * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
         *
         * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by experimments.
         * It is expressed as alpha = A exp(−Q/T).
         * S/V = 3/a (1/m), surface-to-volume ratio of the fuel sample
         * x_eq : equilbrium stoichiometry deviation (evaluated by UO2Thermochemistry model)
         *
         * ### iStoichiometryDeviation = 2
         *
         * Range of utilization:
         * - Pure steam
         * - Temperature range: 1158-2108
         * @ref Bittel et al., J. Amer. Ceram. Soc., 52:446–451, 1969, reanalysed by Cox et al. NUREG/CP-0078 (1986), U.S. NRC
         *
         */
        case 2:
        {

            reference += " : Carter and Lay, J. Nucl. Mater., 36:77-86, 1970. Bittel et al., J. Amer. Ceram. Soc., 52:446-451, 1969.";

            double surface_exchange_coefficient = 0.194 * exp(-19900 / history_variable["Temperature"].getFinalValue());
            double decay_rate = surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue()) * (surface_to_volume);
            double source_rate = surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue()) * sciantix_variable["Equilibrium stoichiometry deviation"].getFinalValue() * (surface_to_volume);

            parameter.push_back(decay_rate);
            parameter.push_back(source_rate);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described with a semi-empirical model.
         * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
         * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
         *
         * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by experimments.
         * It is expressed as alpha = A exp(−Q/T).
         * S/V = 3/a (1/m), surface-to-volume ratio of the fuel sample
         * x_eq : equilbrium stoichiometry deviation (evaluated by UO2Thermochemistry model)
         *
         * ### iStoichiometryDeviation = 3
         *
         * Range of utilization:
         * - Pure steam
         * - Polycrystal sample
         * - Temperature range: 1273-1673
         * @ref Abrefah et al., JNM., 208:98–110, 1994.
         *
         */
        case 3:
        {
            reference += " : Carter and Lay, J. Nucl. Mater., 36:77-86, 1970. Abrefah, JNM., 208:98-110, 1994.";

            double surface_exchange_coefficient = 0.382 * exp(-22080 / history_variable["Temperature"].getFinalValue());
            double decay_rate = surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue()) * (surface_to_volume);
            double source_rate = surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue()) * sciantix_variable["Equilibrium stoichiometry deviation"].getFinalValue() * (surface_to_volume);

            parameter.push_back(decay_rate);
            parameter.push_back(source_rate);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described with a semi-empirical model.
         * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
         * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
         *
         * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by experiments.
         * It is expressed as alpha = A exp(−Q/T).
         * S/V = 3/a (1/m), surface-to-volume ratio of the fuel sample
         * x_eq : equilbrium stoichiometry deviation (evaluated by UO2Thermochemistry model)
         *
         * ### iStoichiometryDeviation = 4
         *
         * Range of utilization:
         * - Pure steam
         * - Temperature range: 1073-1473
         * @ref Imamura and. Une, JNM, 247:131–137, 1997.
         *
         */
        case 4:
        {
            reference += " : Carter and Lay, J. Nucl. Mater., 36:77-86, 1970. Imamura and. Une, JNM, 247:131-137, 1997.";

            double surface_exchange_coefficient = 0.000341 * exp(-15876 / history_variable["Temperature"].getFinalValue());
            double decay_rate = surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue() / 0.12) * surface_to_volume;
            double source_rate = surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue() / 0.12) * sciantix_variable["Equilibrium stoichiometry deviation"].getFinalValue() * surface_to_volume;

            parameter.push_back(decay_rate);
            parameter.push_back(source_rate);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described with a mechanistic Langmuir-based approach
         * @ref <a href="https://www.stralsakerhetsmyndigheten.se/en/publications/reports/safety-at-nuclear-power-plants/2018/201825/" target="_blank">Massih A.R. (2018). UO2 Fuel Oxidation and Fission Gas Release, Swedish Radiation Safety Authority (SSM).</a>
         * The oxidation rate follows: dx/dt = theta/tau (1 - sqrt(Po2(x)/Po2))
         * In SCIANTIX, the ODE is rewritten as: dx / dt = K (1 - beta * exp(alpha * x))
         *
         * ### iStoichiometryDeviation = 5
         *
         * Range of utilization:
         * - Pure steam
         * - Temperature range: 1073-1673 K
         *
         */
        case 5:
        {
            reference += " : Massih, A. R. UO2 fuel oxidation and fission gas release. Swedish Radiation Safety Authority report, Report 2018 (2018): 25";

            double k_star = 1e4 * exp(-21253.0 / history_variable["Temperature"].getFinalValue() - 2.43); // (mol/m2 s)
            double tau_inv = k_star * (surface_to_volume) / 8.0e4;
            double s = 0.023;
            double ka = 1.0e13 * exp(-21557.0 / history_variable["Temperature"].getFinalValue());
            double B = s / sqrt(2 * M_PI * 8.314 * history_variable["Temperature"].getFinalValue() * 0.018);
            double A = 1.0135e5 * B / (1.66e-6 * ka); // (1/atm)
            double theta = A * history_variable["Steam pressure"].getFinalValue() * 1.013e5 / (1 + A * history_variable["Steam pressure"].getFinalValue() * 1.013e5);
            double gamma = sqrt(exp(-32700.0 / history_variable["Temperature"].getFinalValue() + 9.92) * 1.013e5);
            double rad_c = sqrt(0.0004);
            double beta;

            if (sciantix_variable["Gap oxygen partial pressure"].getFinalValue() > 0.0)
                beta = rad_c * gamma / sqrt(sciantix_variable["Gap oxygen partial pressure"].getFinalValue() * 1.013e5);
            else
                beta = 0.0;

            double alpha = 57 / 2;
            double K = tau_inv * theta;

            parameter.push_back(K);
            parameter.push_back(beta);
            parameter.push_back(alpha);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described with a mechanistic Langmuir-based approach
         * @ref <a href="https://www.stralsakerhetsmyndigheten.se/en/publications/reports/safety-at-nuclear-power-plants/2018/201825/" target="_blank">Massih A.R. (2018). UO2 Fuel Oxidation and Fission Gas Release, Swedish Radiation Safety Authority (SSM).</a>
         * The oxidation rate follows: dx/dt = theta/tau (1 - sqrt(Po2(x)/Po2))
         * In SCIANTIX, the ODE is rewritten as: dx / dt = K (1 - beta * exp(alpha * x))
         *
         * ### iStoichiometryDeviation = 6
         *
         * Range of utilization:
         * - Pure steam
         * - Temperature range: 1073-1673 K
         *
         * In this case, the surface-to-volume ratio of the sample is fixed.
         * This is the case for the simulation of oxidised rodlet.
         *
         */
        case 6:
        {
            reference += " : Massih, A. R. UO2 fuel oxidation and fission gas release. Swedish Radiation Safety Authority report, Report 2018 (2018): 25";

            surface_to_volume = 225;

            double k_star = 1e4 * exp(-21253.0 / history_variable["Temperature"].getFinalValue() - 2.43); // (mol/m2 s)
            double tau_inv = k_star * (surface_to_volume) / 8.0e4;
            double s = 0.023;
            double ka = 1.0e13 * exp(-21557.0 / history_variable["Temperature"].getFinalValue());
            double B = s / sqrt(2 * M_PI * 8.314 * history_variable["Temperature"].getFinalValue() * 0.018);
            double A = 1.0135e5 * B / (1.66e-6 * ka); // (1/atm)
            double theta = A * history_variable["Steam pressure"].getFinalValue() * 1.013e5 / (1 + A * history_variable["Steam pressure"].getFinalValue() * 1.013e5);
            double gamma = sqrt(exp(-32700.0 / history_variable["Temperature"].getFinalValue() + 9.92) * 1.013e5);
            double rad_c = sqrt(0.0004);
            double beta;

            if (sciantix_variable["Gap oxygen partial pressure"].getFinalValue() > 0)
                beta = rad_c * gamma / sqrt(sciantix_variable["Gap oxygen partial pressure"].getFinalValue() * 1.013e5);
            else
                beta = 0.0;

            double alpha = 57 / 2;
            double K = tau_inv * theta;

            parameter.push_back(K);
            parameter.push_back(beta);
            parameter.push_back(alpha);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iStoichiometryDeviation", int(input_variable["iStoichiometryDeviation"].getValue()));
            break;
    }

    model.push(model_);

    // Model resolution
    if (!input_variable.isElementPresent("iStoichiometryDeviation")) return;

    if (history_variable["Temperature"].getFinalValue() < 1000.0)
    {
        sciantix_variable["Stoichiometry deviation"].setConstant();
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(0.0);
    }

    else if (input_variable["iStoichiometryDeviation"].getValue() < 5)
    {
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            solver.Decay(
                sciantix_variable["Stoichiometry deviation"].getInitialValue(),
                model["Stoichiometry deviation"].getParameter().at(0),
                model["Stoichiometry deviation"].getParameter().at(1),
                physics_variable["Time step"].getFinalValue()
            )
        );
    }

    else if (input_variable["iStoichiometryDeviation"].getValue() > 4)
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            solver.NewtonLangmuirBasedModel(
                sciantix_variable["Stoichiometry deviation"].getInitialValue(),
                model["Stoichiometry deviation"].getParameter(),
                physics_variable["Time step"].getFinalValue()
            )
        );

    sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(
        BlackburnThermochemicalModel(
            sciantix_variable["Stoichiometry deviation"].getFinalValue(),
            history_variable["Temperature"].getFinalValue(),
            sciantix_variable
        )
    );

    // Fuel oxygen potential
    if(sciantix_variable["Fuel oxygen partial pressure"].getFinalValue() == 0.0)
        sciantix_variable["Fuel oxygen potential"].setFinalValue(0.0);
    else
        sciantix_variable["Fuel oxygen potential"].setFinalValue(8.314*1.0e-3*history_variable["Temperature"].getFinalValue()*log(sciantix_variable["Fuel oxygen partial pressure"].getFinalValue()/0.1013));
}

double BlackburnThermochemicalModel(double stoichiometry_deviation, double temperature, SciantixArray<SciantixVariable> &sciantix_variable)
{
    double ln_p = 2.0 * log(stoichiometry_deviation * (2.0 + stoichiometry_deviation) / (1.0 - stoichiometry_deviation)) + 108.0 * pow(sciantix_variable["Stoichiometry deviation"].getFinalValue(), 2.0) - 32700.0 / temperature + 9.92;
    return exp(ln_p);
}