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

#include "Simulation.h"
#include "StoichiometryDeviation.h"

void Simulation::StoichiometryDeviation()
{
    if (!input_variable["iStoichiometryDeviation"].getValue())
        return;

    // Model declaration
    Model model_;

    std::string         reference;
    std::vector<double> parameter;

    double surface_to_volume = 3 / sciantix_variable["Grain radius"].getInitialValue();  // (1/m)

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
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described
         * with a semi-empirical model.
         * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
         * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
         *
         * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by
         * experimments. It is expressed as alpha = A exp(−Q/T). S/V = 3/a (1/m), surface-to-volume
         * ratio of the fuel sample x_eq : equilbrium stoichiometry deviation (evaluated by
         * UO2Thermochemistry model)
         *
         * ### iStoichiometryDeviation = 1
         *
         * Range of utilization:
         * - Pure steam
         * - Temperature range: 1273-1923
         * - x = 0.0 - 0.25
         *
         * @ref Cox et al. NUREG/CP-0078 (1986), U.S. NRC
         *
         */
        case 1:
        {
            reference += " : Carter and Lay, J. Nucl. Mater., 36:77-86, Cox et al. NUREG/CP-0078 "
                         "(1986), U.S. NRC.";

            double surface_exchange_coefficient = 0.365 * exp(-23500 / history_variable["Temperature"].getFinalValue());
            double decay_rate                   = surface_exchange_coefficient *
                                sqrt(history_variable["Steam pressure"].getFinalValue()) * surface_to_volume;
            double source_rate =
                surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue()) *
                sciantix_variable["Equilibrium stoichiometry deviation"].getFinalValue() * surface_to_volume;

            parameter.push_back(decay_rate);
            parameter.push_back(source_rate);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described
         * with a semi-empirical model.
         * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
         * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
         *
         * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by
         * experimments. It is expressed as alpha = A exp(−Q/T). S/V = 3/a (1/m), surface-to-volume
         * ratio of the fuel sample x_eq : equilbrium stoichiometry deviation (evaluated by
         * UO2Thermochemistry model)
         *
         * ### iStoichiometryDeviation = 2
         *
         * Range of utilization:
         * - Pure steam
         * - Temperature range: 1158-2108
         * - x = 0.0 - 0.25
         *
         * @ref Bittel et al., J. Amer. Ceram. Soc., 52:446–451, 1969, reanalysed by Cox et al.
         * NUREG/CP-0078 (1986), U.S. NRC
         *
         */
        case 2:
        {
            reference += " : Carter and Lay, J. Nucl. Mater., 36:77-86, 1970. Bittel et al., J. Amer. "
                         "Ceram. Soc., 52:446-451, 1969.";

            double surface_exchange_coefficient = 0.194 * exp(-19900 / history_variable["Temperature"].getFinalValue());
            double decay_rate                   = surface_exchange_coefficient *
                                sqrt(history_variable["Steam pressure"].getFinalValue()) * (surface_to_volume);
            double source_rate =
                surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue()) *
                sciantix_variable["Equilibrium stoichiometry deviation"].getFinalValue() * (surface_to_volume);

            parameter.push_back(decay_rate);
            parameter.push_back(source_rate);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described
         * with a semi-empirical model.
         * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
         * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
         *
         * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by
         * experimments. It is expressed as alpha = A exp(−Q/T). S/V = 3/a (1/m), surface-to-volume
         * ratio of the fuel sample x_eq : equilbrium stoichiometry deviation (evaluated by
         * UO2Thermochemistry model)
         *
         * ### iStoichiometryDeviation = 3
         *
         * Range of utilization:
         * - Pure steam
         * - Polycrystal sample
         * - Temperature range: 1273-1673
         * - x = 0.0 - 0.175
         *
         * @ref Abrefah et al., JNM., 208:98–110, 1994.
         *
         */
        case 3:
        {
            reference += " : Carter and Lay, J. Nucl. Mater., 36:77-86, 1970. Abrefah, JNM., "
                         "208:98-110, 1994.";

            double surface_exchange_coefficient = 0.382 * exp(-22080 / history_variable["Temperature"].getFinalValue());
            double decay_rate                   = surface_exchange_coefficient *
                                sqrt(history_variable["Steam pressure"].getFinalValue()) * (surface_to_volume);
            double source_rate =
                surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue()) *
                sciantix_variable["Equilibrium stoichiometry deviation"].getFinalValue() * (surface_to_volume);

            parameter.push_back(decay_rate);
            parameter.push_back(source_rate);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described
         * with a semi-empirical model.
         * @ref Carter and Lay, J. Nucl. Mater., 36:77–86, 1970.
         * The oxidation rate follows: dx/dt = alpha*(S/V)*[x_eq - x]*(Ph2o^0.5)
         *
         * alpha is the surface exchange coefficient for unirradiated UO2 oxidation obtained by
         * experiments. It is expressed as alpha = A exp(−Q/T). S/V = 3/a (1/m), surface-to-volume
         * ratio of the fuel sample x_eq : equilbrium stoichiometry deviation (evaluated by
         * UO2Thermochemistry model)
         *
         * ### iStoichiometryDeviation = 4
         *
         * Range of utilization:
         * - Pure steam
         * - Temperature range: 1073-1473
         * - x = 0.0 - 0.08
         * @ref Imamura and. Une, JNM, 247:131–137, 1997.
         *
         */
        case 4:
        {
            reference += " : Carter and Lay, J. Nucl. Mater., 36:77-86, 1970. Imamura and. Une, JNM, "
                         "247:131-137, 1997.";

            double surface_exchange_coefficient =
                0.000341 * exp(-15876 / history_variable["Temperature"].getFinalValue());
            double decay_rate = surface_exchange_coefficient *
                                sqrt(history_variable["Steam pressure"].getFinalValue() / 0.12) * surface_to_volume;
            double source_rate =
                surface_exchange_coefficient * sqrt(history_variable["Steam pressure"].getFinalValue() / 0.12) *
                sciantix_variable["Equilibrium stoichiometry deviation"].getFinalValue() * surface_to_volume;

            parameter.push_back(decay_rate);
            parameter.push_back(source_rate);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described
         * with a mechanistic Langmuir-based approach
         * @ref <a
         * href="https://www.stralsakerhetsmyndigheten.se/en/publications/reports/safety-at-nuclear-power-plants/2018/201825/"
         * target="_blank">Massih A.R. (2018). UO2 Fuel Oxidation and Fission Gas Release, Swedish
         * Radiation Safety Authority (SSM).</a> The oxidation rate follows: dx/dt = theta/tau (1 -
         * sqrt(Po2(x)/Po2)) In SCIANTIX, the ODE is rewritten as: dx / dt = K (1 - beta * exp(alpha
         * * x))
         *
         * ### iStoichiometryDeviation = 5
         *
         * Range of utilization:
         * - Pure steam
         * - Temperature range: 1073-1673 K
         * - x = 0.0 - 0.25
         */
        case 5:
        {
            reference += " : Massih, A. R. UO2 fuel oxidation and fission gas release. Swedish "
                         "Radiation Safety Authority report, Report 2018 (2018): 25";

            double k_star = 1e4 * exp(-21253.0 / history_variable["Temperature"].getFinalValue() - 2.43);  // (mol/m2 s)
            double tau_inv = k_star * (surface_to_volume) / 8.0e4;
            double s       = 0.023;
            double ka      = 1.0e13 * exp(-21557.0 / history_variable["Temperature"].getFinalValue());
            double B       = s / sqrt(2 * M_PI * 8.314 * history_variable["Temperature"].getFinalValue() * 0.018);
            double A       = 1.0135e5 * B / (1.66e-6 * ka);  // (1/atm)
            double theta   = A * history_variable["Steam pressure"].getFinalValue() * 1.013e5 /
                           (1 + A * history_variable["Steam pressure"].getFinalValue() * 1.013e5);
            double gamma = sqrt(exp(-32700.0 / history_variable["Temperature"].getFinalValue() + 9.92) * 1.013e5);
            double rad_c = sqrt(0.0004);
            double beta;

            if (sciantix_variable["Gap oxygen partial pressure"].getFinalValue() > 0.0)
                beta = rad_c * gamma / sqrt(sciantix_variable["Gap oxygen partial pressure"].getFinalValue() * 1.013e5);
            else
                beta = 0.0;

            double alpha = 57 / 2;
            double K     = tau_inv * theta;

            parameter.push_back(K);
            parameter.push_back(beta);
            parameter.push_back(alpha);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation and stoichimetry deviation evolution is described
         * with a mechanistic Langmuir-based approach
         * @ref <a
         * href="https://www.stralsakerhetsmyndigheten.se/en/publications/reports/safety-at-nuclear-power-plants/2018/201825/"
         * target="_blank">Massih A.R. (2018). UO2 Fuel Oxidation and Fission Gas Release, Swedish
         * Radiation Safety Authority (SSM).</a> The oxidation rate follows: dx/dt = theta/tau (1 -
         * sqrt(Po2(x)/Po2)) In SCIANTIX, the ODE is rewritten as: dx / dt = K (1 - beta * exp(alpha
         * * x))
         *
         * ### iStoichiometryDeviation = 6
         *
         * Range of utilization:
         * - Pure steam
         * - Temperature range: 1073-1673 K
         * - x = 0.0 - 0.25
         *
         * In this case, the surface-to-volume ratio of the sample is fixed.
         * This is the case for the simulation of oxidised rodlet.
         *
         */
        case 6:
        {
            reference += " : Massih, A. R. UO2 fuel oxidation and fission gas release. Swedish "
                         "Radiation Safety Authority report, Report 2018 (2018): 25";

            surface_to_volume = 225;

            double k_star = 1e4 * exp(-21253.0 / history_variable["Temperature"].getFinalValue() - 2.43);  // (mol/m2 s)
            double tau_inv = k_star * (surface_to_volume) / 8.0e4;
            double s       = 0.023;
            double ka      = 1.0e13 * exp(-21557.0 / history_variable["Temperature"].getFinalValue());
            double B       = s / sqrt(2 * M_PI * 8.314 * history_variable["Temperature"].getFinalValue() * 0.018);
            double A       = 1.0135e5 * B / (1.66e-6 * ka);  // (1/atm)
            double theta   = A * history_variable["Steam pressure"].getFinalValue() * 1.013e5 /
                           (1 + A * history_variable["Steam pressure"].getFinalValue() * 1.013e5);
            double gamma = sqrt(exp(-32700.0 / history_variable["Temperature"].getFinalValue() + 9.92) * 1.013e5);
            double rad_c = sqrt(0.0004);
            double beta;

            if (sciantix_variable["Gap oxygen partial pressure"].getFinalValue() > 0)
                beta = rad_c * gamma / sqrt(sciantix_variable["Gap oxygen partial pressure"].getFinalValue() * 1.013e5);
            else
                beta = 0.0;

            double alpha = 57 / 2;
            double K     = tau_inv * theta;

            parameter.push_back(K);
            parameter.push_back(beta);
            parameter.push_back(alpha);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        /**
         * @brief The model for fuel oxidation as the net effect of burnup.
         * @ref https://doi.org/10.1016/0022-3115(79)90154-5 
         * ### iStoichiometryDeviation = 7
         * 
         * The Kleykamp's relation holds provided there is no internal oxidation of Zr-alloy cladding, its validity range is limited to burnup < 5 FIMA.
         * Nevertheless the work of Spino and Peerani indicates that the relation holds up to 10 FIMA.
         * 
         * J. Spino and P. Peerani. Oxygen stoichiometry shift of irradiated LWR-fuels at high burnups: 
         * Review of data and alternative interpretation of recently published results. J. Nucl. Mater., 375:8–25, 2008.
         *
         */
        case 7:
        {
            reference += " : H. Kleykamp, The chemical state of LWR high-power rods under irradiation, Journal of Nuclear Materials (1979)";
            double burnup = sciantix_variable["FIMA"].getIncrement();
            if (burnup > 10)
            {
                std::cout << "WARNING: The model is valid for burnup < 10 FIMA." << std::endl; 
                std::cout << "Burnup (at.%) = " << burnup << std::endl;
            }
            double coefficient = 0.0013;

            parameter.push_back(burnup);
            parameter.push_back(coefficient);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        case 8:
        {
            reference += " : Under development, E.Cappellari: OC - po2 verification";

            // Target trend: x = x0 + 0.001 * t[h], from -0.1 at 0 h to +0.2 at 300 h.
            // The generic integrator uses a rate times an increment, and the time step here is in seconds.
            parameter.push_back(physics_variable["Time step"].getFinalValue());
            parameter.push_back(0.001 / 3600.0);

            // linear increase with time to verify the correct po2 at different O/M
            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }
        case 9:
        {
            reference += " : prescribed O/M history from input_history.txt.";

            // Prescribed O/M is converted to stoichiometry deviation as x = O/M - 2.
            parameter.push_back(history_variable["O/M ratio"].getFinalValue() - 2.0);
            parameter.push_back(history_variable["O/M ratio"].getInitialValue() - 2.0);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }
        case 10:
        {
            reference += " : under development: fission + radial transport from Aitken.";

            // Interaction term due to fission
            double fissionrate_mol = history_variable["Fission rate"].getFinalValue()/avogadro_number;
            parameter.push_back(fissionrate_mol);

            // // Due to radial transport (Atkin model, for hypostoichiometric fuels)
            // double Q = - 125e3;
            // if (sciantix_variable["Stoichiometry deviation"].getInitialValue() > 0)
            //     Q = 0;
            
            // double radialtransfer = exp(
            //     Q/gas_constant*
            //     (
            //         1.0 / history_variable["Temperature"].getFinalValue() -
            //         1.0 / history_variable["Temperature"].getInitialValue()
            //     )
            // );

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        } 


        default:
            ErrorMessages::Switch(
                __FILE__, "iStoichiometryDeviation", int(input_variable["iStoichiometryDeviation"].getValue()));
            break;
    }

    model.push(model_);

    // Model resolution
    if (!input_variable.isElementPresent("iStoichiometryDeviation"))
        return;

    if (history_variable["Temperature"].getFinalValue() < 1000.0 && input_variable["iStoichiometryDeviation"].getValue() < 7)
    {
        sciantix_variable["Stoichiometry deviation"].setConstant();
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(0.0);
    }
    else if (input_variable["iStoichiometryDeviation"].getValue() < 5)
    {
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            solver.Decay(sciantix_variable["Stoichiometry deviation"].getInitialValue(),
                         model["Stoichiometry deviation"].getParameter().at(0),
                         model["Stoichiometry deviation"].getParameter().at(1),
                         physics_variable["Time step"].getFinalValue()));
    }
    else if (input_variable["iStoichiometryDeviation"].getValue() > 4 && input_variable["iStoichiometryDeviation"].getValue() < 7)
    {
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            solver.NewtonLangmuirBasedModel(
                sciantix_variable["Stoichiometry deviation"].getInitialValue(),
                model["Stoichiometry deviation"].getParameter(),
                physics_variable["Time step"].getFinalValue()
            )
        );
    }
    else if (input_variable["iStoichiometryDeviation"].getValue() > 6 && input_variable["iStoichiometryDeviation"].getValue() < 9)
    {
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            solver.Integrator(
                sciantix_variable["Stoichiometry deviation"].getInitialValue(),
                model["Stoichiometry deviation"].getParameter().at(1),
                model["Stoichiometry deviation"].getParameter().at(0)   
            )
        );

        // reduced Uranium content
        sciantix_variable["Uranium content"].addValue( 
            - sciantix_variable["Oxygen content"].getFinalValue()
            * sciantix_variable["Stoichiometry deviation"].getIncrement()
            * pow(2 + sciantix_variable["Stoichiometry deviation"].getFinalValue(), -2)
        );
    }
    else if (input_variable["iStoichiometryDeviation"].getValue() == 9)
    {
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            model["Stoichiometry deviation"].getParameter().at(0)
        );

        sciantix_variable["Uranium content"].addValue(
            - sciantix_variable["Oxygen content"].getFinalValue()
            * sciantix_variable["Stoichiometry deviation"].getIncrement()
            * pow(2 + sciantix_variable["Stoichiometry deviation"].getFinalValue(), -2)
        );
    }
    else if (input_variable["iStoichiometryDeviation"].getValue() == 10)
    {
        sciantix_variable["Uranium content"].setFinalValue(
            solver.Integrator(
                sciantix_variable["Uranium content"].getFinalValue(),
                - model["Stoichiometry deviation"].getParameter().at(0),
                physics_variable["Time step"].getFinalValue()
            )
        );
            
        sciantix_variable["Stoichiometry deviation"].setFinalValue( 
            solver.BinaryInteraction(
                sciantix_variable["Stoichiometry deviation"].getFinalValue() + 2.0,
                - model["Stoichiometry deviation"].getParameter().at(0)/
                sciantix_variable["Oxygen content"].getFinalValue(),
                physics_variable["Time step"].getFinalValue()
            ) 
            - 2.0
        );
    }

    if (sciantix_variable["Stoichiometry deviation"].getFinalValue() == 0) return;

    // EC - this has not the dimensionality of a pressure if not multiplied to the reference one
    // it is not coherent with the oxygen potential also, modified by * reference pressure
    double coeff(1.0);
    if (input_variable["iStoichiometryDeviation"].getValue() > 6)
        coeff = reference_oxygen_pressure_atm;
    
    double q = sciantix_variable["Plutonium content"].getFinalValue(); 

    // MOX (Kato) or UO2 (Blackburn)
    if (q > 0.0)
    {
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(
            coeff *
            KatoThermochemicalModel(
                sciantix_variable["Stoichiometry deviation"].getFinalValue(),
                history_variable["Temperature"].getFinalValue(),
                sciantix_variable
            )
        );

        sciantix_variable["Fuel oxygen partial pressure - Kato"].setFinalValue(
            sciantix_variable["Fuel oxygen partial pressure"].getFinalValue()
        );
    }
    else
    {
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(
            coeff *
            BlackburnThermochemicalModel(
                sciantix_variable["Stoichiometry deviation"].getFinalValue(),
                history_variable["Temperature"].getFinalValue(),
                sciantix_variable
            )
        );

        sciantix_variable["Fuel oxygen partial pressure - Blackburn"].setFinalValue(
            sciantix_variable["Fuel oxygen partial pressure"].getFinalValue()
        );
    }

    // Fuel oxygen potential
    if (sciantix_variable["Fuel oxygen partial pressure"].getFinalValue() == 0.0)
        sciantix_variable["Fuel oxygen potential"].setFinalValue(0.0);
    else
        sciantix_variable["Fuel oxygen potential"].setFinalValue(
            8.314 * 1.0e-3 * history_variable["Temperature"].getFinalValue() *
            log(sciantix_variable["Fuel oxygen partial pressure"].getFinalValue() / reference_oxygen_pressure_atm)
        );

    if (q > 0.0)
    {
        sciantix_variable["Fuel oxygen potential - Kato"].setFinalValue(
            sciantix_variable["Fuel oxygen potential"].getFinalValue()
        );
    }
    else
    {
        sciantix_variable["Fuel oxygen potential - Blackburn"].setFinalValue(
            sciantix_variable["Fuel oxygen potential"].getFinalValue()
        );
    }
}

double BlackburnThermochemicalModel(double                           stoichiometry_deviation,
                                    double                           temperature,
                                    SciantixArray<SciantixVariable>& sciantix_variable)
{
    double ln_p =
        2.0 * log(stoichiometry_deviation * (2.0 + stoichiometry_deviation) / (1.0 - stoichiometry_deviation)) +
        108.0 * pow(sciantix_variable["Stoichiometry deviation"].getFinalValue(), 2.0) - 32700.0 / temperature + 9.92;
    return exp(ln_p);
}

double KatoThermochemicalModel(double stoichiometry_deviation, double temperature, SciantixArray<SciantixVariable> &sciantix_variable)
{
    double q_Pu = sciantix_variable["Plutonium content"].getFinalValue();
    double q_Am = 0.0;

    double target_om = 2.0 + stoichiometry_deviation;

    std::vector<double> parameter;
    parameter.push_back(temperature); // param[0]
    parameter.push_back(q_Pu);        // param[1]
    parameter.push_back(target_om);   // param[2]
    parameter.push_back(q_Am);        // param[3]

    Solver solver;
    return solver.BisectionKato(parameter); 
}
