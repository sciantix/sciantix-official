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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
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

    double surface_to_volume = 3.0 / sciantix_variable["Grain radius"].getInitialValue();  // (1/m)

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
            double gamma =
                sqrt(exp(-blackburn_enthalpy / history_variable["Temperature"].getFinalValue() + blackburn_entropy) *
                     1.013e5);
            double rad_c = sqrt(0.0004);
            double beta;

            if (sciantix_variable["Gap oxygen partial pressure"].getFinalValue() > 0.0)
                beta = rad_c * gamma / sqrt(sciantix_variable["Gap oxygen partial pressure"].getFinalValue() * 1.013e5);
            else
                beta = 0.0;

            double alpha = 57.0 / 2.0;
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
            double gamma =
                sqrt(exp(-blackburn_enthalpy / history_variable["Temperature"].getFinalValue() + blackburn_entropy) *
                     1.013e5);
            double rad_c = sqrt(0.0004);
            double beta;

            if (sciantix_variable["Gap oxygen partial pressure"].getFinalValue() > 0)
                beta = rad_c * gamma / sqrt(sciantix_variable["Gap oxygen partial pressure"].getFinalValue() * 1.013e5);
            else
                beta = 0.0;

            double alpha = 57.0 / 2.0;
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
         * The Kleykamp's relation holds provided there is no internal oxidation of Zr-alloy cladding, its validity
         * range is limited to burnup < 5 FIMA. Nevertheless the work of Spino and Peerani indicates that the relation
         * holds up to 10 FIMA.
         *
         * J. Spino and P. Peerani. Oxygen stoichiometry shift of irradiated LWR-fuels at high burnups:
         * Review of data and alternative interpretation of recently published results. J. Nucl. Mater., 375:8–25, 2008.
         *
         */
        case 7:
        {
            reference += " : H. Kleykamp, The chemical state of LWR high-power rods under irradiation, Journal of "
                         "Nuclear Materials (1979)";
            double burnup = sciantix_variable["FIMA"].getIncrement();
            if (burnup > 10)
            {
                std::cout << "WARNING: The model is valid for burnup < 10 FIMA." << std::endl;
                std::cout << "Burnup (at.%) = " << burnup << std::endl;
            }
            double coefficient = 0.0013;

            parameter.push_back(burnup);
            parameter.push_back(coefficient);

            // MOX : from Samuelsson, K., Dumas, J. C., Sundman, B., Lamontagne, J., & Guéneau, C. (2020). Simulation of
            // the chemical state of high burnup (U,Pu)O2 fuel in fast reactors based on thermodynamic calculations.
            // Journal of Nuclear Materials, 532(1), 151969. https://doi.org/10.1016/j.jnucmat.2019.151969)
            if (sciantix_variable["q"].getFinalValue() > 0.0)
                parameter.push_back(0.71);
            else
                parameter.push_back(1.0);

            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }

        case 8:
        {
            reference += " : E.Cappellari, OC - po2 verification";

            // Unphysical shift, adopted only for verification purposes.
            // x = x0 + 0.001 * t[h]
            // The generic integrator uses a rate times an increment, and the time step here is in seconds.
            parameter.push_back(physics_variable["Time step"].getFinalValue());
            parameter.push_back(0.001 / 3600.0);
            // MOX : for the purpose of the verification q is kept constant.
            parameter.push_back(1.0 - sciantix_variable["q"].getFinalValue());

            // linear increase with time to verify the correct po2 at different O/M
            model_.setParameter(parameter);
            model_.setRef(reference);

            break;
        }
        case 9:
        {
            reference += " : E.Cappellari, prescribed O/M history.";

            // Prescribed O/M is converted to stoichiometry deviation as x = O/M - 2.

            double       prescribed_om_final   = history_variable["O/M ratio"].getFinalValue();
            double       prescribed_om_initial = history_variable["O/M ratio"].getInitialValue();
            const double q_prescribed          = sciantix_variable["q"].getFinalValue();
            double       om                    = 2 + sciantix_variable["Stoichiometry deviation"].getFinalValue();

            if (om < 2.0)
            {
                if (prescribed_om_final > max_prescribed_om_ratio)
                    prescribed_om_final = max_prescribed_om_ratio;
                if (prescribed_om_initial > max_prescribed_om_ratio)
                    prescribed_om_initial = max_prescribed_om_ratio;
            }

            parameter.push_back(prescribed_om_final - 2.0);
            parameter.push_back(prescribed_om_initial - 2.0);

            // constant q
            parameter.push_back(1.0 - q_prescribed);

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

    if (history_variable["Temperature"].getFinalValue() < 1000.0 &&
        input_variable["iStoichiometryDeviation"].getValue() < 7)
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
    else if (input_variable["iStoichiometryDeviation"].getValue() > 4 &&
             input_variable["iStoichiometryDeviation"].getValue() < 7)
    {
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            solver.NewtonLangmuirBasedModel(sciantix_variable["Stoichiometry deviation"].getInitialValue(),
                                            model["Stoichiometry deviation"].getParameter(),
                                            physics_variable["Time step"].getFinalValue()));
    }
    else if (input_variable["iStoichiometryDeviation"].getValue() > 6 &&
             input_variable["iStoichiometryDeviation"].getValue() < 9)
    {
        // MODELS 7 - 8: stoichiometry deviation is calculated as the integral of a rate variation.
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            solver.Integrator(sciantix_variable["Stoichiometry deviation"].getInitialValue(),
                              model["Stoichiometry deviation"].getParameter().at(1),
                              model["Stoichiometry deviation"].getParameter().at(0)));

        sciantix_variable["U content"].addValue(
            -sciantix_variable["O content"].getFinalValue() * model["Stoichiometry deviation"].getParameter().at(2) *
            sciantix_variable["Stoichiometry deviation"].getIncrement() *
            pow(2 + sciantix_variable["Stoichiometry deviation"].getFinalValue(), -2.0));

        sciantix_variable["Pu content"].addValue(
            -sciantix_variable["O content"].getFinalValue() *
            (1.0 - model["Stoichiometry deviation"].getParameter().at(2)) *
            sciantix_variable["Stoichiometry deviation"].getIncrement() *
            pow(2 + sciantix_variable["Stoichiometry deviation"].getFinalValue(), -2.0));
    }
    else if (input_variable["iStoichiometryDeviation"].getValue() == 9)
    {
        // MODEL 9: stoichiometry deviation is prescribed as a history variable, converted to O/M and then to x.
        sciantix_variable["Stoichiometry deviation"].setFinalValue(
            model["Stoichiometry deviation"].getParameter().at(0));

        sciantix_variable["U content"].addValue(
            -sciantix_variable["O content"].getFinalValue() * model["Stoichiometry deviation"].getParameter().at(2) *
            sciantix_variable["Stoichiometry deviation"].getIncrement() *
            pow(2 + sciantix_variable["Stoichiometry deviation"].getFinalValue(), -2.0));

        sciantix_variable["Pu content"].addValue(
            -sciantix_variable["O content"].getFinalValue() *
            (1.0 - model["Stoichiometry deviation"].getParameter().at(2)) *
            sciantix_variable["Stoichiometry deviation"].getIncrement() *
            pow(2 + sciantix_variable["Stoichiometry deviation"].getFinalValue(), -2.0));
    }

    const double plutonium_content = sciantix_variable["Pu content"].getFinalValue();
    const double uranium_content   = sciantix_variable["U content"].getFinalValue();
    const double total             = plutonium_content + uranium_content;

    if (total > 0.0)
        sciantix_variable["q"].setFinalValue(plutonium_content / total);

    const double x = sciantix_variable["Stoichiometry deviation"].getFinalValue();
    double       q = sciantix_variable["q"].getFinalValue();

    // Keep the historical UO2/Blackburn behaviour at exact stoichiometry to avoid log(0).
    if (x == 0.0 && q <= 0.0)
        return;

    // Fuel oxygen partial pressure correlations
    // MOX (Kato) or UO2 (Blackburn)
    if (q > 0.0)
    {
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(
            reference_oxygen_pressure_atm *
            KatoThermochemicalModel(x, history_variable["Temperature"].getFinalValue(), sciantix_variable));

        sciantix_variable["Fuel oxygen partial pressure - Kato"].setFinalValue(
            sciantix_variable["Fuel oxygen partial pressure"].getFinalValue());
    }
    else
    {
        sciantix_variable["Fuel oxygen partial pressure"].setFinalValue(
            reference_oxygen_pressure_atm *
            BlackburnThermochemicalModel(x, history_variable["Temperature"].getFinalValue(), sciantix_variable));

        sciantix_variable["Fuel oxygen partial pressure - Blackburn"].setFinalValue(
            sciantix_variable["Fuel oxygen partial pressure"].getFinalValue());
    }

    // Fuel oxygen potential
    if (sciantix_variable["Fuel oxygen partial pressure"].getFinalValue() == 0.0)
        sciantix_variable["Fuel oxygen potential"].setFinalValue(0.0);
    else
        sciantix_variable["Fuel oxygen potential"].setFinalValue(
            8.314 * 1.0e-3 * history_variable["Temperature"].getFinalValue() *
            log(sciantix_variable["Fuel oxygen partial pressure"].getFinalValue() / reference_oxygen_pressure_atm));

    if (q > 0.0)
    {
        sciantix_variable["Fuel oxygen potential - Kato"].setFinalValue(
            sciantix_variable["Fuel oxygen potential"].getFinalValue());
    }
    else
    {
        sciantix_variable["Fuel oxygen potential - Blackburn"].setFinalValue(
            sciantix_variable["Fuel oxygen potential"].getFinalValue());
    }
}

double BlackburnThermochemicalModel(double                           stoichiometry_deviation,
                                    double                           temperature,
                                    SciantixArray<SciantixVariable>& sciantix_variable)
{
    // Blackburn describes the hyperstoichiometric branch, UO(2+x), and the logarithm takes a
    // non-positive argument outside 0 < x < 1, returning a NaN that propagates into the
    // oxygen partial pressure and potential.
    //
    // The two ends are not symmetric. As x -> 0+ the pressure tends to zero, so a
    // non-positive deviation - which occurs as numerical noise about a vanishing one - is
    // mapped onto that limit. As x -> 1- the pressure diverges instead, so returning zero
    // there would invert the physics; that range is outside the correlation altogether
    // (x = 1 is UO3, far above the x < 0.25 the oxidation models cover) and is reported.
    if (stoichiometry_deviation <= 0.0)
        return 0.0;

    if (stoichiometry_deviation >= 1.0)
    {
        ErrorMessages::Warning("StoichiometryDeviation.C",
                               "stoichiometry deviation x = " + std::to_string(stoichiometry_deviation) +
                                   " is outside the Blackburn correlation, where the oxygen partial pressure "
                                   "diverges; the fuel oxygen partial pressure is left unevaluated");
        return 0.0;
    }

    double ln_p =
        2.0 * log(stoichiometry_deviation * (2.0 + stoichiometry_deviation) / (1.0 - stoichiometry_deviation)) +
        108.0 * pow(sciantix_variable["Stoichiometry deviation"].getFinalValue(), 2.0) -
        blackburn_enthalpy / temperature + blackburn_entropy;
    return exp(ln_p);
}

double KatoThermochemicalModel(double                           stoichiometry_deviation,
                               double                           temperature,
                               SciantixArray<SciantixVariable>& sciantix_variable)
{
    double q_Pu = sciantix_variable["q"].getFinalValue();
    double q_Am = 0.0;

    double target_om = 2.0 + stoichiometry_deviation;

    std::vector<double> parameter;
    parameter.push_back(temperature);
    parameter.push_back(q_Pu);
    parameter.push_back(target_om);
    parameter.push_back(q_Am);

    Solver solver;
    return solver.BisectionKato(parameter);
}
