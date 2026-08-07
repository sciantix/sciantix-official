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
//  Version: 2.2.1                                                                  //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

void Simulation::MetallicFissionProducts()
{
    // Se iCm = 0 nel file di input, il modello è disattivato
    if (!input_variable["iCm"].getValue())
        return;

    // Fission yield dei prodotti metallici di fissione, Hp) costante
    // Da cambiare e verificare tramite fonte bibliografica
    // assumiamo yield efficace per una 5MP, non 5 metalli separati
    const double y = 0.2;

    // Fission rate al passo attuale (fiss/m3/s)
    // Già letto automaticamente dal file di input ad ogni passo
    double fission_rate = history_variable["Fission rate"].getFinalValue();

    // Passo temporale (s)
    double dt = physics_variable["Time step"].getFinalValue();

    double temperature = history_variable["Temperature"].getFinalValue();

    // Tassi di precipitazione (1/s) da calibrare su micrografie
    // k_intra: precipitazione dentro i grani
    // k_gb:    precipitazione ai bordi grano

    const double k_intra = 1 *
                           scaling_factors["MFP precipitation rate intragranular"].getValue();
    const double k_gb = 1 *
                        scaling_factors["MFP precipitation rate grain boundary"].getValue();
    const double k_res_ref        = 6.799e-19;  // s^-1, value used in the present calibration
    const double fission_rate_ref = 1.48e+19;   // fission rate used in the calibration
    const double k_res =
        k_res_ref * (fission_rate / fission_rate_ref) * scaling_factors["MFP resolution rate"].getValue();

    // CONCENTRAZIONE TOTALE Cm

    // Equazione fisica
    // dCm/dt = y * F  discretizzata su Δt:
    // Cm(t+dt) = Cm(t) + y * F * dt
    double produzione = y * fission_rate * dt;

    // Aggiornamento della variabile
    // addValue aggiunge 'produzione' al valore attuale di Cm
    // final_value è protected —> devo usare la funzione pubblica addValue
    sciantix_variable["Cm"].addValue(produzione);

    // CONCENTRAZIONE LIBERA IN MATRICE Cm matrix

    // nucleation rate MFPs
    // Hp) heterogeneous nucleation mainly due to dislocation and bubbles
    // dislocation density (m^(-2)),
    // Ref: Modelling dislocation density evolution of UO2 under irradiation, Aleksandar Djonovic
    double bubble_sites_intra = sciantix_variable["Intragranular bubble concentration"].getFinalValue();
    double bubble_sites_inter = sciantix_variable["Intergranular bubble concentration"].getFinalValue();
    const double lambda = 15e-9;
    double dislocation_density = 0.0;

    double burnup=sciantix_variable["Burnup"].getFinalValue();

    const double A     = 6.545e12;
    const double n     = 1.151;
    const double A_inf = 0.608;
    const double T_c   = 1109.0;
    const double dT    = 25.8;
    const double fT    = A_inf + (1.0 - A_inf) / (1.0 + exp((temperature - T_c) / dT));

    if (burnup <= 0.0 || temperature <= 0.0)
        dislocation_density = 0.0;
    else
        dislocation_density = A * pow(burnup, n) * fT;

    double dislocation_sites = dislocation_density / lambda;
    //Veshchunov 2009 dislocation-density correlation, dependent on burnup and temperature.

    //for intragranular nucleation
    const double f_dislocation = 0.67;  // coeff che pesa la nucleazione eterogenea sulle dislocazioni, Imagej
    const double f_bubbles     = 0.33;  // coeff che pesa la nucleazione eterogenea su bolle di gas, Imagej

    const double dG_nucleation = 1 *
                                 scaling_factors["MFP nucleation energy barrier"].getValue();  // energy barrier (eV)

    // k_nucl coefficient
    // introduzione scaling factor
    double k_nucl = 1 * scaling_factors["MFP nucleation rate"].getValue();  // atm/(m*s)

    // Intragranular 5MP radius
    const double V_eff_5M = 1.44123e-29;  // volume atomico efficace pesato per una 5M (m^3), Excel

    // Aggiornamento Cm - EULERO IMPLICITO ALTRIMENTI DIVERGE
    // dCm_matrix/dt = y * F - (k_intra + k_gb) * cm_matrix + k_res * (C_prec_intra + C_prec_inter)
    // dCm_prec_intragr/dt = + (k_intra) * cm_matrix - k_res Cm_prec_intra
    // dCm_prec_intergr/dt = + (k_gb) * cm_matrix - k_res * Cm_prec_inter
    double A_matrix[9]   = {0};
    double b_matrix[3]   = {0};
    double k_nucleazione = k_nucl * exp(-dG_nucleation / (boltzmann_constant_eV * temperature));
    double N_iniziale_intra    = sciantix_variable["Intragranular 5MPs concentration"].getInitialValue();
    double N_iniziale_inter    = sciantix_variable["Intergranular 5MPs concentration"].getInitialValue();

    A_matrix[0] = 1 + (k_intra + k_gb) * dt;
    A_matrix[1] = -k_res * dt;
    A_matrix[2] = -k_res * dt;
    A_matrix[3] = -k_intra * dt;
    A_matrix[4] = 1 + k_res * dt;
    A_matrix[5] = 0.0;
    A_matrix[6] = -k_gb * dt;
    A_matrix[7] = 0.0;
    A_matrix[8] = 1 + k_res * dt;
    b_matrix[0] = sciantix_variable["Cm matrix"].getInitialValue() + produzione;
    b_matrix[1] = sciantix_variable["Cm precipitated intragranular"].getInitialValue();
    b_matrix[2] = sciantix_variable["Cm precipitated grain boundary"].getInitialValue();

    solver.Laplace3x3(A_matrix, b_matrix);
    sciantix_variable["Cm matrix"].setFinalValue(b_matrix[0]);
    sciantix_variable["Cm precipitated intragranular"].setFinalValue(b_matrix[1]);
    sciantix_variable["Cm precipitated grain boundary"].setFinalValue(b_matrix[2]);

    // PROVA (in discussione con Vittoria, D4).
    const double n_min       = 2.0;
    double       N_candidate_intra = solver.Decay(
                                        N_iniziale_intra,
                                        k_nucleazione,
                                        k_nucleazione* (f_dislocation * dislocation_sites + f_bubbles * bubble_sites_intra),
                                        physics_variable["Time step"].getFinalValue()
    );

    double       N_candidate_inter = solver.Decay(
                                        N_iniziale_inter,
                                        k_nucleazione,
                                        k_nucleazione*bubble_sites_inter,
                                        physics_variable["Time step"].getFinalValue()
    );

    double N_cap_intra   = b_matrix[1] / n_min;  // minimo numero di atomi per 5MP
    double N_final_intra = std::max(std::min(N_candidate_intra, N_cap_intra), N_iniziale_intra);
    sciantix_variable["Intragranular 5MPs concentration"].setFinalValue(N_final_intra);

    double N_cap_inter   = b_matrix[2] / n_min;  // minimo numero di atomi per 5MP
    double N_final_inter = std::max(std::min(N_candidate_inter, N_cap_inter), N_iniziale_inter);
    sciantix_variable["Intergranular 5MPs concentration"].setFinalValue(N_final_inter);

    double n_media_intra = (N_final_intra > 0.0) ? (b_matrix[1] / N_final_intra) : 0.0;
    sciantix_variable["Intragranular atom per 5MP"].setFinalValue(n_media_intra);
    double n_media_inter = (N_final_inter > 0.0) ? (b_matrix[2] / N_final_inter) : 0.0;
    sciantix_variable["Intergranular atom per 5MP"].setFinalValue(n_media_inter);

}