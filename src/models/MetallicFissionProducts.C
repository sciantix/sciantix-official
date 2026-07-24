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
    const double y = 0.6;

    // Fission rate al passo attuale (fiss/m3/s)
    // Già letto automaticamente dal file di input ad ogni passo
    double fission_rate = history_variable["Fission rate"].getFinalValue();

    // Passo temporale (s)
    double dt = physics_variable["Time step"].getFinalValue();

    // Tassi di precipitazione (1/s) da calibrare su micrografie
    // k_intra: precipitazione dentro i grani
    // k_gb:    precipitazione ai bordi grano

    const double k_intra          = 0.241313858 * 1.100694171252e-01 * 9.261187281288e-01 * 1.047128548051e+00 *
                                    scaling_factors["MFP precipitation rate intragranular"].getValue();
    const double k_gb             = 0.944860042 * 4.641588833612e-02 * 8.413951416452e-01 * 9.120108393559e-01 *
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
    // dislocation density (m^(-2)) to check,
    // Ref: Modelling dislocation density evolution of UO2 under irradiation, Aleksandar Djonovic
    double bubble_sink_strenght = (4 * M_PI * sciantix_variable["Intragranular bubble concentration"].getFinalValue() *
                                   sciantix_variable["Intragranular bubble radius"].getFinalValue());  // m^(-2)
    double temperature          = history_variable["Temperature"].getFinalValue();
    double dislocation_density  = 0.0;
    if (temperature < 573.15)  // < 300°C
    {
        dislocation_density = 1.0e16;
    }
    else if (temperature < 673.15)  // 300-400°C
    {
        dislocation_density = 5.0e15;
    }
    else if (temperature < 873.15)  // 400-600°C
    {
        dislocation_density = 2.0e15;
    }
    else if (temperature < 1073.15)  // 600-800°C
    {
        dislocation_density = 5.0e14;
    }
    else  // > 800°C
    {
        dislocation_density = 2.0e14;
    }

    const double f_dislocation = 0.67;  // coeff che pesa la nucleazione eterogenea sulle dislocazioni, da calibrare
    const double f_bubbles     = 0.33;  // coeff che pesa la nucleazione eterogenea su bolle di gas, da calibrare
    const double dG_nucleation = 2.9 * 9.500000000000e-01 *
                                 scaling_factors["MFP nucleation energy barrier"]
                                     .getValue();  // energy barrier (eV), da calibrazione Excel con k_intra ottimizzato

    // k_nucl coefficient - calculation through n and k_intra
    // introduzione scaling factor
    double k_nucl = 3.995668086708020e18 * 7.498942093325e-02 * 7.498942093325e-01 * 1.819783131708e-01 *
                    scaling_factors["MFP nucleation rate"].getValue();  // atm/(m*s)

    // Intragranular 5MP radius
    const double V_eff_5M = 1.44123e-29;  // volume atomico efficace pesato per una 5M (m^3)
    double       R_5M     = pow(
        (3.0 / (M_PI * 4.0)) * (V_eff_5M)*sciantix_variable["Intragranular atom per 5MP"].getInitialValue(), 1.0 / 3.0);

    // Aggiornamento Cm - EULERO IMPLICITO ALTRIMENTI DIVERGE
    // dCm_matrix/dt = y * F - (k_intra + k_gb) * cm_matrix + k_res * (C_prec_intra + C_prec_inter)
    // dCm_prec_intragr/dt = + (k_intra) * cm_matrix - k_res Cm_prec_intra
    // dCm_prec_intergr/dt = + (k_gb) * cm_matrix - k_res * Cm_prec_inter
    double A_matrix[9]   = {0};
    double b_matrix[3]   = {0};
    double k_nucleazione = k_nucl * dt * exp(-dG_nucleation / (boltzmann_constant_eV * temperature));
    double N_iniziale    = sciantix_variable["Intragranular 5MPs concentration"].getInitialValue();
    double S_siti        = dislocation_density + bubble_sink_strenght;
    double S_FMP         = 4.0 * M_PI * R_5M * N_iniziale;
    double disponibili   = (R_5M > 0.0) ? std::max(S_siti - S_FMP, 0.0) : S_siti;

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
    const double n_min = 2.0;
    double N_candidate = N_iniziale + k_nucleazione *
                                          (f_dislocation * dislocation_density + f_bubbles * bubble_sink_strenght) *
                                          disponibili;
    double N_cap       = b_matrix[1] / n_min; // minimo numero di atomi per 5MP
    double N_final     = std::max(std::min(N_candidate, N_cap), N_iniziale);
    sciantix_variable["Intragranular 5MPs concentration"].setFinalValue(N_final);

    double n_media = (N_final > 0.0) ? (b_matrix[1] / N_final) : 0.0;
    sciantix_variable["Intragranular atom per 5MP"].setFinalValue(n_media);
}