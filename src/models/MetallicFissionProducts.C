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

    const double k_intra = 0.241313858 * 1.100694171252e-01 * 9.261187281288e-01 * 1.047128548051e+00 * scaling_factors["MFP precipitation rate intragranular"].getValue();
    const double k_gb    = 0.944860042 * 4.641588833612e-02 * 8.413951416452e-01 * 9.120108393559e-01 * scaling_factors["MFP precipitation rate grain boundary"].getValue();
    const double k_res_ref = 6.799e-19;   // s^-1, value used in the present calibration
    const double fission_rate_ref = 1.48e+19;  // fission rate used in the calibration
    const double k_res = k_res_ref * (fission_rate / fission_rate_ref) * scaling_factors["MFP resolution rate"].getValue();

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

    // Leggi i valori vecchi
    double cm_matrix_old     = sciantix_variable["Cm matrix"].getInitialValue();
    double cm_prec_intra_old = sciantix_variable["Cm precipitated intragranular"].getInitialValue();
    double cm_prec_gb_old    = sciantix_variable["Cm precipitated grain boundary"].getInitialValue();

    // Aggiornamento Cm - EULERO IMPLICITO ALTRIMENTI DIVERGE
    // dCm_matrix/dt = y * F - (k_intra + k_gb) * cm_matrix + k_res * (C_prec_intra + C_prec_inter)
    // dCm_prec_intragr/dt = + (k_intra) * cm_matrix - k_res Cm_prec_intra
    // dCm_prec_intergr/dy = + (k_gb) * cm_matrix - k_res * Cm_prec_inter
    

    // nucleation rate MFPs
    // Hp) heterogeneous nucleation mainly due to dislocation and bubbles
    // dislocation density (m^(-2)) to check,
    // Ref: Modelling dislocation density evolution of UO2 under irradiation, Aleksandar Djonovic
    double intra_bubble_density = sciantix_variable["Intragranular bubble concentration"].getFinalValue();
    double intra_bubble_radius  = sciantix_variable["Intragranular bubble radius"].getFinalValue();
    double bubble_sink_strenght = (4 * M_PI * intra_bubble_density * intra_bubble_radius);  // m^(-2)
    double temperature          = history_variable["Temperature"].getFinalValue();
    double dislocation_density = 0.0;
    if (temperature < 573.15)           // < 300°C
    {
        dislocation_density = 1.0e16;
    }
    else if (temperature < 673.15)      // 300-400°C
    {
        dislocation_density = 5.0e15;
    }
    else if (temperature < 873.15)      // 400-600°C
    {
        dislocation_density = 2.0e15;
    }
    else if (temperature < 1073.15)     // 600-800°C
    {
        dislocation_density = 5.0e14;
    }
    else                                // > 800°C
    {
        dislocation_density = 2.0e14;
    }

    const double f_dislocation = 0.67;  // coeff che pesa la nucleazione eterogenea sulle dislocazioni, da calibrare
    const double f_bubbles     = 0.33;  // coeff che pesa la nucleazione eterogenea su bolle di gas, da calibrare
    const double Kb            = boltzmann_constant_eV;
    const double dG_nucleation = 2.9 * 9.500000000000e-01 * scaling_factors["MFP nucleation energy barrier"].getValue();  // energy barrier (eV), da calibrazione Excel con k_intra ottimizzato

    // k_nucl coefficient - calculation through n and k_intra
    // introduzione scaling factor
    double k_nucl = 3.995668086708020e18 * 7.498942093325e-02 * 7.498942093325e-01 * 1.819783131708e-01 * scaling_factors["MFP nucleation rate"].getValue();  // atm/(m*s)

    // definizione nucleation rate 
    double nucleation_rate_m = (k_nucl * (f_dislocation * dislocation_density + f_bubbles * bubble_sink_strenght) *
                                (exp(-dG_nucleation / (Kb * temperature))));

    // parametri utili
    const double V_eff_5M = 1.44123e-29;           // volume atomico efficace pesato per una 5M (m^3)
    const double R_eff_M  = 1.365 * pow(10, -10);  // raggio atomico efficace di un metallo generico delle 5M
    // Intragranular 5MP radius
    double n_old = sciantix_variable["Intragranular atom per 5MP"].getInitialValue();
    double R_5M = pow((3.0 / (M_PI * 4.0)) * (V_eff_5M)*n_old, 1.0 / 3.0);


    // EC 04.06.2026: soluzione matrice 3x3 per aggiornamento simultaneo di Cm matrix, Cm_prec_intra e Cm_prec_gb con
    // eulero implicito
    double A_matrix[9]   = {0};
    double b_matrix[3]   = {0};
    double k_nucleazione = k_nucl * dt * exp(-dG_nucleation / (Kb * temperature));
    double N_iniziale    = sciantix_variable["Intragranular 5MPs concentration"].getInitialValue();
    double S_siti = dislocation_density + bubble_sink_strenght;
    double S_FMP= 4.0 * M_PI * R_5M * N_iniziale;
    double disponibili = (R_5M > 0.0) ? std::max(S_siti - S_FMP, 0.0) : S_siti;
    double Nucleazione =
        (k_nucleazione * (f_dislocation * dislocation_density + f_bubbles * bubble_sink_strenght)*(disponibili) + N_iniziale) /
        (1.0 + k_nucleazione);
    sciantix_variable["Intragranular 5MPs concentration"].setFinalValue(Nucleazione);

    A_matrix[0] = 1 + (k_intra + k_gb) * dt;
    A_matrix[1] = -k_res * dt;
    A_matrix[2] = -k_res * dt;
    A_matrix[3] = -k_intra * dt;
    A_matrix[4] = 1 + k_res * dt;
    A_matrix[5] = 0.0;
    A_matrix[6] = -k_gb * dt;
    A_matrix[7] = 0.0;
    A_matrix[8] = 1 + k_res * dt;
    b_matrix[0] = cm_matrix_old + produzione;
    b_matrix[1] = cm_prec_intra_old;
    b_matrix[2] = cm_prec_gb_old;

    solver.Laplace3x3(A_matrix, b_matrix);
    sciantix_variable["Cm matrix"].setFinalValue(b_matrix[0]);
    sciantix_variable["Cm precipitated intragranular"].setFinalValue(b_matrix[1]);
    sciantix_variable["Cm precipitated grain boundary"].setFinalValue(b_matrix[2]);

    double N_final = sciantix_variable["Intragranular 5MPs concentration"].getFinalValue();
    double n_media = 0.0;
    if (N_final > 0.0)
        n_media = sciantix_variable["Cm precipitated intragranular"].getFinalValue() / N_final;
    else
        n_media = 0.0;
    sciantix_variable["Intragranular atom per 5MP"].setFinalValue(n_media);
    // END EC 04.06.2026

    // Verifica interna: Cm_prec_intra deve essere uguale a N * n
    double N             = sciantix_variable["Intragranular 5MPs concentration"].getFinalValue();
    double n             = sciantix_variable["Intragranular atom per 5MP"].getFinalValue();
    double cm_prec_check = N * n;
    // cm_prec_check dovrebbe coincidere con Cm precipitated intragranular
}