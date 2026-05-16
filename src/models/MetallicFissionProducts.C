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

void Simulation::MetallicFissionProducts()
{
    // Se iCm = 0 nel file di input, il modello è disattivato
    if (!input_variable["iCm"].getValue())
        return;

    // Fission yield dei prodotti metallici di fissione, Hp) costante
    // Da cambiare e verificare tramite fonte bibliografica
    // assumiamo yield efficace per una 5MP, non 5 metalli separati
    const double y = 0.6;

    // Tassi di precipitazione (1/s) da calibrare su micrografie
    // k_intra: precipitazione dentro i grani
    // k_gb:    precipitazione ai bordi grano
    const double k_intra = 1.0e-10;
    const double k_gb    = 1.0e-9;

    // Fission rate al passo attuale (fiss/m3/s)
    // Già letto automaticamente dal file di input ad ogni passo
    double fission_rate = history_variable["Fission rate"].getFinalValue();

    // Passo temporale (s)
    double dt = physics_variable["Time step"].getFinalValue();

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

    // Valore di Cm matrix al passo precedente usato per calcolare i sink
    double cm_matrix = sciantix_variable["Cm matrix"].getFinalValue();
    // TERMINI DI PERDITA
    double sink_intra = k_intra * cm_matrix;  // precipitazione intragranulare (at/m3 s)
    double sink_gb    = k_gb * cm_matrix;     // precipitazione al bordo grano (at/m3 s)
    // Aggiornamento Cm
    // dCm_matrix/dt = y * F - (k_intra + k_gb) * cm_matrix
    sciantix_variable["Cm matrix"].addValue(produzione - (sink_intra + sink_gb) * dt);

    // Cm PRECIPIATTA INTRAGRANO

    // Aggiornamento della variabile
    // dCm_prec_intragr/dt = + k_intra * cm_matrix
    sciantix_variable["Cm precipitated intragranular"].addValue(sink_intra * dt);

    // Cm PRECIPITATA A GB

    // Aggiornamento della variabile
    // dCm_prec_intergr/dy = + k_gb * cm_matrix
    sciantix_variable["Cm precipitated grain boundary"].addValue(sink_gb * dt);
}