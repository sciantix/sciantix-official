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

    // Fission yield per prodotti metallici di fissione, assunta come costante
    // Da cambiare e verificare tramite fonte bibliografica)
    const double y = 0.25;

    // Fission rate al passo attuale (fiss/m3/s)
    // Già letto automaticamente dal file di input ad ogni passo
    double fission_rate = history_variable["Fission rate"].getFinalValue();

    // Passo temporale (s)
    double dt = physics_variable["Time step"].getFinalValue();

    // Equazione fisica
    // dCm/dt = y * F  discretizzata su Δt:
    // Cm(t+dt) = Cm(t) + y * F * dt
    double produzione = y * fission_rate * dt;

    // Aggiornamento della variabile
    // addValue aggiunge 'produzione' al valore attuale di Cm
    // final_value è protected —> devo usare la funzione pubblica addValue
    sciantix_variable["Cm"].addValue(produzione);
}