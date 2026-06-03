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

    // Tassi di precipitazione (1/s) da calibrare su micrografie
    // k_intra: precipitazione dentro i grani
    // k_gb:    precipitazione ai bordi grano
    const double k_intra = 0.241313858;
    const double k_gb    = 0.944860042;
    const double k_res   = 6.799 * pow(10, -19);

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

    // Leggi i valori vecchi
    double cm_matrix_old     = sciantix_variable["Cm matrix"].getInitialValue();
    double cm_prec_intra_old = sciantix_variable["Cm precipitated intragranular"].getInitialValue();
    double cm_prec_gb_old    = sciantix_variable["Cm precipitated grain boundary"].getInitialValue();

    // Aggiornamento Cm - EULERO IMPLICITO ALTRIMENTI DIVERGE
    // dCm_matrix/dt = y * F - (k_intra + k_gb) * cm_matrix + k_res * (C_prec_intra + C_prec_inter)
    double cm_matrix_new = (cm_matrix_old + dt * (y * fission_rate + k_res * (cm_prec_intra_old + cm_prec_gb_old))) /
                           (1.0 + dt * (k_intra + k_gb));

    sciantix_variable["Cm matrix"].setFinalValue(cm_matrix_new);

    // Cm PRECIPIATTA INTRAGRANO

    // Aggiornamento della variabile
    // dCm_prec_intragr/dt = + (k_intra) * cm_matrix - k_res Cm_prec_intra
    double cm_prec_intra_new = (cm_prec_intra_old + dt * k_intra * cm_matrix_new) / (1.0 + dt * k_res);
    sciantix_variable["Cm precipitated intragranular"].setFinalValue(cm_prec_intra_new);

    // Aggiornamento della variabile
    // dCm_prec_intragr/dt = + k_intra * cm_matrix
    sciantix_variable["Cm precipitated intragranular"].addValue(sink_intra * dt);

    // Aggiornamento della variabile
    // dCm_prec_intergr/dy = + (k_gb) * cm_matrix - k_res * Cm_prec_inter
    double cm_prec_gb_new = (cm_prec_gb_old + dt * k_gb * cm_matrix_new) / (1.0 + dt * k_res);
    sciantix_variable["Cm precipitated grain boundary"].setFinalValue(cm_prec_gb_new);

    // Definizione Cm_eq (mx solubility)
    // Da the chemical state of the fission products in oxide fuels, H. KLEYKAMP
    // e Da Solubility and clustering of ruthenium fission products in uranium dioxide
    // as determined by density functional theory, Minki Hong
    // sensato in prima approx porre C_eq=0
    const double Cm_eq = 0.0;

    // nucleation rate MFPs
    // EQUATION: v = k_nucl * (f_disl*dislocation density + f_bub*sink_strenght_bub) * C_matrix^2
    // Hp) heterogeneous nucleation mainly due to dislocation and bubbles
    // dislocation density (m^(-2)) to check,
    // Ref: Modelling dislocation density evolution of UO2 under irradiation, Aleksandar Djonovic
    double intra_bubble_density = sciantix_variable["Intragranular bubble concentration"].getFinalValue();
    double intra_bubble_radius  = sciantix_variable["Intragranular bubble radius"].getFinalValue();
    double bubble_sink_strenght = (4 * M_PI * intra_bubble_density * intra_bubble_radius);  // m^(-2)
    double temperature          = history_variable["Temperature"].getFinalValue();
    double dislocation_density  = 0.0;
    if (temperature < 673.15)  // Hp) 400°C come thershold ma potremmo farne più casi, oppure
                               // introdurre dislocation_density come sciantix_variable
    {
        dislocation_density = 5.0 * pow(10, 15);
    }
    else
    {
        dislocation_density = 5.0 * pow(10, 14);
    }
    const double f_dislocation = 0.5;  // coeff che pesa la nucleazione eterogenea sulle dislocazioni, da calibrare
    const double f_bubbles     = 0.5;  // coeff che pesa la nucleazione eterogenea su bolle di gas, da calibrare
    const double Kb            = boltzmann_constant_eV;
    const double dG_nucleation = 2.9;  // energy barrier (eV), da calibrazione Excel con k_intra ottimizzato

    // k_nucl coefficient - calculation through n and k_intra
    double N_current = sciantix_variable["Intragranular 5MPs concentration"].getFinalValue();
    double n_old     = 0.0;

    // Inizializza n se ci sono bolle ma n è ancora zero
    // altrimenti avremmo diviso 0
    if (N_current > 0.0 && n_old == 0.0)
    {
        n_old = 2.0;
        sciantix_variable["Intragranular atom per 5MP"].setFinalValue(n_old);
    }
    else
    {
        n_old = sciantix_variable["Intragranular atom per 5MP"].getFinalValue();
    }

    double k_nucl = 4.4253016024739E-47;  // (m^5)/(atm^2)(s), calibrato su Excel

    // definiziatione nucleation rate
    double nucleation_rate_m = (k_nucl * (f_dislocation * dislocation_density + f_bubbles * bubble_sink_strenght) *
                                (exp(-dG_nucleation / (Kb * temperature))) * pow((cm_matrix_new - Cm_eq), 2));

    // Intragranular 5MPs concentration N
    // discrtizzazione equazione
    // EULERO ESPLICITO
    double dN = nucleation_rate_m * dt;
    sciantix_variable["Intragranular 5MPs concentration"].addValue(dN);

    // resolution rate coefficient definition
    // const double b = k_res; // parametro da calibrare
    // valore attuale con Sakib, 2025
    // double b = b_0 * fission_rate; // resolution coefficient
    const double b = k_res;

    // trapping rate coefficient g - diffusione verso una sfera
    const double V_eff_5M = 1.44123e-29;           // volume atomico efficace pesato per una 5M (m^3)
    const double R_eff_M  = 1.365 * pow(10, -10);  // raggio atomico efficace di un metallo generico delle 5M

    // diffusion coefficient of a Mt in UO2
    // D_Ru4+ taken from: Diffusion of solid fission products in UO2 and UO2+x, Xi Zhou
    // double D_m_Ru4+ = 7.5 * pow(10, -15) * exp(-4.63/(Kb * temperature)); // m2/s
    // nuovi valori di D_Mt calibrati da esperimenti di annealing
    // Q in eV e D_0 m^2/s
    // RELEASE OF FISSION PRODUCTS (Xe, I, Te, Cs, MO AND Tc) FROM POLYLYCRYSTALLINE U02
    // by S.G. PRUSSIN, D.R. OLANDER, W.K. LAU and L. HANSSON
    double D_Ru = 2.717089493 * exp(-6.76486202 / (Kb * temperature));
    double D_Pd = D_Ru;
    double D_Rh = D_Ru;
    double D_Tc = 54.83368418 * exp(-8.51626727 / (Kb * temperature));
    double D_Mo = 0.0;
    if (temperature <= 2000)
    {
        D_Mo = 1.40736E+43 * exp(-25.29793853 / (Kb * temperature));
    }
    else
    {
        D_Mo = 14509643.57 * exp(-11.00540754 / (Kb * temperature));
    }
    double D_m = 0.4 * D_Mo + 0.3 * D_Ru + 0.1 * D_Tc + 0.15 * D_Pd + 0.05 * D_Rh;

    // Intragranular 5MP radius
    double R_5M = pow((3.0 / (M_PI * 4.0)) * (V_eff_5M)*n_old, 1.0 / 3.0);

    // trapping coefficient g
    double g = ((4.0 * M_PI) * (R_5M + R_eff_M) * (D_m));

    // Intragranular atom per 5MP
    // dn/dt = (g)*(c_matrix) - b * n

    double source_n = g * cm_matrix_new;
    double n_new    = (n_old + source_n * dt) / (1.0 + b * dt);
    sciantix_variable["Intragranular atom per 5MP"].setFinalValue(n_new);

    // Verifica interna: Cm_prec_intra deve essere uguale a N * n
    double N             = sciantix_variable["Intragranular 5MPs concentration"].getFinalValue();
    double n             = sciantix_variable["Intragranular atom per 5MP"].getFinalValue();
    double cm_prec_check = N * n;
    // cm_prec_check dovrebbe coincidere con Cm precipitated intragranular
}