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

    // Passo temporale (s)
    double dt = physics_variable["Time step"].getFinalValue();

    // Effective production yield dei cinque metallic fission products
    // Mo-Tc-Ru-Rh-Pd.
    // Il valore y = 0.578 atomi/fissione deriva dalla composizione PRODHEL
    // del combustibile sperimentale: Mo + Tc + Ru + Rh + Pd = 28.9 at.% dei
    // prodotti di fissione, assumendo circa due prodotti di fissione per fissione.
    //
    // Si tratta quindi di un yield efficace specifico del sistema considerato,
    // non di un independent fission yield universale.
    const double y = 0.578;  // atomi metallici / fissione

    // Fission rate al passo attuale (fiss/m3/s)
    // Già letto automaticamente dal file di input ad ogni passo
    double fission_rate = history_variable["Fission rate"].getFinalValue();

    double T = history_variable["Temperature"].getFinalValue();

    double kB = 8.617333262145e-5;  // eV/K, costante di Boltzmann

    // Tassi di precipitazione da calibrare su micrografie
    // k_intra: precipitazione dentro i grani
    // k_gb:    precipitazione ai bordi grano

    const double k_intra_0 = 1.0 * scaling_factors["MFP precipitation rate intragranular"].getValue();
    const double dG_intra  = 1.0 * scaling_factors["MFP intra activation energy"].getValue();
    const double k_gb_0    = 1.0 * scaling_factors["MFP precipitation rate grain boundary"].getValue();
    const double dG_gb     = 1.0 * scaling_factors["MFP grain boundary activation energy"].getValue();

    // Irradiation-induced re-solution of metallic precipitates.
    //
    // k_res_ref is derived from the 5MP shrinkage measured by Sakib et al.
    // between 0.30 and 1.75 dpa:
    //
    // R1/R0 = 0.80/0.91
    //
    // assuming:
    //      n ~ R^3
    //      dn/dt = -k_res * n
    //
    // and a reference LWR damage rate of approximately 1 dpa/day.
    //
    // This gives:
    //      k_res_ref = 3.085e-6 s^-1
    //
    // The coefficient is then scaled with the local fission rate.

    const double k_res_ref        = 3.085078e-6;  // s^-1
    const double fission_rate_ref = 1.48e19;      // fission rate used in the calibration
    const double k_res =
        k_res_ref * (fission_rate / fission_rate_ref) * scaling_factors["MFP resolution rate"].getValue();

    // CONCENTRAZIONE TOTALE Cm

    // Equazione fisica:
    // dCm/dt = y * F
    //
    // discretizzata su dt:
    // Cm(t+dt) = Cm(t) + y * F * dt
    //
    // y  = fission yield [atomi/fissione]
    // F  = fission rate [fissioni/m3/s]
    // dt = timestep [s]
    //
    // La produzione ha quindi unità [atomi/m3]
    double produzione = y * fission_rate * dt;

    // Aggiornamento della variabile
    // addValue aggiunge 'produzione' al valore attuale di Cm
    // final_value è protected -> devo usare la funzione pubblica addValue
    sciantix_variable["Cm"].addValue(produzione);

    // SETUP: VALORI INIZIALI DEL TIMESTEP

    double Cm_matrix_old       = sciantix_variable["Cm matrix"].getInitialValue();
    double Cm_precip_intra_old = sciantix_variable["Cm precipitated intragranular"].getInitialValue();
    double Cm_precip_gb_old    = sciantix_variable["Cm precipitated grain boundary"].getInitialValue();

    double N_iniziale_intra = sciantix_variable["Intragranular 5MPs concentration"].getInitialValue();
    double N_iniziale_inter = sciantix_variable["Intergranular 5MPs concentration"].getInitialValue();

    // Minimum atomistic floor for a newly nucleated cluster.
    //
    // IMPORTANT:
    // this is no longer interpreted as the physical size of every new nucleus.
    // The physical critical nucleus size is calculated below from CNT:
    //
    //      n* = 2 DeltaG* / DeltaMu
    //
    // with DeltaMu = kB*T*ln(S).
    //
    // The value 2 is retained only as a lower atomistic/numerical bound.
    // It is consistent with the literature indication that metallic dimers
    // can act as precursors of metallic precipitates in UO2.
    const double n_floor = 2.0;

    // EQUILIBRIUM CONCENTRATION AND SUPERSATURATION
    //
    // La nucleazione viene attivata dalla sovrasaturazione della matrice:
    //
    //      S = C_matrix / C_eq(T)
    //
    // con:
    //
    //      C_eq(T) = C_eq_ref *
    //                exp[-(H_sol/kB) * (1/T - 1/T_ref)]
    //
    // Parametri literature-based:
    //
    // - x_eq_ref = 0.006 at.% = 6e-5:
    //   upper bound della solubilita' del Mo in UO2 stechiometrico
    //   a 1900 degC (Kleykamp).
    //
    // - a_UO2 = 547.5 pm:
    //   parametro reticolare dello UO2. La cella fluorite contiene
    //   quattro siti cationici U, quindi:
    //
    //      N_sites = 4 / a_UO2^3
    //
    // - H_sol_eff = 1.14 eV:
    //   effective solution enthalpy, assunta uguale alla solution energy
    //   DFT del Mo metallico bcc in UO2 stechiometrico (Brillant/Gupta).
    //
    // Poiche' x_eq_ref e' un upper bound sperimentale per il Mo e il modello
    // rappresenta Mo-Tc-Ru-Rh-Pd come una singola specie, C_eq(T) deve essere
    // interpretata come effective equilibrium concentration del modello 5MP.

    const double T_eq_ref    = 2173.15;                    // K = 1900 degC
    const double x_eq_ref    = 6.0e-5;                     // 0.006 at.%
    const double a_UO2_ref   = 547.5e-12;                  // m
    const double N_sites_UO2 = 4.0 / pow(a_UO2_ref, 3.0);  // m^-3
    const double C_eq_ref    = x_eq_ref * N_sites_UO2;     // ~1.462e24 at/m3
    const double H_sol_eff   = 1.14;                       // eV

    double C_eq = C_eq_ref;

    if (T > 0.0)
        C_eq = C_eq_ref * exp(-(H_sol_eff / kB) * (1.0 / T - 1.0 / T_eq_ref));

    // Stato della matrice usato per valutare la nucleazione:
    // concentrazione all'inizio del timestep + produzione del timestep,
    // prima dei trasferimenti per nucleazione e precipitazione.
    double Cm_matrix_for_nucleation = std::max(Cm_matrix_old + produzione, 0.0);

    double supersaturation     = 0.0;
    double log_supersaturation = 0.0;

    if (C_eq > 0.0)
        supersaturation = Cm_matrix_for_nucleation / C_eq;

    if (supersaturation > 1.0)
        log_supersaturation = log(supersaturation);

    // Chemical driving force per metallic atom for precipitation:
    //
    //      DeltaMu = kB * T * ln(S)
    //
    // [eV/atom], since kB is expressed in eV/K.
    //
    // DeltaMu is positive only for S > 1. For S <= 1 the matrix is not
    // supersaturated and nucleation is disabled below.
    double delta_mu = 0.0;

    if (T > 0.0 && log_supersaturation > 1.0e-12)
        delta_mu = kB * T * log_supersaturation;

    // HETEROGENEOUS NUCLEATION SITES - INTRAGRANULAR
    //
    // Per la popolazione intragranulare manteniamo, in questa prima modifica,
    // gli stessi siti eterogenei gia' utilizzati: dislocazioni + bolle
    // intragranulari. In questo modo isoliamo l'effetto della nuova dipendenza
    // dalla sovrasaturazione senza modificare contemporaneamente la definizione
    // dei siti intra.

    double bubble_sites_intra = sciantix_variable["Intragranular bubble concentration"].getFinalValue();

    // Dislocation density
    // Ref: Modelling dislocation density evolution of UO2 under irradiation
    const double lambda              = 15e-9;  // m, characteristic spacing between effective nucleation sites
    double       dislocation_density = 0.0;

    double burnup = sciantix_variable["Burnup"].getFinalValue();

    const double A     = 6.545e12;
    const double n     = 1.151;
    const double A_inf = 0.608;
    const double T_c   = 1109.0;
    const double dT    = 25.8;
    const double fT    = A_inf + (1.0 - A_inf) / (1.0 + exp((T - T_c) / dT));

    if (burnup <= 0.0 || T <= 0.0)
        dislocation_density = 0.0;
    else
        dislocation_density = A * pow(burnup, n) * fT;

    double dislocation_sites = dislocation_density / lambda;

    // Coefficienti che pesano la nucleazione eterogenea
    // su dislocazioni e bolle, stimati tramite analisi delle immagini.
    // Questi pesi vengono mantenuti invariati in questa prima implementazione.
    const double f_dislocation = 0.67;
    const double f_bubbles     = 0.33;

    double siti_intra = f_dislocation * dislocation_sites + f_bubbles * bubble_sites_intra;

    // CNT-INSPIRED SUPERSATURATION-DEPENDENT NUCLEATION BARRIER
    //
    // Classical nucleation theory:
    //
    //      DeltaG* ~ 1 / [T^2 * ln(S)^2]
    //
    // and therefore:
    //
    //      k_nucl = k0 * exp[-DeltaG*(T,S)/(kB*T)]
    //
    // Since gamma and the heterogeneous shape factor are not independently
    // known for the effective 5MP/UO2 system, they are absorbed into one
    // effective barrier coefficient.
    //
    // To preserve the existing dimensionless scaling-factor interface, the
    // parameter "MFP nucleation energy barrier" now scales a reference
    // coefficient B_ref = 1e6 eV K^2:
    //
    //      DeltaG*(T,S) = B / [T^2 ln(S)^2]
    //
    // with B = B_ref * scaling_factor.
    //
    // B_ref = 1e6 eV K^2 corresponds simply to DeltaG* = 1 eV at
    // T = 1000 K and S = e. This is a normalization, not an additional
    // physical parameter.
    //
    // For S <= 1 nucleation is thermodynamically inactive.

    const double B_nucleation_ref = 1.0e6;  // eV K^2

    double B_nucleation = B_nucleation_ref * scaling_factors["MFP nucleation energy barrier"].getValue();

    double k_nucl = 1.0 * scaling_factors["MFP nucleation rate"].getValue();  // s^-1

    double dG_nucleation = 0.0;  // eV
    double k_nucleazione = 0.0;  // s^-1

    if (T > 0.0 && log_supersaturation > 1.0e-12)
    {
        dG_nucleation = B_nucleation / (T * T * log_supersaturation * log_supersaturation);

        double nucleation_exponent = dG_nucleation / (kB * T);

        // Protezione numerica: per barriere molto grandi exp(-x) tende a zero.
        if (nucleation_exponent < 700.0)
            k_nucleazione = k_nucl * exp(-nucleation_exponent);
        else
            k_nucleazione = 0.0;
    }

    double S_intra = siti_intra * k_nucleazione;  // [m^-3 s^-1]

    // HETEROGENEOUS NUCLEATION SITES - GRAIN BOUNDARY
    //
    // La popolazione GB non viene piu' vincolata al numero di bolle
    // intergranulari. Il grain boundary stesso e' trattato come sito
    // eterogeneo di nucleazione.
    //
    // Per grani sferici:
    //
    //      A_GB / V = 3 / (2a)      [m^-1]
    //
    // dove a e' il grain radius. Il fattore 2 tiene conto della condivisione
    // della superficie di bordo tra due grani.
    //
    // Se lambda e' la distanza caratteristica tra siti sulla superficie GB,
    // la densita' areale dei siti e' circa 1/lambda^2 [m^-2]. Pertanto:
    //
    //      N_sites_GB = (3 / 2a) * (1 / lambda^2)      [m^-3]
    //
    // Il grain radius e' letto direttamente da SCIANTIX. Se il modello HBS
    // modifica gia' "Grain radius", l'aumento della superficie GB entra
    // automaticamente in questa formulazione senza introdurre un fattore HBS
    // aggiuntivo.

    double grain_radius = sciantix_variable["Grain radius"].getFinalValue();

    double grain_boundary_surface_to_volume = 0.0;

    if (grain_radius > 0.0)
        grain_boundary_surface_to_volume = 3.0 / (2.0 * grain_radius);

    double siti_inter = grain_boundary_surface_to_volume / (lambda * lambda);

    // La nucleazione GB usa la stessa supersaturazione della matrice ma mantiene
    // prefattore e coefficiente di barriera separati.

    double B_nucleation_inter =
        B_nucleation_ref * scaling_factors["MFP nucleation energy barrier grain boundary"].getValue();

    double k_nucl_inter = 1.0 * scaling_factors["MFP nucleation rate grain boundary"].getValue();  // s^-1

    double dG_nucleation_inter = 0.0;  // eV
    double k_nucleazione_inter = 0.0;  // s^-1

    if (T > 0.0 && log_supersaturation > 1.0e-12)
    {
        dG_nucleation_inter = B_nucleation_inter / (T * T * log_supersaturation * log_supersaturation);

        double nucleation_exponent_inter = dG_nucleation_inter / (kB * T);

        if (nucleation_exponent_inter < 700.0)
            k_nucleazione_inter = k_nucl_inter * exp(-nucleation_exponent_inter);
        else
            k_nucleazione_inter = 0.0;
    }

    double S_inter = siti_inter * k_nucleazione_inter;  // [m^-3 s^-1]

    // 5MP EFFECTIVE ATOMIC VOLUME

    // Volume atomico efficace pesato per una 5MP.
    // Utilizzato anche nel codice Python di calibrazione per mantenere
    // coerenza tra conversione concentrazione-volume e calcolo del raggio.
    const double V_eff_5M = 1.44123e-29;  // effective atomic volume of 5MP [m3/atom]

    // PRECIPITATION COEFFICIENTS

    // k_intra e k_gb valutati UNA SOLA VOLTA con lo stato "old"
    // all'inizio del timestep.
    //
    // Niente iterazione: la formula di crescita (r cresce con C_precip)
    // crea una retroazione positiva instabile se iterata nello stesso
    // timestep con dt grandi.

    double N_intra_safe_old = std::max(N_iniziale_intra, 1e-30);
    double N_inter_safe_old = std::max(N_iniziale_inter, 1e-30);

    // Protezione numerica: un eventuale piccolo valore negativo di C_precip
    // generato dal solver non deve entrare nella radice cubica.
    double Cm_precip_intra_safe_old = std::max(Cm_precip_intra_old, 0.0);
    double Cm_precip_gb_safe_old    = std::max(Cm_precip_gb_old, 0.0);

    double radius_intra_old =
        pow((3.0 * Cm_precip_intra_safe_old * V_eff_5M) / (4.0 * M_PI * N_intra_safe_old), 1.0 / 3.0);
    double radius_gb_old = pow((3.0 * Cm_precip_gb_safe_old * V_eff_5M) / (4.0 * M_PI * N_inter_safe_old), 1.0 / 3.0);

    double k_intra = k_intra_0 * exp(-dG_intra / (kB * T)) * 4.0 * M_PI * N_intra_safe_old * radius_intra_old;
    double k_gb    = k_gb_0 * exp(-dG_gb / (kB * T)) * 4.0 * M_PI * N_inter_safe_old * radius_gb_old;

    // 1. AGGIORNAMENTO N
    // Eulero implicito:
    //
    // dN_intra/dt = S_intra - k_nucleazione * N_intra
    // dN_inter/dt = S_inter - k_nucleazione_inter * N_inter
    //
    // N_new = (N_old + S*dt) / (1 + k*dt)

    double N_candidate_intra = (N_iniziale_intra + S_intra * dt) / (1.0 + k_nucleazione * dt);
    double N_candidate_inter = (N_iniziale_inter + S_inter * dt) / (1.0 + k_nucleazione_inter * dt);

    // 2. NUMERO DI NUOVE PARTICELLE RICHIESTE

    // Non permettiamo una diminuzione di N in questo modello:
    // se N_candidate < N_old, il numero di nuove particelle è zero.

    double dN_intra_requested = std::max(N_candidate_intra - N_iniziale_intra, 0.0);
    double dN_inter_requested = std::max(N_candidate_inter - N_iniziale_inter, 0.0);

    // 3. CRITICAL NUCLEUS SIZE AND MASS REQUIRED BY NUCLEATION

    // In the previous formulation every new 5MP was created with a fixed
    // minimum size of two atoms. Once the nucleation barrier is described
    // through classical nucleation theory, that assumption is no longer
    // internally consistent.
    //
    // For the capillarity form of CNT:
    //
    //      DeltaG(n) = -n*DeltaMu + a*n^(2/3)
    //
    // the critical nucleus corresponds to d(DeltaG)/dn = 0. At the maximum:
    //
    //      DeltaG* = (1/2) n* DeltaMu
    //
    // and therefore:
    //
    //      n* = 2 DeltaG* / DeltaMu
    //
    // with:
    //
    //      DeltaMu = kB*T*ln(S).
    //
    // The same relation is also obtained from the nucleation theorem
    // n* = -d(DeltaG*)/d(DeltaMu), provided the effective interfacial/
    // heterogeneous coefficient is independent of supersaturation.
    //
    // Since the intra- and intergranular barriers can be different,
    // n*_intra and n*_GB are evaluated separately.
    //
    // n_floor = 2 is retained only as a minimum atomistic/numerical bound,
    // not as the universal physical nucleus size.

    double n_crit_intra = n_floor;
    double n_crit_inter = n_floor;

    if (delta_mu > 0.0)
    {
        if (dN_intra_requested > 0.0 && dG_nucleation > 0.0)
            n_crit_intra = std::max(n_floor, 2.0 * dG_nucleation / delta_mu);

        if (dN_inter_requested > 0.0 && dG_nucleation_inter > 0.0)
            n_crit_inter = std::max(n_floor, 2.0 * dG_nucleation_inter / delta_mu);
    }

    // Each requested new nucleus removes n* atoms from the free matrix.
    // Therefore the mass associated with nucleation is now dynamically
    // coupled to T and supersaturation through the same CNT barrier used
    // in the nucleation rate.
    double massa_nucleata_intra_requested = n_crit_intra * dN_intra_requested;
    double massa_nucleata_inter_requested = n_crit_inter * dN_inter_requested;
    double massa_nucleata_tot_requested   = massa_nucleata_intra_requested + massa_nucleata_inter_requested;

    // 4. MASSA DISPONIBILE

    // La nucleazione può utilizzare solamente gli atomi presenti
    // nella matrice più quelli prodotti durante il timestep.
    double massa_disponibile = std::max(Cm_matrix_old + produzione, 0.0);

    // 5. LIMITAZIONE DELLA NUCLEAZIONE PER CONSERVAZIONE DELLA MASSA

    // Se intra + GB richiedono più atomi di quelli realmente disponibili,
    // entrambe le masse nucleate vengono ridotte proporzionalmente.
    //
    // Questo impedisce di creare più particelle di quante possano essere
    // fisicamente formate dall'inventario metallico disponibile.

    double nucleation_scale = 1.0;

    if (massa_nucleata_tot_requested > massa_disponibile && massa_nucleata_tot_requested > 0.0)
        nucleation_scale = massa_disponibile / massa_nucleata_tot_requested;

    double massa_nucleata_intra = massa_nucleata_intra_requested * nucleation_scale;
    double massa_nucleata_inter = massa_nucleata_inter_requested * nucleation_scale;

    // N finale viene ricalcolato dalla massa realmente trasferita.
    //
    // Since the two populations can have different critical nucleus sizes,
    // the actual number of nuclei formed is obtained by dividing the
    // transferred mass by the corresponding n*.
    //
    // If nucleation is mass-limited, this automatically reduces dN while
    // preserving the CNT nucleus size used for that timestep.
    double dN_intra_actual = 0.0;
    double dN_inter_actual = 0.0;

    if (n_crit_intra > 0.0)
        dN_intra_actual = massa_nucleata_intra / n_crit_intra;

    if (n_crit_inter > 0.0)
        dN_inter_actual = massa_nucleata_inter / n_crit_inter;

    double N_intra_final = N_iniziale_intra + dN_intra_actual;
    double N_inter_final = N_iniziale_inter + dN_inter_actual;

    // 6. THERMODYNAMICALLY DRIVEN PRECIPITATION + 3x3 SYSTEM FOR Cm

    // Nella formulazione precedente la precipitazione era proporzionale
    // direttamente alla concentrazione libera:
    //
    //      J_prec = k_prec * C_matrix
    //
    // Questa forma continua a rimuovere metallo dalla matrice anche quando
    // la concentrazione si avvicina alla solubilita' di equilibrio.
    //
    // Ora che il modello dispone di C_eq(T), la driving force per la crescita
    // dei precipitati viene espressa attraverso l'eccesso di soluto:
    //
    //      J_prec = k_prec * max(C_matrix - C_eq(T), 0)
    //
    // In questo modo:
    //
    //      C_matrix > C_eq(T)  -> precipitazione attiva
    //      C_matrix <= C_eq(T) -> precipitazione disattivata
    //
    // IMPORTANTE:
    // questa formulazione NON introduce dissoluzione termica quando
    // C_matrix < C_eq. La dissoluzione dei precipitati resta descritta
    // esclusivamente dal termine irradiation-induced re-solution k_res.
    //
    // Coerentemente con lo schema lagged gia' adottato, decidiamo se la
    // precipitazione e' attiva usando lo stato disponibile dopo produzione
    // e nucleazione, ma prima del solve implicito:
    //
    //      C_matrix_pre = C_matrix_old + produzione
    //                     - massa_nucleata_intra
    //                     - massa_nucleata_inter
    //
    // Se C_matrix_pre <= C_eq, entrambi i coefficienti di precipitazione
    // vengono posti a zero per il timestep corrente.

    double Cm_matrix_pre_solve = Cm_matrix_old + produzione - massa_nucleata_intra - massa_nucleata_inter;

    double precipitation_excess_old = std::max(Cm_matrix_pre_solve - C_eq, 0.0);

    bool precipitation_active = (precipitation_excess_old > 0.0);

    double k_intra_precip = precipitation_active ? k_intra : 0.0;
    double k_gb_precip    = precipitation_active ? k_gb : 0.0;

    // IMPLICIT 3x3 MASS BALANCE
    // Quando la precipitazione e' attiva:
    //
    // dCm_matrix/dt =
    // y*F
    // - k_intra * (Cm_matrix - C_eq)
    // - k_gb    * (Cm_matrix - C_eq)
    // + k_res * (Cm_prec_intra + Cm_prec_gb)
    //
    // dCm_prec_intra/dt =
    // + k_intra * (Cm_matrix - C_eq)
    // - k_res * Cm_prec_intra
    //
    // dCm_prec_gb/dt =
    // + k_gb * (Cm_matrix - C_eq)
    // - k_res * Cm_prec_gb
    //
    // Espandendo i termini di precipitazione:
    //
    //      k * (Cm_matrix - C_eq) = k*Cm_matrix - k*C_eq
    //
    // la matrice del sistema resta della stessa forma del modello precedente,
    // mentre i termini costanti +/- k*C_eq vengono aggiunti al RHS.
    //
    // La somma dei tre termini +/- k*C_eq e' nulla: il nuovo driving force
    // modifica solamente la redistribuzione della massa tra matrice e
    // precipitati e mantiene la conservazione globale.
    //
    // Quando la precipitazione non e' attiva:
    //
    //      k_intra_precip = k_gb_precip = 0
    //
    // e rimangono solamente nucleazione gia' trasferita esplicitamente
    // e irradiation-induced re-solution.

    double A_matrix[9] = {0.0};
    double b_matrix[3] = {0.0};

    A_matrix[0] = 1.0 + (k_intra_precip + k_gb_precip) * dt;
    A_matrix[1] = -k_res * dt;
    A_matrix[2] = -k_res * dt;

    A_matrix[3] = -k_intra_precip * dt;
    A_matrix[4] = 1.0 + k_res * dt;
    A_matrix[5] = 0.0;

    A_matrix[6] = -k_gb_precip * dt;
    A_matrix[7] = 0.0;
    A_matrix[8] = 1.0 + k_res * dt;

    b_matrix[0] = Cm_matrix_pre_solve + (k_intra_precip + k_gb_precip) * C_eq * dt;

    b_matrix[1] = Cm_precip_intra_old + massa_nucleata_intra - k_intra_precip * C_eq * dt;

    b_matrix[2] = Cm_precip_gb_old + massa_nucleata_inter - k_gb_precip * C_eq * dt;

    solver.Laplace3x3(A_matrix, b_matrix);

    // 7. CONTROLLO NUMERICO

    // Se il solver produce NaN o inf, stampiamo i parametri del timestep
    // per identificare la combinazione che causa l'instabilità.
    // Non applichiamo un clipping a zero dopo il solver per non nascondere
    // eventuali problemi fisici o numerici del sistema.

    if (!std::isfinite(b_matrix[0]) || !std::isfinite(b_matrix[1]) || !std::isfinite(b_matrix[2]))
        std::cerr << "ERROR MFP: non-finite concentration | T = " << T << " | dt = " << dt
                  << " | production = " << produzione << " | C_eq = " << C_eq
                  << " | C_matrix_pre = " << Cm_matrix_pre_solve << " | precipitation_active = " << precipitation_active
                  << " | S = " << supersaturation << " | DeltaMu = " << delta_mu << " | ncrit_intra = " << n_crit_intra
                  << " | ncrit_GB = " << n_crit_inter << " | k_intra = " << k_intra_precip
                  << " | k_gb = " << k_gb_precip << " | k_res = " << k_res << " | N_intra = " << N_intra_final
                  << " | N_inter = " << N_inter_final << std::endl;

    // 8. SALVATAGGIO DEI RISULTATI FINALI

    sciantix_variable["Cm matrix"].setFinalValue(b_matrix[0]);
    sciantix_variable["Cm precipitated intragranular"].setFinalValue(b_matrix[1]);
    sciantix_variable["Cm precipitated grain boundary"].setFinalValue(b_matrix[2]);

    sciantix_variable["Intragranular 5MPs concentration"].setFinalValue(N_intra_final);
    sciantix_variable["Intergranular 5MPs concentration"].setFinalValue(N_inter_final);

    // 9. n UPDATE - INTRAGRANULAR

    // Numero medio di atomi per particella:
    // n = Cm_precip / N

    double n_media_intra = 0.0;

    if (N_intra_final < 1.0)
        n_media_intra = n_floor;  // fallback numerico; non rappresenta n* del timestep
    else
        n_media_intra = b_matrix[1] / N_intra_final;

    sciantix_variable["Intragranular atom per 5MP"].setFinalValue(n_media_intra);

    // 10. n UPDATE - INTERGRANULAR

    double n_media_inter = 0.0;

    if (N_inter_final < 1.0)
        n_media_inter = n_floor;  // fallback numerico; non rappresenta n* del timestep
    else
        n_media_inter = b_matrix[2] / N_inter_final;

    sciantix_variable["Intergranular atom per 5MP"].setFinalValue(n_media_inter);
}