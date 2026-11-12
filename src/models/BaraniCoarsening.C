#include "HartTrace.h"
#include "NucleationModels.h"
#include "Simulation.h"
#include "MainVariables.h"
#include "BaraniCoarsening.h"

// # COARSENING: selected Barani 2020 reference, with one-off dislocation nucleation.
void Simulation::BaraniCoarsening()
{
    using namespace barani_coarsening;
    const double dt = physics_variable["Time step"].getFinalValue();
    const double T = history_variable["Temperature"].getFinalValue();
    const double F = history_variable["Fission rate"].getFinalValue();
    const double stress = history_variable["Hydrostatic stress"].getFinalValue();
    auto initial = [&](const std::string& name) { return sciantix_variable[name].getInitialValue(); };
    auto set = [&](const std::string& name, double value) { sciantix_variable[name].setFinalValue(value); };

    const auto population = initializePopulation(positive(initial("Coarsening dislocation concentration")),
                                                initial("Coarsening initialized") > 0, F);
    double Nd = population.number;
    double M = 0;
    for (const std::string gas : {"Xe", "Kr"})
        M += positive(sciantix_variable[gas + " in intragranular bubbles"].getFinalValue());
    double md = std::min(positive(initial("Xe coarsening partition state")) +
                         positive(initial("Kr coarsening partition state")), M);

    // # COARSENING: new-length-associated births, without reseeding the old population.
    nucleation_tests::Packet birth;
    birth.increment=nucleation_tests::densityIncrement(
        sciantix_variable["Burnup"].getInitialValue(),sciantix_variable["Burnup"].getFinalValue(),T,F,dt);
    nucleation_tests::state().rho_sites=nucleation_tests::connected()?
        nucleation_tests::densityCurve(sciantix_variable["Burnup"].getFinalValue()*nucleation_tests::oxide_to_heavy_metal,T):
        nucleation_tests::initial_density;
    const double pipe_density=nucleation_tests::connected()?nucleation_tests::state().rho_sites:nucleation_tests::initial_density;
    birth.old_number=Nd;
    birth.old_bulk_number=positive(sciantix_variable["Intragranular bubble concentration"].getFinalValue());
    const double old_vacancies=positive(initial("Coarsening vacancies per bubble"));
    birth.old_vacancy_total=Nd*old_vacancies;
    birth.requested=nucleation_tests::requestedBirths(birth.increment,birth.old_bulk_number,
                         positive(sciantix_variable["Intragranular bubble radius"].getFinalValue()),
                         birth.old_bulk_number>0?(M-md)/birth.old_bulk_number:0);
    if (birth.requested>0) {
        if(nucleation_tests::capture()) {
            const double mean_gas=birth.old_bulk_number>0?(M-md)/birth.old_bulk_number:0;
            const double probability=nucleation_tests::stableCapture()?nucleation_tests::stableProbability(mean_gas):1.;
            const double transferred_mean=nucleation_tests::stableCapture()?nucleation_tests::stableMean(mean_gas):mean_gas;
            birth.born=std::min(birth.requested,birth.old_bulk_number*probability);
            if(transferred_mean>0)birth.born=std::min(birth.born,(M-md)/transferred_mean);
            birth.gas=nucleation_tests::stableCapture()?transferred_mean*birth.born:
                (birth.old_bulk_number>0?(M-md)*birth.born/birth.old_bulk_number:0);
            // Existing bulk embryos are reclassified: no phase transfer or gas creation.
            set("Intragranular bubble concentration",birth.old_bulk_number-birth.born);
            md+=birth.gas;
        } else {
            double available=0;
            for(const std::string gas:{"Xe","Kr"})
                available+=positive(sciantix_variable[gas+" in intragranular solution"].getFinalValue());
            birth.born=std::min(birth.requested,available/nucleation_tests::embryo_atoms);
            const double fraction=available>0?nucleation_tests::embryo_atoms*birth.born/available:0;
            for(const std::string gas:{"Xe","Kr"}) {
                const double solution=positive(sciantix_variable[gas+" in intragranular solution"].getFinalValue());
                const double transfer=solution*fraction;
                double* cm=getDiffusionModesSolution(gas);
                double* bm=getDiffusionModesBubbles(gas);
                for(int j=0;j<n_modes;++j) {
                    const double total=cm[j]+bm[j];
                    const double moved=cm[j]*fraction;
                    cm[j]-=moved;bm[j]+=moved;
                    birth.mode_balance_error=std::max(birth.mode_balance_error,
                        std::abs(cm[j]+bm[j]-total)/(1+std::abs(total)+std::abs(cm[j])+std::abs(bm[j])));
                }
                set(gas+" in intragranular solution",solution-transfer);
                set(gas+" in intragranular bubbles",sciantix_variable[gas+" in intragranular bubbles"].getFinalValue()+transfer);
                birth.gas+=transfer;
            }
            M+=birth.gas;md+=birth.gas;
        }
        Nd+=birth.born;
    }
    const double mixed_vacancies=nucleation_tests::mixedVacancies(birth.old_number,old_vacancies,birth.born);
    birth.birth_number=Nd;
    birth.birth_vacancy_total=Nd*mixed_vacancies;
    nucleation_tests::state().cumulative_births+=birth.born;

    const double Nb = positive(sciantix_variable["Intragranular bubble concentration"].getFinalValue());
    const double rb = positive(sciantix_variable["Intragranular bubble radius"].getFinalValue());
    const double rd0 = positive(initial("Coarsening dislocation radius"));
    const double D = sciantix_system[0].getFissionGasDiffusivity() * sciantix_system[0].getGas().getPrecursorFactor();
    const double bb = 4 * pi * D * rb * Nb;
    const double line = lineTrapping(T, D);
    hart_trace::write(Time_h,T,D,pipe_density,line);
    const double bd = 4 * pi * D * std::max(rd0, rb) * Nd + line;
    const double alpha = resolution(rd0, F) * scaling_factors["Resolution rate"].getValue();

    md = partition(md, M, bb, bd, alpha, dt);
    // # COARSENING: phi=1/(n-1) enters the bubble-number loss only.
    if (Nd > 0)
        Nd = solver.Decay(Nd, alpha * numberCorrection(md / Nd), 0, dt);
    double n = Nd > 0 ? md / Nd : 0;
    double nv = mixed_vacancies;
    double vd = bubbleVolume(n, nv, T);
    double rd = radius(vd);
    if (Nd > 0 && n > 0)
    {
        const double old_v = vd;
        const double old_r = rd;
        nv = uptake(nv, n, Nd, T, stress, dt);
        vd = bubbleVolume(n, nv, T);
        rd = radius(vd);
        if (dt > 0 && pressure(n, old_v, T) > equilibrium(old_r, stress))
        {
            const double porosity = std::min(Nd * vd, .95);
            const double lambda = (2 - porosity) / (2 * std::pow(1 - porosity, 3));
            Nd /= 1 + 4 * lambda * Nd * positive(vd - old_v);
            const double capture = std::min(Nd * 4 * pi * std::pow(rd + rb, 2) * positive(rd - rd0), 1.);
            md = std::clamp(md + (M - md) * capture, 0., M);
            n = Nd > 0 ? md / Nd : 0;
            vd = bubbleVolume(n, nv, T);
            rd = radius(vd);
        }
    }

    const double mb = positive(M - md);
    const double nb = Nb > 0 ? mb / Nb : 0;
    set("Intragranular bubble radius", radius(nb * atomic_volume));
    set("Intragranular bubble volume", nb * atomic_volume);
    set("Intragranular atoms per bubble", nb);
    set("Coarsening dislocation concentration", Nd);
    set("Coarsening vacancy concentration", Nd * nv);
    set("Coarsening initialized", population.initialized ? 1 : 0);
    set("Coarsening dislocation radius", rd);
    set("Coarsening vacancies per bubble", nv);
    set("Coarsening pressure", pressure(n, vd, T));
    set("Coarsening equilibrium pressure", equilibrium(rd, stress));
    set("Coarsening dislocation swelling", Nd * vd);
    set("Coarsening bulk swelling", mb * atomic_volume);
    set("Intragranular gas bubble swelling", Nd * vd + mb * atomic_volume);
    set("Coarsening line trapping rate", line);
    for (const std::string gas : {"Xe", "Kr"})
    {
        const double species = sciantix_variable[gas + " in intragranular bubbles"].getFinalValue();
        const double fraction = M > 0 ? md / M : 0;
        set(gas + " coarsening partition state", species * fraction);
        set(gas + " in dislocation bubbles", species * fraction);
        set(gas + " in bulk bubbles", species * (1 - fraction));
    }
    const char* ratefile=std::getenv("SCIANTIX_NU_RATE_TRACE");
    if(ratefile) {
        static std::ofstream rates(ratefile);static bool first=true;
        if(first)rates<<"time_h,dt_s,T_K,B_HM,rho_fit,delta_rho,nu_requested_m3s,nu_actual_m3s\n";
        first=false;rates<<std::setprecision(17)<<Time_h<<','<<dt<<','<<T<<','
            <<sciantix_variable["Burnup"].getFinalValue()*nucleation_tests::oxide_to_heavy_metal<<','
            <<pipe_density<<','<<birth.increment<<','<<(dt>0?birth.requested/dt:0)<<','<<(dt>0?birth.born/dt:0)<<'\n';
    }
    nucleation_tests::trace(Time_h,T,sciantix_variable["Burnup"].getFinalValue(),pipe_density,birth,Nd,rd);

}
