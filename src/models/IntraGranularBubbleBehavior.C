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
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"
#include "MainVariables.h"
#include "IntraGranularBubbleBehavior.h"

void Simulation::IntraGranularBubbleBehavior()
{

    // Model declaration
    Model model_;

    model_.setName("Intragranular bubble behavior");

    std::string         reference;
    std::vector<double> parameter;

    switch (int(input_variable["iIntraGranularBubbleBehavior"].getValue()))
    {
        case 0:
        {
            reference += ": Constant bubble concentration and radius.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(7.0e23);
            sciantix_variable["Intragranular bubble radius"].setInitialValue(1.0e-9);

            sciantix_variable["Intragranular bubble concentration"].setFinalValue(7.0e23);
            sciantix_variable["Intragranular bubble radius"].setFinalValue(1.0e-9);

            parameter.push_back(0.);
            parameter.push_back(0.);

            break;
        }

        case 5: // # COARSENING: bulk-number equation followed by the two-population model.
        case 1:
        {
            reference += ": Pizzocri et al., JNM, 502 (2018) 323-330.";

            parameter.push_back(sciantix_system[0].getResolutionRate());
            parameter.push_back(sciantix_system[0].getNucleationRate());

            break;
        }

        case 2:
        {
            reference += "White and Tucker, JNM, 118 (1983), 1-38.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(
                1.52e+27 / history_variable["Temperature"].getFinalValue() - 3.3e+23);
            parameter.push_back(0.0);
            parameter.push_back(0.0);

            break;
        }

        case 3:
        {
            reference += "Case specific for annealing experiments and helium intragranular behaviour.";

            if (physics_variable["Time step"].getFinalValue() > 0.0)
                parameter.push_back((1.0 / sciantix_variable["Intragranular similarity ratio"].getFinalValue() - 1.0) /
                                    physics_variable["Time step"].getFinalValue());
            else
                parameter.push_back(0.);

            parameter.push_back(0.);

            break;
        }

        case 99:
        {
            reference += "No intragranular bubbles.";

            sciantix_variable["Intragranular bubble concentration"].setInitialValue(0.0);
            sciantix_variable["Intragranular bubble radius"].setInitialValue(0.0);
            sciantix_variable["Intragranular atoms per bubble"].setInitialValue(0.0);

            sciantix_variable["Intragranular bubble concentration"].setFinalValue(0.0);
            sciantix_variable["Intragranular bubble radius"].setFinalValue(0.0);
            sciantix_variable["Intragranular atoms per bubble"].setFinalValue(0.0);

            parameter.push_back(0.);
            parameter.push_back(0.);

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__,
                                  "iIntraGranularBubbleBehavior",
                                  int(input_variable["iIntraGranularBubbleBehavior"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    // dN / dt = - getParameter().at(0) * N + getParameter().at(1)
    sciantix_variable["Intragranular bubble concentration"].setFinalValue(
        solver.Decay(sciantix_variable["Intragranular bubble concentration"].getInitialValue(),
                     model["Intragranular bubble behavior"].getParameter().at(0),
                     model["Intragranular bubble behavior"].getParameter().at(1),
                     physics_variable["Time step"].getFinalValue()));

    // Atom per bubbles and bubble radius
    for (auto& system : sciantix_system)
    {
        if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
        {
            if (sciantix_variable["Intragranular bubble concentration"].getFinalValue() > 0.0)
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].setFinalValue(
                    sciantix_variable[system.getGasName() + " in intragranular bubbles"].getFinalValue() /
                    sciantix_variable["Intragranular bubble concentration"].getFinalValue());

            else
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].setFinalValue(0.0);

            sciantix_variable["Intragranular bubble volume"].addValue(
                system.getVolumeInLattice() *
                sciantix_variable["Intragranular " + system.getGasName() + " atoms per bubble"].getFinalValue());
        }
    }

    // Intragranular bubble radius
    sciantix_variable["Intragranular bubble radius"].setFinalValue(
        0.620350491 * pow(sciantix_variable["Intragranular bubble volume"].getFinalValue(), (1.0 / 3.0)));

    // Intragranular gaseous swelling
    // 4/3 pi N R^3
    sciantix_variable["Intragranular gas bubble swelling"].setFinalValue(
        4.188790205 * pow(sciantix_variable["Intragranular bubble radius"].getFinalValue(), 3) *
        sciantix_variable["Intragranular bubble concentration"].getFinalValue());

    // # COARSENING: evolve the dislocation bubble population.
    if (int(input_variable["iIntraGranularBubbleBehavior"].getValue()) == 5)
        IntraGranularBubbleCoarsening();

    if (sciantix_variable["He in intragranular bubbles"].getInitialValue() > 0.0)
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(
            sqrt(sciantix_variable["He in intragranular bubbles"].getFinalValue() /
                 sciantix_variable["He in intragranular bubbles"].getInitialValue()));
    else
        sciantix_variable["Intragranular similarity ratio"].setFinalValue(0.0);
}

// # COARSENING: dislocation-bubble population of the two-population model (iIntraGranularBubbleBehavior = 5).
void Simulation::IntraGranularBubbleCoarsening()
{
    using namespace intragranular_coarsening;
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
    intragranular_nucleation::Packet birth;
    birth.increment=intragranular_nucleation::densityIncrement(
        sciantix_variable["Burnup"].getInitialValue(),sciantix_variable["Burnup"].getFinalValue(),T,F,dt);
    // rho_d from the DislocationDensity model (same step).
    intragranular_nucleation::state().rho_sites=intragranular_nucleation::connected()?
        sciantix_variable["Dislocation density"].getFinalValue():intragranular_nucleation::initial_density;
    const double pipe_density=intragranular_nucleation::connected()?intragranular_nucleation::state().rho_sites:intragranular_nucleation::initial_density;
    birth.old_number=Nd;
    birth.old_bulk_number=positive(sciantix_variable["Intragranular bubble concentration"].getFinalValue());
    const double old_vacancies=positive(initial("Coarsening vacancies per bubble"));
    birth.old_vacancy_total=Nd*old_vacancies;
    birth.requested=intragranular_nucleation::requestedBirths(birth.increment,birth.old_bulk_number,
                         positive(sciantix_variable["Intragranular bubble radius"].getFinalValue()),
                         birth.old_bulk_number>0?(M-md)/birth.old_bulk_number:0);
    if (birth.requested>0) {
        if(intragranular_nucleation::capture()) {
            const double mean_gas=birth.old_bulk_number>0?(M-md)/birth.old_bulk_number:0;
            const double probability=intragranular_nucleation::stableCapture()?intragranular_nucleation::stableProbability(mean_gas):1.;
            const double transferred_mean=intragranular_nucleation::stableCapture()?intragranular_nucleation::stableMean(mean_gas):mean_gas;
            birth.born=std::min(birth.requested,birth.old_bulk_number*probability);
            if(transferred_mean>0)birth.born=std::min(birth.born,(M-md)/transferred_mean);
            birth.gas=intragranular_nucleation::stableCapture()?transferred_mean*birth.born:
                (birth.old_bulk_number>0?(M-md)*birth.born/birth.old_bulk_number:0);
            // Existing bulk embryos are reclassified: no phase transfer or gas creation.
            set("Intragranular bubble concentration",birth.old_bulk_number-birth.born);
            md+=birth.gas;
        } else {
            double available=0;
            for(const std::string gas:{"Xe","Kr"})
                available+=positive(sciantix_variable[gas+" in intragranular solution"].getFinalValue());
            birth.born=std::min(birth.requested,available/intragranular_nucleation::embryo_atoms);
            const double fraction=available>0?intragranular_nucleation::embryo_atoms*birth.born/available:0;
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
    const double mixed_vacancies=intragranular_nucleation::mixedVacancies(birth.old_number,old_vacancies,birth.born);
    birth.birth_number=Nd;
    birth.birth_vacancy_total=Nd*mixed_vacancies;
    intragranular_nucleation::state().cumulative_births+=birth.born;

    const double Nb = positive(sciantix_variable["Intragranular bubble concentration"].getFinalValue());
    const double rb = positive(sciantix_variable["Intragranular bubble radius"].getFinalValue());
    const double rd0 = positive(initial("Coarsening dislocation radius"));
    const double D = sciantix_system[0].getFissionGasDiffusivity() * sciantix_system[0].getGas().getPrecursorFactor();
    const double line = lineTrapping(T, D);
    hart_trace::write(Time_h,T,D,pipe_density,line);
    const double bd = 4 * pi * D * std::max(rd0, rb) * Nd + line;
    const double alpha = resolution(rd0, F) * scaling_factors["Resolution rate"].getValue();

    // # COARSENING: gas in dislocation bubbles from Eq. (1), dm_d/dt = (beta_d + beta') c - alpha_d m_d, with c the
    // dissolved gas (Solver::Decay); the bulk bubbles hold the rest of the solver bubble gas. beta_d + beta' also
    // enters the trapping of the dissolved gas in System (next step). Replaces the relaxation to the fraction
    // bd / (bb + bd) of the bubble gas, which was not Eq. (1).
    double dissolved = 0;
    for (const std::string gas : {"Xe", "Kr"})
        dissolved += positive(sciantix_variable[gas + " in intragranular solution"].getFinalValue());
    set("Coarsening dislocation trapping rate", bd);
    if (dt > 0)
        md = std::clamp(solver.Decay(md, alpha, bd * dissolved, dt), 0., M);
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
            Nd = solver.BinaryInteraction(Nd, 4 * lambda, positive(vd - old_v));
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
            <<sciantix_variable["Burnup"].getFinalValue()*intragranular_nucleation::oxide_to_heavy_metal<<','
            <<pipe_density<<','<<birth.increment<<','<<(dt>0?birth.requested/dt:0)<<','<<(dt>0?birth.born/dt:0)<<'\n';
    }
    intragranular_nucleation::trace(Time_h,T,sciantix_variable["Burnup"].getFinalValue(),pipe_density,birth,Nd,rd);

}
