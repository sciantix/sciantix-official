#include "IntraGranularBubbleCoarsening.h"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <limits>
#include <stdexcept>

// #COARSENING: physical invariants and convergence, not implementation snapshots.
void require(bool ok, const char* message)
{
    if (!ok)
        throw std::runtime_error(message);
}
void close(double x, double y, double tolerance, const char* message)
{
    require(std::abs(x - y) <= tolerance * std::max({1., std::abs(x), std::abs(y)}), message);
}
int main(int argc, char** argv)
{
    using namespace coarsening;
    using dislocation_density::Model;
    if (argc > 1 && std::string(argv[1]) == "density-grid")
    {
        std::cout.precision(17);
        std::cout << "model,B,T,rho\n";
        for (int model = 0; model < 2; ++model)
            for (double B : {0., 5., 20., 100.})
                for (double T : {800., 900., 1200., 1800., 2400.})
                    std::cout << model << ',' << B << ',' << T << ','
                              << dislocation_density::correlation(B, T, static_cast<Model>(model)) << '\n';
        return 0;
    }
    for (double T : {800., 1000., 1875., 2400.})
    {
        const auto fixed = dislocation_density::response(20., T, Model::Barani2020);
        require(fixed.density == 4e13 && fixed.dBurnup == 0. && fixed.dTemperature == 0., "Constant density option");
        const auto   fit = dislocation_density::response(20., T, Model::Nicodemo2026);
        const double h   = .001;
        close(fit.dTemperature,
              (dislocation_density::correlation(20., T + h) - dislocation_density::correlation(20., T - h)) / (2 * h),
              1e-6,
              "Full thermal density derivative");
        close(fit.dBurnup,
              (dislocation_density::correlation(20. + h, T) - dislocation_density::correlation(20. - h, T)) / (2 * h),
              1e-6,
              "Full burnup density derivative");
    }
    require(thermalKFraction(1800.) == 0. && thermalKFraction(1950.) == 1. && thermalKFraction(1875.) == .5,
            "Compact thermal K");
    close(thermalKFractionDerivative(1850.),
          (thermalKFraction(1850.001) - thermalKFraction(1849.999)) / .002,
          1e-9,
          "Thermal K derivative");
    Parameters  p;
    Environment cold{1176., 4.14e18, -.2e6, 8.55e-6, 20.};
    // The hard-sphere correction raises dense-gas pressure and tends to the
    // ideal-gas limit at low packing. Uptake must use the same EOS as diagnostics.
    auto eos                   = initialState();
    eos.largeNumber            = 1e18;
    eos.gas[2][0]              = 1e24;
    eos.vacancies              = 1e24;
    auto eosEnvironment        = cold;
    eosEnvironment.temperature = 2080.;
    const auto   dense         = observe(eos, eosEnvironment, p);
    const double ideal         = eos.gas[2][0] * 1.380649e-23 * eosEnvironment.temperature / dense.largeSwelling;
    require(dense.pressure > ideal, "Carnahan-Starling dense-gas pressure exceeds ideal pressure");
    eosEnvironment.stress = 1.4 / dense.largeRadius - .5 * (dense.pressure + ideal);
    require(vacancyRate(1e6, 1e6, eos.largeNumber, eosEnvironment, p) > 0.,
            "Vacancy uptake uses the Carnahan-Starling overpressure");
    eosEnvironment.stress = 1.4 / dense.largeRadius - 2 * dense.pressure;
    require(vacancyRate(1e6, 1e6, eos.largeNumber, eosEnvironment, p) == 0.,
            "No vacancy uptake below the Carnahan-Starling equilibrium pressure");
    auto dilute              = eos;
    dilute.largeNumber       = 1e9;
    dilute.gas[2][0]         = 1e15;
    const auto   lowPacking  = observe(dilute, cold, p);
    const double diluteIdeal = dilute.gas[2][0] * 1.380649e-23 * cold.temperature / lowPacking.largeSwelling;
    close(lowPacking.pressure / diluteIdeal, 1., 3e-9, "Carnahan-Starling dilute limit");
    auto gasOnly                    = eos;
    gasOnly.vacancies               = 0.;
    const auto   gasOnlyObservation = observe(gasOnly, cold, p);
    const double gasOnlyIdeal = gasOnly.gas[2][0] * 1.380649e-23 * cold.temperature / gasOnlyObservation.largeSwelling;
    close(gasOnlyObservation.pressure / gasOnlyIdeal,
          83. / 27.,
          1e-14,
          "Gas co-volume fixes zero-vacancy packing to one quarter");
    auto packed         = eos;
    packed.vacancies    = -.9 * gasCoVolume(cold.temperature) * packed.gas[2][0] / vacancyVolume;
    bool invalidPacking = false;
    try
    {
        observe(packed, cold, p);
    }
    catch (const std::runtime_error&)
    {
        invalidPacking = true;
    }
    require(invalidPacking, "Invalid Carnahan-Starling packing is rejected without clipping");
    const double bulk = diffusivity(cold), capture = captureDiffusivity(cold, p);
    const double pipe = 7.74e-6 * std::exp(-1.21e-19 / (1.380649e-23 * cold.temperature));
    require(capture >= bulk && capture <= pipe, "Hart mixture bounds");
    auto scaled                      = p;
    scaled.lineDiffusivityMultiplier = 2.;
    close(captureDiffusivity(cold, scaled) / capture, 2., 1e-14, "Independent gas mobility scale");
    scaled = p;
    scaled.vacancyDiffusivityMultiplier *= 2.;
    close(vacancyDiffusivity(2080., scaled) / vacancyDiffusivity(2080., p),
          2.,
          1e-14,
          "Independent vacancy mobility scale");
    auto s = initialState();
    evolve(s, 1000., cold, cold, p);
    require(s.bulkNumber > 0. && s.largeNumber == 0. && average(s)[2] == 0.,
            "Native bulk limit before thermal recruitment");
    const auto m = average(s);
    close(m[0] + m[1] + m[2] + s.diffusedGas, s.projectedProduction, 1e-11, "Accepted source and escape balance");
    auto seeded         = s;
    seeded.vacancies    = 1e20;
    const double before = average(seeded)[1];
    recruit(seeded, 1e19);
    const auto after = average(seeded);
    close(after[1] + after[2], before, 1e-13, "Recruitment transfers existing gas");
    close(seeded.bulkNumber + seeded.largeNumber, s.bulkNumber, 1e-13, "Recruitment reclassifies existing bubbles");
    require(seeded.vacancies == 1e20, "Recruitment creates no vacancies");
    // Pure growth-driven merging at F=0 and zero vacancy mobility must retain Qv.
    auto merging                                 = initialState();
    merging.bulkNumber                           = 1e24;
    merging.largeNumber                          = 1e19;
    merging.activeFraction                       = 1.;
    merging.vacancies                            = 1e24;
    merging.gas[0][0]                            = 1e25;
    merging.gas[2][0]                            = 1e23;
    auto mergeParameters                         = p;
    mergeParameters.K                            = 0.;
    mergeParameters.vacancyDiffusivityMultiplier = 0.;
    auto anneal                                  = cold;
    anneal.temperature                           = 2080.;
    anneal.fissionRate                           = 0.;
    evolve(merging, 1000., anneal, anneal, mergeParameters);
    require(merging.largeNumber < 1e19, "Growth causes coalescence");
    require(merging.vacancies == 1e24, "Pure coalescence conserves vacancy inventory");
    // With no source, return, trapping or vacancy uptake, cooling changes only
    // the gas co-volume and number through overlap; gas and vacancies are conserved.
    auto thermal             = initialState();
    thermal.largeNumber      = 1e19;
    thermal.gas[2][0]        = 1e25;
    thermal.vacancies        = 1e24;
    const auto thermalBefore = observe(thermal, anneal, mergeParameters);
    auto       cooled        = anneal;
    cooled.temperature       = 573.;
    evolve(thermal, 60., anneal, cooled, mergeParameters);
    const auto thermalAfter = observe(thermal, cooled, mergeParameters);
    require(average(thermal)[2] == 1e25 && thermal.vacancies == 1e24,
            "Temperature-dependent co-volume conserves atomic and vacancy inventories");
    require(thermalAfter.largeSwelling > thermalBefore.largeSwelling && thermal.largeNumber < 1e19,
            "Thermal volume growth enters swelling and coalescence consistently");
    const double cooledNumber = thermal.largeNumber;
    evolve(thermal, 60., cooled, anneal, mergeParameters);
    require(thermal.largeNumber == cooledNumber, "Thermal contraction alone does not cause coalescence");
    auto hot        = cold;
    hot.temperature = 2080.;
    hot.burnup      = 20.0001;
    auto coarse = s, fine = s;
    auto tighter              = p;
    tighter.relativeTolerance = 2e-6;
    evolve(coarse, 60., cold, hot, p);
    evolve(fine, 60., cold, hot, tighter);
    const auto oc = observe(coarse, hot, p), of = observe(fine, hot, tighter);
    close(oc.largeRadius, of.largeRadius, 1e-12, "Radius tolerance refinement");
    close(coarse.largeNumber / fine.largeNumber, 1., .002, "Number tolerance refinement");
    close(oc.largeSwelling / of.largeSwelling, 1., .002, "Swelling tolerance refinement");
    const double births = coarse.cumulativeBirths;
    evolve(coarse, 60., hot, cold, p);
    evolve(coarse, 60., cold, hot, p);
    require(coarse.activeFraction == 1. && coarse.cumulativeBirths == births,
            "No cooling or repeated-temperature reseeding");
    auto off     = initialState();
    auto noSites = p;
    noSites.K    = 0.;
    evolve(off, 60., cold, hot, noSites);
    require(off.largeNumber == 0. && average(off)[2] == 0., "Zero K scale disables recruitment conservatively");
    bool rejected = false;
    try
    {
        auto invalid = p;
        invalid.K    = -1.;
        evolve(off, 1., cold, hot, invalid);
    }
    catch (const std::invalid_argument&)
    {
        rejected = true;
    }
    require(rejected, "Invalid scaling rejected");
    std::cout << "Coarsening EOS, conservation, access memory, density and refinement passed\n";
}
