#pragma once
// #COARSENING: density options only; no alternative coarsening implementation.
namespace dislocation_density
{
    enum class Model
    {
        Barani2020   = 0,
        Nicodemo2026 = 1
    };
    constexpr double oxide_to_heavy_metal = (238.02891 + 2 * 15.9994) / 238.02891;
    struct Response
    {
        double density, dBurnup, dTemperature;
    };
    double   correlation(double burnup, double temperature, Model model = Model::Nicodemo2026);
    Response response(double burnup, double temperature, Model model = Model::Nicodemo2026);
    double   timeDerivative(double burnup, double temperature, double burnupRate, double temperatureRate, Model model);
    double   intervalRate(double B0, double T0, double B1, double T1, double seconds, Model model);
}  // namespace dislocation_density
