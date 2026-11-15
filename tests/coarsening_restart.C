// #COARSENING: separate-process native API restart check. No restart-specific model state
// is retained outside the public state/history/mode arrays.
#include "Initialization.h"
#include "InputReading.h"
#include "MainVariables.h"
#include "Sciantix.h"
#include "SourceHandler.h"
#include <fstream>
#include <stdexcept>
#include <string>
#include <type_traits>

template <class Stream, class T> void transfer(Stream& file, T& value)
{
    if constexpr (std::is_same_v<Stream, std::ifstream>)
        file.read(reinterpret_cast<char*>(&value), sizeof(value));
    else
        file.write(reinterpret_cast<const char*>(&value), sizeof(value));
    if (!file)
        throw std::runtime_error("Checkpoint I/O failed");
}
template <class Stream> void state(Stream& file)
{
    transfer(file, Sciantix_history);
    transfer(file, Sciantix_variables);
    transfer(file, Sciantix_diffusion_modes);
    transfer(file, Sciantix_diffusion_modes_NUS);
}
int main(int argc, char** argv)
{
    if (argc != 5)
        throw std::runtime_error("restart_driver input_dir start end checkpoint");
    TestPath        = std::string(argv[1]) + "/";
    const int start = std::stoi(argv[2]), end = std::stoi(argv[3]);
    InputReading(Sciantix_options,
                 Sciantix_variables,
                 Sciantix_scaling_factors,
                 Input_history_points,
                 Time_input,
                 Temperature_input,
                 Fissionrate_input,
                 Hydrostaticstress_input,
                 Steampressure_input,
                 Time_end_h,
                 Time_end_s,
                 sources_input,
                 initial_distribution);
    Initialization(Sciantix_history,
                   Sciantix_variables,
                   Sciantix_diffusion_modes,
                   Sciantix_diffusion_modes_NUS,
                   Temperature_input,
                   Fissionrate_input,
                   Hydrostaticstress_input,
                   Steampressure_input);
    sources_interp  = sourceInterpolation(sources_input, 100);
    sources_correct = subtractResolutionFromSource(sources_interp);
    if (start)
    {
        std::ifstream checkpoint(argv[4], std::ios::binary);
        state(checkpoint);
    }
    auto time        = [](int i) { return i <= 50 ? i * 1e6 : 50e6 + (i - 50) * 2.; };
    auto temperature = [](int i) {
        return i <= 50 ? 1176. : i <= 100 ? 1176. + (i - 50) * 18.08 : i <= 150 ? 2080. : 2080. - (i - 150) * 24.;
    };
    for (int i = start; i <= end; ++i)
    {
        Sciantix_history[0] = Sciantix_history[1];
        Sciantix_history[1] = temperature(i);
        Sciantix_history[2] = Sciantix_history[3];
        Sciantix_history[3] = 4.14e18;
        Sciantix_history[4] = Sciantix_history[5];
        Sciantix_history[5] = -.2;
        Sciantix_history[6] = i ? time(i) - time(i - 1) : 0.;
        Time_s              = time(i);
        Time_h              = Time_s / 3600.;
        Time_step_number    = i;
        Sciantix_history[7] = Time_h;
        Sciantix_history[8] = i;
        Sciantix(Sciantix_options,
                 Sciantix_history,
                 Sciantix_variables,
                 Sciantix_scaling_factors,
                 Sciantix_diffusion_modes,
                 Sciantix_diffusion_modes_NUS);
    }
    std::ofstream checkpoint(argv[4], std::ios::binary);
    state(checkpoint);
}
