#ifndef SCIANTIX_UN_L3_MODEL_H
#define SCIANTIX_UN_L3_MODEL_H

#include "UNIntraGranular.h"
#include <string>

class Solver;
namespace un_l3
{
// Complete L3 model owned by Simulation. The legacy scalar/mode arrays are
// only compatibility outputs; they never reconstruct or overwrite this state.
class Model
{
  public:
    Model(const double history[], const double variables[], const double scaling[]);
    void execute(Solver& solver, const double history[]);
    void exportVariables(double variables[]) const;
    void output(const std::string& directory);
    const State& state() const { return state_; }
    static double nominalTimeStep() { return 10800.0; }

  private:
    Parameters parameters_;
    PhysicsOptions options_;
    State state_;
    IntraGranularDiagnostics diagnostics_;
    std::map<std::string,double> history_values_;
    std::size_t step_count_ = 0;
    double dt_ = 0.0;
    double capture_fraction_sum_ = 0.0, capture_raw_sum_ = 0.0;
    double captured_bubbles_ = 0.0, max_capture_ = 0.0;
    void refreshHistory();
};
}
#endif
