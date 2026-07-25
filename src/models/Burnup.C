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

#include "MainVariables.h"
#include "Simulation.h"
#include "SourceHandler.h"

void Simulation::Burnup()
{
    // Model declaration
    Model model_;
    model_.setName("Burnup");

    double fissionRate = history_variable["Fission rate"].getFinalValue();
    double fuelDensity = sciantix_variable["Fuel density"].getFinalValue();

    // With the non-uniform-source solver the fission rate entering the burnup is the
    // volume average of the radial source profile, not the history value.
    double fR = fissionRate;
    if (int(input_variable["iDiffusionSolver"].getValue()) == 4 && Time_step_number >= 0 &&
        static_cast<size_t>(Time_step_number) < sources_interp.size())
    {
        fR = Source_Volume_Average(sciantix_variable["Grain radius"].getFinalValue(), sources_interp[Time_step_number]);
    }

    double specificPower = fR * 3.12e-17 / fuelDensity;

    double burnup = specificPower / 86400.0;  // specific power in MW/kg, burnup in MWd/kg
    sciantix_variable["Specific power"].setFinalValue(specificPower);

    std::vector<double> parameter;
    parameter.push_back(burnup);

    std::string reference = ": The local burnup is calculated from the fission rate density.";

    model_.setParameter(parameter);
    model_.setRef(reference);
    model.push(model_);

    // Model resolution
    sciantix_variable["Burnup"].setFinalValue(solver.Integrator(sciantix_variable["Burnup"].getInitialValue(),
                                                                model["Burnup"].getParameter().at(0),
                                                                physics_variable["Time step"].getFinalValue()));

    if (fR > 0.0)
        sciantix_variable["Irradiation time"].setFinalValue(
            solver.Integrator(sciantix_variable["Irradiation time"].getInitialValue(),
                              1.0 / sciantix_variable["Specific power"].getFinalValue(),
                              24.0 * sciantix_variable["Burnup"].getIncrement()));
    else
        sciantix_variable["Irradiation time"].setConstant();

    if (sciantix_variable["U"].getFinalValue() > 0.0)
        sciantix_variable["FIMA"].setFinalValue(solver.Integrator(sciantix_variable["FIMA"].getInitialValue(),
                                                                  fR * 3.6e5 / sciantix_variable["U"].getFinalValue(),
                                                                  sciantix_variable["Irradiation time"].getIncrement()));
    else
        sciantix_variable["FIMA"].setConstant();
}
