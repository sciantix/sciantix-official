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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

void Simulation::GrainVaporisation()
{
    double T =  history_variable["Temperature"].getFinalValue();
    double x = sciantix_variable["Stoichiometry deviation"].getFinalValue();
    double p_sys = history_variable["THERMOCHIMICA pressure"].getFinalValue(); //Pa

    double molarmassuo =  254.02e-3; // kg/mol
    double molarmassuo2 = 270.03e-3; // kg/mol
    double molarmassuo3 = 286.03e-3; // kg/mol
    double molarmasso2 = 16.00e-3; // kg/mol

    double fuelporosity = 1 - sciantix_variable["Fuel density"].getFinalValue()/matrices["UO2"].getTheoreticalDensity();

    // Constants form Olander (Blackburn's model)
    double p_uo = exp(- 49.5/(T/1000) + 11.9);
    double p_uo2 = exp(- 74.0/(T/1000) + 19.9);
    double p_uo3 = exp(- 44.0/(T/1000) + 11.9);

    double p_o2 = 0.0;
    double molarvolume =  fuelporosity *(molarmassuo2/sciantix_variable["Fuel density"].getFinalValue()); //HP of same volume removed

    if (x < 0)
    {
        p_uo *= pow(2 + x, 1) * x;
        p_uo2 *= pow(2 + x, 2) * (1 - x);
        p_uo3 *= pow(2 + x, 3) * 0;
        p_o2 = exp(78.3 * 1e3/T + 13.6) * pow(x / ((2 + x)*(1 - x)), -1);
    }
    else
    {
        p_uo *= pow(2 - x, 1) * 0;
        p_uo2 *= pow(2 - x, 2) * (1 - x);
        p_uo3 *= pow(2 - x, 3) * x;
        p_o2 = exp(- 16.4 * 1e3/T + 5) * pow( (1 - x) / ((2 + x) * x), -1);
    }

    // double dg_uo2 = 567000 - T * 150; //J/mol
    // double p_uo2 = exp( - dg_uo2/ (gas_constant * T));

    double flux_u = p_sys * pow(2 * M_PI * gas_constant * T, -0.5) * (p_uo/pow(molarmassuo, 0.5) + p_uo2/pow(molarmassuo2, 0.5) + p_uo3/pow(molarmassuo3, 0.5)); // Missing U single
    double flux_o = p_sys * pow(2 * M_PI * gas_constant * T, -0.5) * (p_uo/pow(molarmassuo, 0.5) + 2 * p_uo2/pow(molarmassuo2, 0.5) + 3 * p_uo3/pow(molarmassuo3, 0.5) + 2 * p_o2/pow(molarmasso2, 0.5)); // Missing O single

    //sciantix_variable["Stoichiometry deviation"].setFinalValue(2 - (flux_o / flux_u));
    sciantix_variable["U vapour"].setFinalValue(
        solver.Integrator(
            sciantix_variable["U vapour"].getInitialValue(),
            + flux_u * 4 * M_PI * pow(sciantix_variable["Grain radius"].getFinalValue(), 2),
            physics_variable["Time step"].getFinalValue()
        )
    );
    sciantix_variable["O vapour"].setFinalValue(
        solver.Integrator(
            sciantix_variable["O vapour"].getInitialValue(),
            + flux_o * 4 * M_PI * pow(sciantix_variable["Grain radius"].getFinalValue(), 2),
            physics_variable["Time step"].getFinalValue()
        )
    );

    double grainradius = solver.Integrator(
        sciantix_variable["Grain radius"].getFinalValue(),
        - molarvolume * flux_u,
        physics_variable["Time step"].getFinalValue()
    );

    if (grainradius > 0.0) sciantix_variable["Grain radius"].setFinalValue(grainradius);
    else std::cout<<"Warning: The grain radius is negative. The vaporisation model is not valid."<<std::endl;

    if (grainradius < 3e-6) input_variable["iGrainBoundaryBehaviour"].setValue(0);

    matrices["UO2"].setGrainRadius(sciantix_variable["Grain radius"].getFinalValue());

    for (auto &system : sciantix_system)
    {
        double UO2vaporisation = (
            sciantix_variable[system.getGasName() + " in grain"].getInitialValue() - 
            solver.Decay(
                sciantix_variable[system.getGasName() + " in grain"].getInitialValue(),
                1.0,
                0.0,
                - 3 * sciantix_variable["Grain radius"].getIncrement() / sciantix_variable["Grain radius"].getFinalValue()
            )
        );

        sciantix_variable[system.getGasName() + " released"].setInitialValue(
            sciantix_variable[system.getGasName() + " released"].getInitialValue() + UO2vaporisation
        );
        sciantix_variable[system.getGasName() + " released"].setConstant();
    }
}