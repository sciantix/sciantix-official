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

    if (p_sys >= 1e6) return;

    double molarmassuo =  254.02e-3; // kg/mol
    double molarmassuo2 = 270.03e-3; // kg/mol
    double molarmassuo3 = 286.03e-3; // kg/mol
    double molarmasso2 = 2 * 16.00e-3; // kg/mol

    double fuelporosity = 1 - sciantix_variable["Fuel density"].getFinalValue()/matrices["UO2"].getTheoreticalDensity();

    // Constants form Olander (Blackburn's model)
    double p_uo = exp(- 49.5/(T/1000) + 11.9);
    double p_uo2 = exp(- 74.0/(T/1000) + 19.9);
    double p_uo3 = exp(- 44.0/(T/1000) + 11.9);

    double p_o2 = sciantix_variable["Fuel oxygen partial pressure"].getFinalValue()*1e6; //Pa
    // double g_o2boundary = - 266700 + 16.5*T; // LINDEMER

    double molarvolume =  fuelporosity *(molarmassuo2/sciantix_variable["Fuel density"].getFinalValue()); //HP of same volume removed
    
    if (x < -1e-6 && x > -1) // hypostoichiometric
    {
        p_uo *= pow(2 + x, 1) * (- x);
        p_uo2 *= pow(2 + x, 2) * (1 + x);
        p_uo3 *= pow(2 + x, 3) * 0;
        p_o2 = pow(exp(- 78300/T + 13.6) * pow( (- x) / ((2 + x)*(1 + x)), -1) ,2);
        
        // LINDEMER
        // p_o2 = exp((-1300000 + 225.7*T - 3*gas_constant*T*log( - 1.5* x/(pow( 1 - x, 2/3)*pow(1 + 0.5*x, 1/3))))/(gas_constant*T));
    }
    else if (x > 1e-6 && x < 1)// hyperstoichiometric
    {
        p_uo *= pow(2 + x, 1) * 0;
        p_uo2 *= pow(2 + x, 2) * (1 - x);
        p_uo3 *= pow(2 + x, 3) * x;
        p_o2 = pow(exp(- 16400/T + 5) * pow( (1 - x) / ((2 + x) * x), -1), 2);
        
        // LINDEMER
        // p_o2 = exp((- 360000 + 214*T + 4*gas_constant*T*log( 2* x * (1 - 2*x)/(pow( 1 - 4*x, 2))))/(gas_constant*T));
        // if (gas_constant*T*log(p_o2) > g_o2boundary)
        //     p_o2 = exp((- 312800 + 126*T + 2*gas_constant*T*log( x * pow(1 - 2*x, 2)/(pow( 1 - 3*x, 3))))/(gas_constant*T));
    }
    else if (x < 1e-6 && x > - 1e-6) // stoichiometric
    {
        p_uo *= 0;
        p_uo2 *= pow(2, 2);
        p_uo3 *= 0;
        p_o2 = 0;

        // LINDEMER
        p_o2 = exp((- 897000 + 224.8*T)/(gas_constant*T));
    }
    else
    {
        std::cout<<"Warning: The stoichiometry is out of range. The vaporisation model is not valid."<<std::endl;
    }


    //double flux_u = p_sys * pow(2 * M_PI * gas_constant * T, -0.5) * (p_uo/pow(molarmassuo, 0.5) + p_uo2/pow(molarmassuo2, 0.5) + p_uo3/pow(molarmassuo3, 0.5)); // Missing U single
    //double flux_o = p_sys * pow(2 * M_PI * gas_constant * T, -0.5) * (p_uo/pow(molarmassuo, 0.5) + 2 * p_uo2/pow(molarmassuo2, 0.5) + 3 * p_uo3/pow(molarmassuo3, 0.5) + 2 * p_o2/pow(molarmasso2, 0.5)); // Missing O single

    double flux_o2 = p_sys * pow(2 * M_PI * gas_constant * T, -0.5) * (p_o2/pow(molarmasso2, 0.5));
    double flux_uo = p_sys * pow(2 * M_PI * gas_constant * T, -0.5) * (p_uo/pow(molarmassuo, 0.5));
    double flux_uo2 = p_sys * pow(2 * M_PI * gas_constant * T, -0.5) * (p_uo2/pow(molarmassuo2, 0.5));
    double flux_uo3 = p_sys * pow(2 * M_PI * gas_constant * T, -0.5) * (p_uo3/pow(molarmassuo3, 0.5));

    double grainradius_f = solver.Integrator(
        sciantix_variable["Grain radius"].getFinalValue(),
        - molarvolume * flux_uo2,
        physics_variable["Time step"].getFinalValue()
    );

    if (grainradius_f > 0.0) sciantix_variable["Grain radius"].setFinalValue(grainradius_f);
    else std::cout<<"Warning: The grain radius is negative. The vaporisation model is not valid."<<std::endl;

    matrices["UO2"].setGrainRadius(sciantix_variable["Grain radius"].getFinalValue());

    if (sciantix_variable["Grain radius"].getIncrement() > 0) return;

    for (auto &system : sciantix_system)
    {
        double UO2vaporisation_IG = (
            sciantix_variable[system.getGasName() + " in grain"].getInitialValue() - 
            solver.Decay(
                sciantix_variable[system.getGasName() + " in grain"].getInitialValue(),
                1.0,
                0.0,
                - 3 * sciantix_variable["Grain radius"].getIncrement() / sciantix_variable["Grain radius"].getInitialValue()
            )
        );

        double bubblethickness = ( 1.0 - cos(matrices["UO2"].getSemidihedralAngle()) ) * sciantix_variable["Intergranular bubble radius"].getFinalValue();
        double UO2vaporisation_GB = (
            sciantix_variable[system.getGasName() + " at grain boundary"].getInitialValue() - 
            solver.Decay(
                sciantix_variable[system.getGasName() + " at grain boundary"].getInitialValue(),
                1.0,
                0.0,
                (sciantix_variable["Initial grain radius"].getFinalValue() - sciantix_variable["Grain radius"].getFinalValue()) / bubblethickness
            )
        );

        double UO2vaporisation_reacted(0);
        if (system.getGas().getChemicallyActive() == 1.0)
        {
            UO2vaporisation_reacted = (
                sciantix_variable[system.getGasName() + " reacted - GB"].getInitialValue() - 
                solver.Decay(
                    sciantix_variable[system.getGasName() + " reacted - GB"].getInitialValue(),
                    1.0,
                    0.0,
                    (sciantix_variable["Initial grain radius"].getFinalValue() - sciantix_variable["Grain radius"].getFinalValue()) / bubblethickness
                )
            );
        }


        if (UO2vaporisation_IG < 0.0)
            UO2vaporisation_IG = 0.0;
        if (UO2vaporisation_GB < 0.0)
            UO2vaporisation_GB = 0.0;
        if (UO2vaporisation_reacted < 0.0)
            UO2vaporisation_reacted = 0.0;

        sciantix_variable[system.getGasName() + " released"].setInitialValue(
            sciantix_variable[system.getGasName() + " released"].getInitialValue() + UO2vaporisation_IG + UO2vaporisation_GB + UO2vaporisation_reacted
        );
        sciantix_variable[system.getGasName() + " released"].setConstant();
    }

    if (sciantix_variable["Xe at grain boundary"].getInitialValue() <= 0.0)
    {
        input_variable["iThermochimica"].setValue(0);
        input_variable["iGrainBoundaryMicroCracking"].setValue(0);
        input_variable["iGrainBoundaryBehaviour"].setValue(0);
        input_variable["iGrainBoundaryVenting"].setValue(0);
    }

    // sciantix_variable["U vapour"].setFinalValue(
    //     solver.Integrator(
    //         sciantix_variable["U vapour"].getInitialValue(),
    //         + flux_u * 4 * M_PI * pow(grainradius_i, 2),
    //         physics_variable["Time step"].getFinalValue()
    //     )
    // );
    double Uinitial = (4/3 * M_PI * pow(sciantix_variable["Initial grain radius"].getFinalValue(), 3))/molarvolume;

    sciantix_variable["O2 vapour"].setFinalValue(
        solver.Integrator(
            sciantix_variable["O2 vapour"].getInitialValue(),
            + flux_o2 * 4 * M_PI * pow(sciantix_variable["Grain radius"].getInitialValue(), 2),
            physics_variable["Time step"].getFinalValue()
        )
    );
    if (sciantix_variable["O2 vapour"].getFinalValue() > Uinitial)
    {
        sciantix_variable["O2 vapour"].setConstant();
    }

    sciantix_variable["UO vapour"].setFinalValue(
        solver.Integrator(
            sciantix_variable["UO vapour"].getInitialValue(),
            + flux_uo * 4 * M_PI * pow(sciantix_variable["Grain radius"].getInitialValue(), 2),
            physics_variable["Time step"].getFinalValue()
        )
    );
    if (sciantix_variable["UO vapour"].getFinalValue() > Uinitial)
    {
        sciantix_variable["UO vapour"].setConstant();
    }

    sciantix_variable["UO2 vapour"].setFinalValue(
        solver.Integrator(
            sciantix_variable["UO2 vapour"].getInitialValue(),
            + flux_uo2 * 4 * M_PI * pow(sciantix_variable["Grain radius"].getInitialValue(), 2),
            physics_variable["Time step"].getFinalValue()
        )
    );
    if (sciantix_variable["UO2 vapour"].getFinalValue() > Uinitial)
    {
        sciantix_variable["UO2 vapour"].setConstant();
    }

    sciantix_variable["UO3 vapour"].setFinalValue(
        solver.Integrator(
            sciantix_variable["UO3 vapour"].getInitialValue(),
            + flux_uo3 * 4 * M_PI * pow(sciantix_variable["Grain radius"].getInitialValue(), 2),
            physics_variable["Time step"].getFinalValue()
        )
    );
    if (sciantix_variable["UO3 vapour"].getFinalValue() > Uinitial)
    {
        sciantix_variable["UO3 vapour"].setConstant();
    }

    double dnu = sciantix_variable["UO vapour"].getIncrement() + sciantix_variable["UO2 vapour"].getIncrement() + sciantix_variable["UO3 vapour"].getIncrement(); 
    double dno = sciantix_variable["UO vapour"].getIncrement() + 2*sciantix_variable["UO2 vapour"].getIncrement() + 3*sciantix_variable["UO3 vapour"].getIncrement() + 2*sciantix_variable["O2 vapour"].getIncrement(); 
    double U = (4/3 * M_PI * pow(sciantix_variable["Grain radius"].getInitialValue(), 3))/molarvolume;
    

    double xf = solver.Decay(
        x,
        dnu/(U + dnu),
        (2*dnu -  dno)/ (U+dnu),
        1.0
    );

    // if (xf < 0)
    // {     
    //     std::cout<<"temperature "<<T<<std::endl;
    //     std::cout<<"stoichiometry "<<x<<std::endl;
    //     std::cout<<"stoichiometry f "<<xf<<std::endl;
    //     std::cout<<"uo "<<(p_uo/pow(molarmassuo, 0.5))<<std::endl;
    //     std::cout<<"uo2 "<<(p_uo2/pow(molarmassuo2, 0.5))<<std::endl;
    //     std::cout<<"uo3 "<<(p_uo3/pow(molarmassuo3, 0.5))<<std::endl;
    //     std::cout<<"o2 "<<(p_o2/pow(molarmasso2, 0.5))<<std::endl;
    // }

    sciantix_variable["Stoichiometry deviation"].setFinalValue(xf);
}