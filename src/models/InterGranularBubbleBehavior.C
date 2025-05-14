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

void Simulation::InterGranularBubbleBehavior()
{
    // Model declaration
    Model model_;
    Matrix fuel_(matrices[0]);

    model_.setName("Intergranular bubble behavior");
    std::string reference;
    std::vector<double> parameter;

    switch (int(input_variable["iGrainBoundaryBehaviour"].getValue()))
    {
        case 0:
        {
            parameter.push_back(0.0);
            parameter.push_back(0.0);

            reference += ": No model for grain-boundary bubble evolution.";
            break;
        }

        case 1:
        {
            // Gas is distributed among bubbles
            // n(at/bub) = c(at/m3) / (N(bub/m2) S/V(1/m))
            double n_at(0);

            for (auto& system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].setFinalValue(
                        sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() /
                        (sciantix_variable["Intergranular bubble concentration"].getInitialValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));

                    n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
                }
            }
            sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

            // Calculation of the bubble dimension
            // initial volume
            double vol(0);
            for (auto& system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    vol += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue() *
                        system.getGas().getVanDerWaalsVolume();
                }
            }
            vol += sciantix_variable["Intergranular vacancies per bubble"].getInitialValue() * fuel_.getSchottkyVolume();
            sciantix_variable["Intergranular bubble volume"].setInitialValue(vol);

            // initial radius
            sciantix_variable["Intergranular bubble radius"].setInitialValue(
                0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getInitialValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.)
            );

            // initial area
            sciantix_variable["Intergranular bubble area"].setInitialValue(
                M_PI * pow(sciantix_variable["Intergranular bubble radius"].getInitialValue() * sin(fuel_.getSemidihedralAngle()), 2)
            );

            // initial fractional coverage  
            sciantix_variable["Intergranular fractional coverage"].setInitialValue(
                sciantix_variable["Intergranular bubble concentration"].getInitialValue() *
                sciantix_variable["Intergranular bubble area"].getInitialValue()
            );

            // approximation of 1/S, S = -1/4 ((1-F)(3-F)+2lnF)
            const double AA = 1830.1;
            const double BB = -1599.2;
            const double CC = 690.91;
            const double DD = -99.993;
            const double EE = 20.594;

            double sink_strength = 0.4054 +
                AA * pow(sciantix_variable["Intergranular fractional coverage"].getInitialValue(), 5) +
                BB * pow(sciantix_variable["Intergranular fractional coverage"].getInitialValue(), 4) +
                CC * pow(sciantix_variable["Intergranular fractional coverage"].getInitialValue(), 3) +
                DD * pow(sciantix_variable["Intergranular fractional coverage"].getInitialValue(), 2) +
                EE * sciantix_variable["Intergranular fractional coverage"].getInitialValue();

            double volume_flow_rate
                = 2.0 * M_PI * fuel_.getGrainBoundaryThickness() * fuel_.getGrainBoundaryVacancyDiffusivity() * sink_strength;

            // Initial value of the growth rate = 2 pi t D n / S V
            const double growth_rate = volume_flow_rate * sciantix_variable["Intergranular atoms per bubble"].getFinalValue() / fuel_.getSchottkyVolume();

            double surfacetension = fuel_.getSurfaceTension();

            if (input_variable["iGrainBoundaryMicroCracking"].getValue() == 2)
                surfacetension *= (1 - cos(fuel_.getSemidihedralAngle()));

            double equilibrium_pressure(0), equilibrium_term(0);
            if (sciantix_variable["Intergranular bubble radius"].getInitialValue())
            {
                equilibrium_pressure = 2.0 * surfacetension / sciantix_variable["Intergranular bubble radius"].getInitialValue() - history_variable["Hydrostatic stress"].getFinalValue() * 1e6;

                equilibrium_term = -volume_flow_rate * equilibrium_pressure / (boltzmann_constant * history_variable["Temperature"].getFinalValue());
            }

            parameter.push_back(growth_rate);
            parameter.push_back(equilibrium_term);

            reference += ": Pastore et al., NED, 256 (2013) 75-86.";

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "iGrainBoundaryBehaviour", int(input_variable["iGrainBoundaryBehaviour"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    // Vacancy concentration
    sciantix_variable["Intergranular vacancies per bubble"].setFinalValue(
        solver.LimitedGrowth(
            sciantix_variable["Intergranular vacancies per bubble"].getInitialValue(),
            model["Intergranular bubble behavior"].getParameter(),
            physics_variable["Time step"].getFinalValue()
        )
    );

    // Model declaration
    Model model2_;
    model2_.setName("Release mode");
    std::string reference2;

    switch (int(input_variable["iReleaseMode"].getValue()))
    {
        case 0:
        {
            // Grain-boundary bubble volume
            double vol(0);
            for (auto &system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    vol += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue() * system.getGas().getVanDerWaalsVolume();
                }
            }
            vol += sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * fuel_.getSchottkyVolume();
            sciantix_variable["Intergranular bubble volume"].setFinalValue(vol);

            // Grain-boundary bubble radius
            sciantix_variable["Intergranular bubble radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.)
            );

            // Grain-boundary bubble area
            sciantix_variable["Intergranular bubble area"].setFinalValue(
                M_PI * pow(sciantix_variable["Intergranular bubble radius"].getFinalValue() * sin(fuel_.getSemidihedralAngle()), 2)
            );

            // Grain-boundary bubble coalescence
            double dbubble_area = sciantix_variable["Intergranular bubble area"].getIncrement();
            sciantix_variable["Intergranular bubble concentration"].setFinalValue(
                solver.BinaryInteraction(sciantix_variable["Intergranular bubble concentration"].getInitialValue(), 2.0, dbubble_area)
            );

            // Conservation
            for (auto &system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].rescaleFinalValue(
                        sciantix_variable["Intergranular bubble concentration"].getInitialValue() / sciantix_variable["Intergranular bubble concentration"].getFinalValue()
                    );
                }
            }

            double n_at(0);
            for (auto &system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                    n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
            }
            sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

            sciantix_variable["Intergranular vacancies per bubble"].rescaleFinalValue(
                sciantix_variable["Intergranular bubble concentration"].getInitialValue() / sciantix_variable["Intergranular bubble concentration"].getFinalValue()
            );

            vol = 0.0;
            for (auto &system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                    vol += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue() *
                            system.getGas().getVanDerWaalsVolume();
            }

            vol += sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * fuel_.getSchottkyVolume();
            sciantix_variable["Intergranular bubble volume"].setFinalValue(vol);

            sciantix_variable["Intergranular bubble radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.)
            );

            sciantix_variable["Intergranular bubble area"].setFinalValue(
                M_PI * pow(sciantix_variable["Intergranular bubble radius"].getFinalValue() * sin(fuel_.getSemidihedralAngle()), 2)
            );

            // Fractional coverage
            sciantix_variable["Intergranular fractional coverage"].setFinalValue(
                sciantix_variable["Intergranular bubble area"].getFinalValue() * sciantix_variable["Intergranular bubble concentration"].getFinalValue()
            );

            // Intergranular gas release
            //                          F0
            //   ___________A0____________
            //   |_________A1__________  |
            //   |                    |  |
            //   |          F1        N1 N0
            //   |                    |  |
            //   |____________________|__|
            double similarity_ratio;

            if (sciantix_variable["Intergranular fractional coverage"].getFinalValue() > 0.0)
                similarity_ratio = sqrt(
                    sciantix_variable["Intergranular saturation fractional coverage"].getFinalValue() / sciantix_variable["Intergranular fractional coverage"].getFinalValue());
            else
                similarity_ratio = 1.0;

            if (similarity_ratio < 1.0)
            {
                sciantix_variable["Intergranular bubble area"].rescaleFinalValue(similarity_ratio);
                sciantix_variable["Intergranular bubble concentration"].rescaleFinalValue(similarity_ratio);
                sciantix_variable["Intergranular fractional coverage"].rescaleFinalValue(pow(similarity_ratio, 2));
                sciantix_variable["Intergranular bubble volume"].rescaleFinalValue(pow(similarity_ratio, 1.5));
                sciantix_variable["Intergranular bubble radius"].rescaleFinalValue(pow(similarity_ratio, 0.5));
                sciantix_variable["Intergranular vacancies per bubble"].rescaleFinalValue(pow(similarity_ratio, 1.5));

                // New intergranular gas concentration
                for (auto &system : sciantix_system)
                {
                    if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                        sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].rescaleFinalValue(pow(similarity_ratio, 1.5));
                }

                n_at = 0.0;
                for (auto &system : sciantix_system)
                {
                    if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                        n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
                }
                sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

                for (auto &system : sciantix_system)
                {
                    if (system.getRestructuredMatrix() == 0)
                        sciantix_variable[system.getGasName() + " at grain boundary"].rescaleFinalValue(pow(similarity_ratio, 2.5));
                }
            }

            reference2 += " coalescence from White, JNM, 325 (2004), 61-77; release from Pastore et al., NED, 256 (2013), 75-86.";

            break;
        }
        case 1:
        {
            reference2 += " coalescence from Pastore et al., NED, 256 (2013), 75-86.";
			
            // Volume occupied by gas at grain boundaries
            double gasvolume_i(0.0);
            double gasvolume_f(0.0);
            for (auto& system : sciantix_system) 
            {   
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    gasvolume_i += (system.getGas().getVanDerWaalsVolume()*sciantix_variable[system.getGasName() + " at grain boundary"].getInitialValue());
                    gasvolume_f += (system.getGas().getVanDerWaalsVolume()*sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue());
                }
            }
            
            // State matrix for the grain boundary: bubble volume, area, concentration and grain-face coverage
		    double grainboundary_statematrix[4*4];

            // Bubble volume 
		    grainboundary_statematrix[0] = 1.0;
		    grainboundary_statematrix[1] = 0.0;
		    grainboundary_statematrix[2] = gasvolume_i / ( pow(sciantix_variable["Intergranular bubble concentration"].getInitialValue(),2) * (3.0 / sciantix_variable["Grain radius"].getFinalValue()));
		    grainboundary_statematrix[3] = 0.0;

            // Bubble area
            if (sciantix_variable["Intergranular bubble volume"].getInitialValue() > 0.0)
                grainboundary_statematrix[4+0] = - ( 2.0 / 3.0 ) * sciantix_variable["Intergranular bubble area"].getInitialValue() / sciantix_variable["Intergranular bubble volume"].getInitialValue();
            else
                grainboundary_statematrix[4+0] = 0.0;

            grainboundary_statematrix[4+1] = 1.0;
            grainboundary_statematrix[4+2] = 0.0;
            grainboundary_statematrix[4+3] = 0.0;

            // Bubble concentration solved by Pastore et al. (2013) coalescence
            grainboundary_statematrix[4+4+0] = 0.0;

            if (sciantix_variable["Intergranular fractional coverage"].getInitialValue() < sciantix_variable["Intergranular saturation fractional coverage"].getInitialValue())
                grainboundary_statematrix[4+4+1] = 6.0 * pow(sciantix_variable["Intergranular bubble concentration"].getInitialValue(),2) / ( 3.0 + 4.0 * sciantix_variable["Intergranular fractional coverage"].getInitialValue());
            else
                grainboundary_statematrix[4+4+1] = sciantix_variable["Intergranular bubble concentration"].getInitialValue() / sciantix_variable["Intergranular bubble area"].getInitialValue();
            
            grainboundary_statematrix[4+4+2] = 1.0;
            grainboundary_statematrix[4+4+3] = 0.0;

            // Fractional coverage
            grainboundary_statematrix[4+4+4+0] = 0.0;
            grainboundary_statematrix[4+4+4+1] = - sciantix_variable["Intergranular bubble concentration"].getInitialValue();
            grainboundary_statematrix[4+4+4+2] = - sciantix_variable["Intergranular bubble area"].getInitialValue();
            grainboundary_statematrix[4+4+4+3] = 1.0;

            for (int i = 0; i < 4*4; ++i) {
                if (std::isnan(grainboundary_statematrix[i]))
                    grainboundary_statematrix[i] = 0.0;
            }
            
            // Initial values of bubble volume, area, concentration, and grain-face coverage
		    double grainboundary_input[4];
		    grainboundary_input[0] = sciantix_variable["Intergranular bubble volume"].getInitialValue() + sciantix_variable["Intergranular vacancies per bubble"].getIncrement() * fuel_.getSchottkyVolume() +
            	( gasvolume_f ) / ( sciantix_variable["Intergranular bubble concentration"].getInitialValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue()));
		    grainboundary_input[1] = sciantix_variable["Intergranular bubble area"].getInitialValue() / 3.0;
            if (sciantix_variable["Intergranular fractional coverage"].getInitialValue() < sciantix_variable["Intergranular saturation fractional coverage"].getInitialValue())
            {
                grainboundary_input[2] = sciantix_variable["Intergranular bubble concentration"].getInitialValue() + 
                    6.0 * pow(sciantix_variable["Intergranular bubble concentration"].getInitialValue(),2) * sciantix_variable["Intergranular bubble area"].getInitialValue() 
                    / ( 3.0 + 4.0 * sciantix_variable["Intergranular fractional coverage"].getInitialValue());
            }
            else
            {
                grainboundary_input[2]= 2.0 * sciantix_variable["Intergranular bubble concentration"].getInitialValue();
            }
		    grainboundary_input[3]= sciantix_variable["Intergranular fractional coverage"].getInitialValue() - 2.0 * sciantix_variable["Intergranular bubble concentration"].getInitialValue() * sciantix_variable["Intergranular bubble area"].getInitialValue();

            for (int i = 0; i < 4; ++i) {
                if (std::isnan(grainboundary_input[i]))
                    grainboundary_input[i] = 0.0;
            }
		
            // Solve for the finale values of bubble volume, area, concentration, and grain-face coverage
		    solver.Laplace(4, grainboundary_statematrix, grainboundary_input);

            for (int i = 0; i < 4; ++i) {
                if (grainboundary_input[i]<0)
                    grainboundary_input[i] = 0.0;
            }

            sciantix_variable["Intergranular bubble volume"].setFinalValue(grainboundary_input[0]);
            sciantix_variable["Intergranular bubble area"].setFinalValue(grainboundary_input[1]);
            
            if (sciantix_variable["Intergranular bubble area"].getIncrement() >= 0.0)
            {
                sciantix_variable["Intergranular bubble concentration"].setFinalValue(grainboundary_input[2]);
                sciantix_variable["Intergranular fractional coverage"].setFinalValue(grainboundary_input[3]);
            }
            else
            {
                sciantix_variable["Intergranular bubble concentration"].setConstant();
                sciantix_variable["Intergranular fractional coverage"].setFinalValue(
                    sciantix_variable["Intergranular bubble area"].getFinalValue() * sciantix_variable["Intergranular bubble concentration"].getFinalValue());
            }
            
            sciantix_variable["Intergranular bubble radius"].setFinalValue(
                    0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.));

            // Release fraction for diffusion-based release
            double vented_fraction_initial(0.0), vented_fraction_final(0.0);
            
            if (scaling_factors["Diffusion-based release"].getValue() == 2)
            {
                vented_fraction_initial = (26.3997 * 1e-2 * (erf(0.0718 * 100 * sciantix_variable["Intergranular fractional coverage"].getInitialValue() - 2.6002) + 1));
		        vented_fraction_final = (26.3997 * 1e-2 * (erf(0.0718 * 100 * sciantix_variable["Intergranular fractional coverage"].getFinalValue() - 2.6002) + 1));
                reference2 += " release model from Cappellari et al., JNM, (2025, under review), Gaussian Process Regression mean prediction";
            }
            else if (scaling_factors["Diffusion-based release"].getValue() == 3)
            {
                vented_fraction_initial = (21.0911 * 1e-2 * (erf(0.0937 * 100 * (sciantix_variable["Intergranular fractional coverage"].getInitialValue() - 0.5)) + 1));
                vented_fraction_final = (21.0911 * 1e-2 * (erf(0.0937 * 100 * (sciantix_variable["Intergranular fractional coverage"].getFinalValue() - 0.5)) + 1));
                reference2 += " release model from Cappellari et al., JNM, (2025, under review), saturation threshold at 50% fractional coverage";
            }
            else
            {
                vented_fraction_initial = (21.0911 * 1e-2 * (erf(0.0937 * 100 * sciantix_variable["Intergranular fractional coverage"].getInitialValue() - 3.7250) + 1));
                vented_fraction_final = (21.0911 * 1e-2 * (erf(0.0937 * 100 * sciantix_variable["Intergranular fractional coverage"].getFinalValue() - 3.7250) + 1));
                reference2 += " release model from Cappellari et al., JNM, (2025, under review), Gaussian Process Regression lower bound";
            }
            
            // Combined fraction (probability) of diffusion-based release and burst release from microcracking, from Cappellari et al. (2025);
            double release_fraction_initial(0.0), release_fraction_final(0.0), release_fraction_increment(0.0);
            
            release_fraction_initial = vented_fraction_initial * sciantix_variable["Intergranular fractional intactness"].getInitialValue() + (1 - sciantix_variable["Intergranular fractional intactness"].getInitialValue());
            release_fraction_final = vented_fraction_final * sciantix_variable["Intergranular fractional intactness"].getFinalValue() + (1 - sciantix_variable["Intergranular fractional intactness"].getFinalValue());
            
            release_fraction_increment = release_fraction_final - release_fraction_initial;
            if (release_fraction_increment < 0.0)
                release_fraction_increment = 0.0;


            // Atoms at grain boundary
            double n_at(0.0);
            
            for (auto& system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {                                      
                    sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(
                        solver.Integrator(
                            (1 - release_fraction_increment) * sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue(),
                            - release_fraction_final,
                            sciantix_variable[system.getGasName() + " at grain boundary"].getIncrement()
                        )
                    );

                    if (sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() < 0.0)
                        sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(0.0);
                
                    sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].setFinalValue(
                        sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() /
                        (sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));

                    n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
                }
            }
            sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

            break;
        }

        case 2:
        {
            // Grain-boundary bubble volume
            double vol(0);
            for (auto &system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    vol += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue() * system.getGas().getVanDerWaalsVolume();
                }
            }
            vol += sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * fuel_.getSchottkyVolume();
            sciantix_variable["Intergranular bubble volume"].setFinalValue(vol);

            // Grain-boundary bubble radius
            sciantix_variable["Intergranular bubble radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.)
            );

            // Grain-boundary bubble area
            sciantix_variable["Intergranular bubble area"].setFinalValue(
                M_PI * pow(sciantix_variable["Intergranular bubble radius"].getFinalValue() * sin(fuel_.getSemidihedralAngle()), 2)
            );

            // Grain-boundary bubble coalescence
            double N_bub = 2e13/(1+2*2e13*sciantix_variable["Intergranular bubble area"].getFinalValue());
            if (N_bub > sciantix_variable["Intergranular bubble concentration"].getFinalValue())
                sciantix_variable["Intergranular bubble concentration"].setConstant();
            else 
                sciantix_variable["Intergranular bubble concentration"].setFinalValue(N_bub);

            // Conservation
            for (auto &system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].rescaleFinalValue(
                        sciantix_variable["Intergranular bubble concentration"].getInitialValue() / sciantix_variable["Intergranular bubble concentration"].getFinalValue()
                    );
                }
            }

            double n_at(0);
            for (auto &system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                    n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
            }
            sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

            sciantix_variable["Intergranular vacancies per bubble"].rescaleFinalValue(
                sciantix_variable["Intergranular bubble concentration"].getInitialValue() / sciantix_variable["Intergranular bubble concentration"].getFinalValue()
            );

            vol = 0.0;
            for (auto &system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                    vol += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue() *
                            system.getGas().getVanDerWaalsVolume();
            }

            vol += sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * fuel_.getSchottkyVolume();
            sciantix_variable["Intergranular bubble volume"].setFinalValue(vol);

            sciantix_variable["Intergranular bubble radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.)
            );

            sciantix_variable["Intergranular bubble area"].setFinalValue(
                M_PI * pow(sciantix_variable["Intergranular bubble radius"].getFinalValue() * sin(fuel_.getSemidihedralAngle()), 2)
            );

            // Fractional coverage
            sciantix_variable["Intergranular fractional coverage"].setFinalValue(
                sciantix_variable["Intergranular bubble area"].getFinalValue() * sciantix_variable["Intergranular bubble concentration"].getFinalValue()
            );

            sciantix_variable["Intergranular bubble radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.));

            // Release fraction for diffusion-based release
            double vented_fraction_initial(0.0), vented_fraction_final(0.0);
            
            if (scaling_factors["Diffusion-based release"].getValue() == 2)
            {
                vented_fraction_initial = (26.3997 * 1e-2 * (erf(0.0718 * 100 * sciantix_variable["Intergranular fractional coverage"].getInitialValue() - 2.6002) + 1));
                vented_fraction_final = (26.3997 * 1e-2 * (erf(0.0718 * 100 * sciantix_variable["Intergranular fractional coverage"].getFinalValue() - 2.6002) + 1));
                reference2 += " release model from Cappellari et al., JNM, (2025, under review), Gaussian Process Regression mean prediction";
            }
            else if (scaling_factors["Diffusion-based release"].getValue() == 3)
            {
                vented_fraction_initial = (21.0911 * 1e-2 * (erf(0.0937 * 100 * (sciantix_variable["Intergranular fractional coverage"].getInitialValue() - 0.5)) + 1));
                vented_fraction_final = (21.0911 * 1e-2 * (erf(0.0937 * 100 * (sciantix_variable["Intergranular fractional coverage"].getFinalValue() - 0.5)) + 1));
                reference2 += " release model from Cappellari et al., JNM, (2025, under review), saturation threshold at 50% fractional coverage";
            }
            else
            {
                vented_fraction_initial = (21.0911 * 1e-2 * (erf(0.0937 * 100 * sciantix_variable["Intergranular fractional coverage"].getInitialValue() - 3.7250) + 1));
                vented_fraction_final = (21.0911 * 1e-2 * (erf(0.0937 * 100 * sciantix_variable["Intergranular fractional coverage"].getFinalValue() - 3.7250) + 1));
                reference2 += " release model from Cappellari et al., JNM, (2025, under review), Gaussian Process Regression lower bound";
            }
            
            // Combined fraction (probability) of diffusion-based release and burst release from microcracking, from Cappellari et al. (2025);
            double release_fraction_initial(0.0), release_fraction_final(0.0), release_fraction_increment(0.0);
            
            release_fraction_initial = vented_fraction_initial * sciantix_variable["Intergranular fractional intactness"].getInitialValue() + (1 - sciantix_variable["Intergranular fractional intactness"].getInitialValue());
            release_fraction_final = vented_fraction_final * sciantix_variable["Intergranular fractional intactness"].getFinalValue() + (1 - sciantix_variable["Intergranular fractional intactness"].getFinalValue());
            
            release_fraction_increment = release_fraction_final - release_fraction_initial;
            if (release_fraction_increment < 0.0)
                release_fraction_increment = 0.0;

            double n_bub(0.0), n_tot(0.0);
            vol = 0.0;
            for (auto &system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {
                    
                    double n_tot = solver.Integrator(
                            (1 - release_fraction_increment) * sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue(),
                            - release_fraction_final,
                            sciantix_variable[system.getGasName() + " at grain boundary"].getIncrement()
                    );

                    if (n_tot < 0.0) n_tot = 0.0;
                
                    double n_bub = n_tot / (sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue()));

                    if (n_bub < 0.0) n_bub = 0.0;

                    vol += n_bub * system.getGas().getVanDerWaalsVolume();
                }
            }

            vol += sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * fuel_.getSchottkyVolume();
            sciantix_variable["Intergranular bubble volume"].setFinalValue(vol);

            // Grain-boundary bubble radius
            sciantix_variable["Intergranular bubble radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["Intergranular bubble volume"].getFinalValue() / (fuel_.getLenticularShapeFactor()), 1. / 3.)
            );
            
            // Grain-boundary bubble area
            sciantix_variable["Intergranular bubble area"].setFinalValue(
                M_PI * pow(sciantix_variable["Intergranular bubble radius"].getFinalValue() * sin(fuel_.getSemidihedralAngle()), 2)
            );

            sciantix_variable["Intergranular fractional coverage"].setFinalValue(sciantix_variable["Intergranular bubble concentration"].getFinalValue() * sciantix_variable["Intergranular bubble area"].getFinalValue());
 
            if (scaling_factors["Diffusion-based release"].getValue() == 2)
                vented_fraction_final = (26.3997 * 1e-2 * (erf(0.0718 * 100 * sciantix_variable["Intergranular fractional coverage"].getFinalValue() - 2.6002) + 1));
            else if (scaling_factors["Diffusion-based release"].getValue() == 3)
                vented_fraction_final = (21.0911 * 1e-2 * (erf(0.0937 * 100 * (sciantix_variable["Intergranular fractional coverage"].getFinalValue() - 0.5)) + 1));
            else
                vented_fraction_final = (21.0911 * 1e-2 * (erf(0.0937 * 100 * sciantix_variable["Intergranular fractional coverage"].getFinalValue() - 3.7250) + 1));

            release_fraction_final = vented_fraction_final * sciantix_variable["Intergranular fractional intactness"].getFinalValue() + (1 - sciantix_variable["Intergranular fractional intactness"].getFinalValue());
            release_fraction_increment = release_fraction_final - release_fraction_initial;
            if (release_fraction_increment < 0.0) release_fraction_increment = 0.0;
            // Atoms at grain boundary
            n_at =0.0 ;
            
            for (auto& system : sciantix_system)
            {
                if (system.getGas().getDecayRate() == 0.0 && system.getRestructuredMatrix() == 0)
                {                                     
                    sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(
                        solver.Integrator(
                            (1 - release_fraction_increment) * sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue(),
                            - release_fraction_final,
                            sciantix_variable[system.getGasName() + " at grain boundary"].getIncrement()
                        )
                    );

                    if (sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() < 0.0)
                        sciantix_variable[system.getGasName() + " at grain boundary"].setFinalValue(0.0);
                
                    sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].setFinalValue(
                        sciantix_variable[system.getGasName() + " at grain boundary"].getFinalValue() /
                        (sciantix_variable["Intergranular bubble concentration"].getFinalValue() * (3.0 / sciantix_variable["Grain radius"].getFinalValue())));

                    n_at += sciantix_variable["Intergranular " + system.getGasName() + " atoms per bubble"].getFinalValue();
                }
            }
            sciantix_variable["Intergranular atoms per bubble"].setFinalValue(n_at);

            break;
        }
    }

    model2_.setRef(reference2);
    model.push(model2_);

    // Intergranular bubble pressure p = kTng/Onv (MPa)
    if(sciantix_variable["Intergranular vacancies per bubble"].getFinalValue())
        sciantix_variable["Intergranular bubble pressure"].setFinalValue(
            1e-6 * boltzmann_constant * history_variable["Temperature"].getFinalValue() * sciantix_variable["Intergranular atoms per bubble"].getFinalValue() /
            (sciantix_variable["Intergranular vacancies per bubble"].getFinalValue() * matrices["UO2"].getSchottkyVolume())
        );
    else
        sciantix_variable["Intergranular bubble pressure"].setFinalValue(0.0);
}