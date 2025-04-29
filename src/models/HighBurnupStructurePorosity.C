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

#include "Solver.h"
#include <iostream>

void Simulation::HighBurnupStructurePorosity()
{
    if (!int(input_variable["iHighBurnupStructurePorosity"].getValue())) return;

    // Model declaration
    Model model_;

    model_.setName("High-burnup structure porosity");

    double porosity_increment = 0.0;

    std::string reference;
    std::vector<double> parameter;

    Matrix fuel_(matrices["UO2HBS"]);

    switch (int(input_variable["iHighBurnupStructurePorosity"].getValue()))
    {
        case 0:
        {
            reference += "not considered";
            parameter.push_back(0.0);
            sciantix_variable["HBS porosity"].setInitialValue(0.0);
            sciantix_variable["HBS porosity"].setFinalValue(0.0);
            break;
        }

        case 1:
        {
            double rate_coefficient = 1.3e-3;
            double porosity_upper_threshold = 0.15;
            double burnup_threshold = 50.0;

            if (sciantix_variable["HBS porosity"].getInitialValue() < porosity_upper_threshold)
            {
                if (sciantix_variable["Burnup"].getFinalValue() < burnup_threshold)
                    porosity_increment = 0.0;
                else
                    porosity_increment = rate_coefficient;
            }

            else
            {
                sciantix_variable["HBS porosity"].setInitialValue(0.15);
                porosity_increment = 0.0;
            }

            reference = ": Based on Spino et al. 2006 data";

            parameter.push_back(porosity_increment);

            break;
        }

        case 2:
        {   
            double pore_trapping_rate_HBS = 4.0 * M_PI * fuel_.getGrainBoundarySingleAtomDiffusivity() *
                   (sciantix_variable["Xe at grain boundary HBS"].getFinalValue()) *
                   sciantix_variable["HBS pore radius"].getFinalValue() * 
                   (1.0 + 1.8 * pow(sciantix_variable["HBS porosity"].getFinalValue(), 1.3)) * 
                   sin(40.0 * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);

            double pore_trapping_rate = 4.0 * M_PI * fuel_.getGrainBoundarySingleAtomDiffusivity() *
                   (sciantix_variable["Xe at grain boundary"].getFinalValue()) *
                   sciantix_variable["HBS pore radius"].getFinalValue() * 
                   (1.0 + 1.8 * pow(sciantix_variable["HBS porosity"].getFinalValue(), 1.3));

            double sf_nucleation_rate_porosity = 1.25e-6; 
            double avrami_constant = 3.54; 
            double transformation_rate = 2.77e-7; 
            double pore_nucleation_rate = (5.0e17 * transformation_rate * avrami_constant * (1.0 - sciantix_variable["Restructured volume fraction"].getFinalValue())* pow(sciantix_variable["Effective burnup"].getFinalValue()/0.8814, avrami_constant - 1.));
            pore_nucleation_rate *= sf_nucleation_rate_porosity;
            
            // double correction_coefficient = (1.0 - exp(pow(-sciantix_variable["HBS pore radius"].getFinalValue() / (9e-9), 3)));
            double b0(2.0e-23 * history_variable["Fission rate"].getFinalValue());
            double pore_resolution_rate =
                b0 *
                (3.0 * 1.0e-9 / (3.0 * 1.0e-9 + sciantix_variable["HBS pore radius"].getFinalValue())) *
                (1.0e-9 / (1.0e-9 + sciantix_variable["HBS pore radius"].getFinalValue()));

            double sweeping_term(0.0);
            if(physics_variable["Time step"].getFinalValue())
                // sweeping_term = 1./(1. - sciantix_variable["Restructured volume fraction"].getFinalValue()) * sciantix_variable["Restructured volume fraction"].getIncrement() / physics_variable["Time step"].getFinalValue();
        
            if (std::isinf(sweeping_term) || std::isnan(sweeping_term))
                sweeping_term = 0.0;

            double source_ig(0.0);
            if(physics_variable["Time step"].getFinalValue())
            {
                source_ig =  
                    (sciantix_variable["Xe produced"].getIncrement() +
                    sciantix_variable["Xe produced in HBS"].getIncrement() -
                    sciantix_variable["Xe decayed"].getIncrement() -
                    sciantix_variable["Xe in grain"].getIncrement() -
                    sciantix_variable["Xe in grain HBS"].getIncrement() -
                    sciantix_variable["Xe in HBS pores"].getIncrement() -
                    sciantix_variable["Xe released"].getIncrement());
            }

            double source_ig_HBS = sciantix_variable["Restructured volume fraction"].getFinalValue() * source_ig;

            source_ig = (1 - sciantix_variable["Restructured volume fraction"].getFinalValue()) * source_ig;
    

        
            // pore_resolution_rate *= scaling_factors["Cent parameter"].getValue();

            // double source_ig = sciantix_variable["Xe produced in HBS"].getIncrement() -
            //         sciantix_variable["Xe in grain HBS"].getIncrement();
            
            // References:
            // <a href="https://www.sciencedirect.com/science/article/pii/S002231152030427X" target="_blank">Barani T. et al (2020). Journal of Nuclear Materials, 539, 152296.</a>
            // <a href="https://www.sciencedirect.com/science/article/pii/S0022311522001234" target="_blank">Barani T. et al (2022). Journal of Nuclear Materials, 563, 153627.</a>

            reference = ": Barani T. et al (2020). Journal of Nuclear Materials, 539, 152296. Barani T. et al (2022). Journal of Nuclear Materials, 563, 153627 (linear model).";

            // Solution of the linear model for Np (pore density), A (1st moment), B (2nd moment)
            double coeff_matrix[25];
            double initial_conditions[5];
        
            coeff_matrix[0] = 1 + pore_resolution_rate * physics_variable["Time step"].getFinalValue();
            coeff_matrix[1] = 0.0;
            coeff_matrix[2] = 0.0;
            coeff_matrix[3] = 0.0;
            coeff_matrix[4] = 0.0;

            coeff_matrix[5] = - (pore_trapping_rate + pore_trapping_rate_HBS) * physics_variable["Time step"].getFinalValue();
            coeff_matrix[6] = 1.0 + pore_resolution_rate * physics_variable["Time step"].getFinalValue();
            coeff_matrix[7] = 0.0;
            coeff_matrix[8] = 0.0;
            coeff_matrix[9] = 0.0;

            coeff_matrix[10] = - (pore_trapping_rate + pore_trapping_rate_HBS) * physics_variable["Time step"].getFinalValue();
            coeff_matrix[11] = 0.0;
            coeff_matrix[12] = 1.0 + pore_resolution_rate * physics_variable["Time step"].getFinalValue();
            coeff_matrix[13] = 0.0;
            coeff_matrix[14] = 0.0;

            coeff_matrix[15] = pore_trapping_rate * physics_variable["Time step"].getFinalValue();
            coeff_matrix[16] = - (1 - sciantix_variable["Restructured volume fraction"].getFinalValue()) * pore_resolution_rate * physics_variable["Time step"].getFinalValue();
            coeff_matrix[17] = 0.0;
            coeff_matrix[18] = 1 + sweeping_term * physics_variable["Time step"].getFinalValue();
            coeff_matrix[19] = 0.0;

            coeff_matrix[20] = pore_trapping_rate_HBS * physics_variable["Time step"].getFinalValue();
            coeff_matrix[21] = - sciantix_variable["Restructured volume fraction"].getFinalValue() * pore_resolution_rate * physics_variable["Time step"].getFinalValue();
            coeff_matrix[22] = 0.0;
            coeff_matrix[23] = - sweeping_term * physics_variable["Time step"].getFinalValue();
            coeff_matrix[24] = 1.0;
    
            initial_conditions[0] = sciantix_variable["HBS pore density"].getInitialValue() + pore_nucleation_rate * physics_variable["Time step"].getFinalValue();
            initial_conditions[1] = sciantix_variable["Xe in HBS pores"].getInitialValue() + source_ig_HBS + 2.0 * pore_nucleation_rate * physics_variable["Time step"].getFinalValue();
            initial_conditions[2] = sciantix_variable["Xe in HBS pores - variance"].getInitialValue() + pore_nucleation_rate * pow(sciantix_variable["Xe atoms per HBS pore"].getFinalValue() - 2.0 , 2.0) * physics_variable["Time step"].getFinalValue();
            initial_conditions[3] = sciantix_variable["Xe at grain boundary"].getInitialValue() + source_ig - (1 - sciantix_variable["Restructured volume fraction"].getFinalValue()) * pore_nucleation_rate * physics_variable["Time step"].getFinalValue();
            initial_conditions[4] = sciantix_variable["Xe at grain boundary HBS"].getInitialValue() - sciantix_variable["Restructured volume fraction"].getFinalValue() * pore_nucleation_rate * physics_variable["Time step"].getFinalValue();

            solver.Laplace(5, coeff_matrix, initial_conditions);
        
            sciantix_variable["HBS pore density"].setFinalValue(initial_conditions[0]); // Np
            sciantix_variable["Xe in HBS pores"].setFinalValue(initial_conditions[1]);  // A
            sciantix_variable["Xe in HBS pores - variance"].setFinalValue(initial_conditions[2]); // B
            sciantix_variable["Xe at grain boundary"].setFinalValue(initial_conditions[3]); // CGB
            sciantix_variable["Xe at grain boundary HBS"].setFinalValue(initial_conditions[4]); // CGBHBS

            if (sciantix_variable["Xe at grain boundary"].getFinalValue() < 0.0)
            sciantix_variable["Xe at grain boundary"].setFinalValue(0.0);
            if (sciantix_variable["Xe at grain boundary HBS"].getFinalValue() < 0.0)
            sciantix_variable["Xe at grain boundary HBS"].setFinalValue(0.0);

            // Provisional variables
            sciantix_variable["trapping rate hbs"].setFinalValue(pore_trapping_rate);
            sciantix_variable["re-solution rate hbs"].setFinalValue(pore_resolution_rate);
            sciantix_variable["nucleation rate hbs"].setFinalValue(pore_nucleation_rate);
            
            // Xe atoms per HBS pore: n = A / Np
            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore"].setFinalValue(
                    sciantix_variable["Xe in HBS pores"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue()
                );
           
            // HBS pore volume 
            double XeHSDiameter = 4.45e-10 * (0.8542 - 0.03996 * log(history_variable["Temperature"].getFinalValue() / 231.2));
            double PackingFraction = M_PI / 6.0 * pow(XeHSDiameter, 3) * sciantix_variable["Xe in HBS pores"].getFinalValue();
            double gasVolumeInPore = M_PI / 6.0 * pow(XeHSDiameter, 3);
            double Z_compr = ((1.0 + PackingFraction + pow(PackingFraction, 2.0) - pow(PackingFraction, 3.0)) / (pow(1.0 - PackingFraction, 3.0)));

            sciantix_variable["HBS pore volume"].setInitialValue(
                sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * gasVolumeInPore + 
                sciantix_variable["Vacancies per HBS pore"].getInitialValue() * fuel_.getSchottkyVolume()
            ); 
            
            // HBS pore radius
            sciantix_variable["HBS pore radius"].setInitialValue(0.620350491 * pow(sciantix_variable["HBS pore volume"].getInitialValue(), (1.0 / 3.0)));
        
            // Vacancy contribution       
            double WignerSeitzCellRadius(0.0), psi(0.0);
            double equilibrium_pressure(0.0);
            double volume_flow_rate(0.0), growth_rate(0.0), equilibrium_term(0);
            double DimensionlessFactor(0.0);

            if(sciantix_variable["HBS pore density"].getFinalValue())
            {
                WignerSeitzCellRadius = pow(3.0 / (4.0 * M_PI * sciantix_variable["HBS pore density"].getFinalValue()), (1.0 / 3.0));
                psi = sciantix_variable["HBS pore radius"].getInitialValue() / WignerSeitzCellRadius;
                DimensionlessFactor =  10.0 * psi * (1 + pow(psi, 3.0)) / (-pow(psi, 6.0) + 5.0 * pow(psi, 2.0) - 9.0 * psi + 5.0);
                //DimensionlessFactor = 3.0 / fuel_.getGrainRadius() * WignerSeitzCellRadius * 8.0 * psi * (1.-pow(psi,3)) / (- pow(psi, 5.0) + 2.0 * pow(psi, 3.0) + 4.0 * pow(psi, 2.0) - 9.0 * psi + 4.0);
            }
        
            // double N_grains = pow(WignerSeitzCellRadius,3) - pow(sciantix_variable["HBS pore radius"].getInitialValue(),3) / pow(fuel_.getGrainRadius(),3);

            if(sciantix_variable["HBS pore radius"].getInitialValue()) equilibrium_pressure = 2.0 * fuel_.getSurfaceTension() / sciantix_variable["HBS pore radius"].getInitialValue() - history_variable["Hydrostatic stress"].getFinalValue() * 1e6;
            
            if(DimensionlessFactor)
            {
                //volume_flow_rate = 2.0 * M_PI * fuel_.getGrainBoundaryThickness() * fuel_.getGrainBoundaryVacancyDiffusivity() * DimensionlessFactor;
                volume_flow_rate = 2.0 * M_PI * WignerSeitzCellRadius * fuel_.getGrainBoundaryVacancyDiffusivity() / DimensionlessFactor;
                growth_rate = volume_flow_rate * sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * Z_compr / fuel_.getSchottkyVolume();
                equilibrium_term = - volume_flow_rate * equilibrium_pressure / (boltzmann_constant * history_variable["Temperature"].getFinalValue());
                
                parameter.push_back(growth_rate);
                parameter.push_back(equilibrium_term);
            }
            else
            {
                parameter.push_back(0.0);
                parameter.push_back(0.0);
            }
        
            // print wigner seitz cell radius
            // std::cout << "Wigner Seitz Cell Radius: " << WignerSeitzCellRadius << std::endl;
            // std::cout << "psi: " << psi << std::endl;
            // std::cout << "Dimensionless Factor: " << DimensionlessFactor << std::endl;
            // std::cout << "Equilibrium Pressure: " << equilibrium_pressure << std::endl;
            // std::cout << "Volume Flow Rate: " << volume_flow_rate << std::endl;
            
            sciantix_variable["Vacancies per HBS pore"].setFinalValue(
                solver.LimitedGrowth(
                    sciantix_variable["Vacancies per HBS pore"].getInitialValue(),
                    parameter,
                    physics_variable["Time step"].getFinalValue()
                )
            );
        
            // HBS pore volume - update with vacancy contribution
            sciantix_variable["HBS pore volume"].setFinalValue(
                sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * gasVolumeInPore + sciantix_variable["Vacancies per HBS pore"].getFinalValue() *  fuel_.getSchottkyVolume()
            );
        
            sciantix_variable["HBS pore radius"].setFinalValue(
                0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), 1. / 3.)
            );
        
            sciantix_variable["HBS porosity"].setFinalValue(
                sciantix_variable["HBS pore volume"].getFinalValue() * sciantix_variable["HBS pore density"].getFinalValue()
            );
            
            // HBS pore interconnection by impingement
            double limiting_factor =
               (2.0 - sciantix_variable["HBS porosity"].getFinalValue()) / (2.0 * pow(1.0 - sciantix_variable["HBS porosity"].getFinalValue(), 3.0));
        
            double pore_interconnection_rate = 4.0 * limiting_factor;
        
            sciantix_variable["HBS pore density"].resetValue();
            sciantix_variable["HBS pore density"].setFinalValue(
                solver.BinaryInteraction(
                    sciantix_variable["HBS pore density"].getInitialValue(),
                    pore_interconnection_rate,
                    sciantix_variable["HBS pore volume"].getIncrement()
                )
            );
        
            // Conservation (atoms and vacancies)
            if(sciantix_variable["HBS pore density"].getFinalValue())
            {
                sciantix_variable["Xe atoms per HBS pore"].rescaleFinalValue(
                    sciantix_variable["HBS pore density"].getInitialValue() / sciantix_variable["HBS pore density"].getFinalValue()
                );
        
                sciantix_variable["Vacancies per HBS pore"].rescaleFinalValue(
                    sciantix_variable["HBS pore density"].getInitialValue() / sciantix_variable["HBS pore density"].getFinalValue()
                );
        
                sciantix_variable["HBS pore volume"].setFinalValue(sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * gasVolumeInPore + sciantix_variable["Vacancies per HBS pore"].getFinalValue() *  fuel_.getSchottkyVolume());
        
                sciantix_variable["HBS pore radius"].setFinalValue(
                    0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), 1. / 3.)
                );
            }
            
            sciantix_variable["HBS porosity"].setFinalValue(
                sciantix_variable["HBS pore volume"].getFinalValue() * sciantix_variable["HBS pore density"].getFinalValue()
            );
        
            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore - variance"].setFinalValue(
                    sciantix_variable["Xe in HBS pores - variance"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue()
                );

            break;
        }

        default:
            ErrorMessages::Switch(__FILE__, "HighBurnupStructurePorosity", int(input_variable["HighBurnupStructurePorosity"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution
    switch (int(input_variable["iHighBurnupStructurePorosity"].getValue()))
    {
        case 0:
            break;

        case 1:
        {
            // empirical porosity evolution
            sciantix_variable["HBS porosity"].setFinalValue(
                solver.Integrator(
                    sciantix_variable["HBS porosity"].getInitialValue(),
                    model["High-burnup structure porosity"].getParameter().at(0),
                    sciantix_variable["Burnup"].getIncrement()
                )
            );

            if (sciantix_variable["HBS porosity"].getFinalValue() > 0.15)
                sciantix_variable["HBS porosity"].setFinalValue(0.15);

            // evolution of pore number density via pore nucleation and re-solution
            if (sciantix_variable["HBS porosity"].getFinalValue())
                sciantix_variable["HBS pore density"].setFinalValue(
                    solver.Decay(
                        sciantix_variable["HBS pore density"].getInitialValue(),
                        fuel_.getPoreResolutionRate(),
                        fuel_.getPoreNucleationRate(),
                        physics_variable["Time step"].getFinalValue()
                    )
                );
            else
                sciantix_variable["HBS pore density"].setFinalValue(0.0);

            // calculation of pore volume based on porosity and pore number density
            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["HBS pore volume"].setFinalValue(
                    sciantix_variable["HBS porosity"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue()
                );

            sciantix_variable["HBS pore radius"].setFinalValue(0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), (1.0 / 3.0)));

            // update of number density of HBS pores: interconnection by impingement
            double limiting_factor =
                (2.0 - sciantix_variable["HBS porosity"].getFinalValue()) / (2.0 * pow(1.0 - sciantix_variable["HBS porosity"].getFinalValue(), 3.0));

            double pore_interconnection_rate = 4.0 * limiting_factor;
            sciantix_variable["HBS pore density"].setFinalValue(
                solver.BinaryInteraction(
                    sciantix_variable["HBS pore density"].getFinalValue(),
                    pore_interconnection_rate,
                    sciantix_variable["HBS pore volume"].getIncrement()
                )
            );

            // update of pore volume and pore radius after interconnection by impingement
            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["HBS pore volume"].setFinalValue(
                    sciantix_variable["HBS porosity"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue()
                );

            sciantix_variable["HBS pore radius"].setFinalValue(0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), (1.0 / 3.0)));

            // average (at/m^3) of gas atoms in HBS pores
            sciantix_variable["Xe in HBS pores"].setFinalValue(
                solver.Integrator(
                    sciantix_variable["Xe in HBS pores"].getInitialValue(),
                    2.0 * fuel_.getPoreNucleationRate() + sciantix_variable["HBS pore density"].getFinalValue() * (fuel_.getPoreTrappingRate() - fuel_.getPoreResolutionRate()),
                    physics_variable["Time step"].getFinalValue()
                )
            );

            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore"].setFinalValue(
                    sciantix_variable["Xe in HBS pores"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue()
                );

            sciantix_variable["Xe in HBS pores - variance"].setFinalValue(
                solver.Integrator(
                    sciantix_variable["Xe in HBS pores - variance"].getInitialValue(),
                    matrices["UO2"].getPoreTrappingRate() * sciantix_variable["HBS pore density"].getFinalValue() - matrices["UO2"].getPoreResolutionRate() * sciantix_variable["HBS pore density"].getFinalValue() + matrices["UO2"].getPoreNucleationRate() * pow((sciantix_variable["Xe atoms per HBS pore"].getFinalValue() - 2.0), 2.0),
                    physics_variable["Time step"].getFinalValue()
                )
            );

            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore - variance"].setFinalValue(
                    sciantix_variable["Xe in HBS pores - variance"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue()
                );
            break;
        }

        case 2:
            break;

        default:
            ErrorMessages::Switch(__FILE__, "HighBurnupStructurePorosity", int(input_variable["HighBurnupStructurePorosity"].getValue()));
            break;
    }

}