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
            // Trapping rate (HBS), Gosele approximated formula
            double pore_trapping_rate_HBS = 0.0;
            double alpha = sciantix_variable["Restructured volume fraction"].getFinalValue();
            double angle_deg = 4.0 * (1.0 - alpha) + 40.0 * alpha;
            double D_sa = fuel_.getGrainBoundarySingleAtomDiffusivity();
            double D_gb = D_sa * sin(angle_deg * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);

            double c_gb = sciantix_variable["Xe at grain boundary HBS"].getFinalValue();
            double R_pore = sciantix_variable["HBS pore radius"].getFinalValue();
            pore_trapping_rate_HBS =
                4.0 * M_PI * D_gb * c_gb * R_pore * (1.0 + 1.8 * pow(sciantix_variable["HBS porosity"].getFinalValue(), 1.3));
                           
            // Nucleation rate with incubation burnup.
            // Following Biswas & Aagesen 2025 (Comput. Mater. Sci. 258, 114052,
            // Eq. 45), the modified JMAK is alpha_r = 1 - exp[-K*(bu - bu_inc)^n]
            // for bu > bu_inc, 0 otherwise. The threshold bu_inc is derived from
            // the dislocation-energy vs subgrain-formation-energy balance
            // (f_d > E_sub). We apply it consistently to both
            // alpha_r (in HighBurnupStructureFormation) and nu_P (here), so that
            // d(alpha_r)/d(bu) remains the exact Barani 2022 Eq. 6 prescription.
            // This avoids the "gas-reservoir burst" that occurs when the
            // threshold is applied to nu_P alone: grain sub-division produces
            // c_gb^HBS with no pore sink, then the first pores explode when
            // nu_P reactivates.
            double avrami_constant = model["High-burnup structure formation"].getParameter().at(0);
            double transformation_rate = model["High-burnup structure formation"].getParameter().at(1);
            double bu_inc = model["High-burnup structure formation"].getParameter().at(4);
            double bu_for_nucl = sciantix_variable["Effective burnup"].getFinalValue() / 0.8814 - bu_inc;
            double pore_nucleation_rate = 0.0;
            if (bu_for_nucl > 0.0)
            {
                pore_nucleation_rate =
                    1.0e18 * transformation_rate * avrami_constant
                    * (1.0 - sciantix_variable["Restructured volume fraction"].getFinalValue())
                    * pow(bu_for_nucl, avrami_constant - 1.);
                // from d_alpha/d_burnup -> d_alpha/d_time
                if(physics_variable["Time step"].getFinalValue())
                    pore_nucleation_rate *= sciantix_variable["Effective burnup"].getIncrement() / physics_variable["Time step"].getFinalValue();
                else
                    pore_nucleation_rate = 0.0;
            }
            
            // Resolution rate
            double resolution_layer_thickness = model["High-burnup structure formation"].getParameter().at(2);
            double resolution_critical_distance =model["High-burnup structure formation"].getParameter().at(3);
            
            double b0(2.0e-23 * history_variable["Fission rate"].getFinalValue());
            double pore_resolution_rate =
            b0 * 
            (3.0 * resolution_critical_distance / (3.0 * resolution_critical_distance + sciantix_variable["HBS pore radius"].getFinalValue())) *
            (resolution_layer_thickness / (resolution_layer_thickness + sciantix_variable["HBS pore radius"].getFinalValue()));
            
            // Sweeping term
            double sweeping_term(0.0);
            if(physics_variable["Time step"].getFinalValue())
                sweeping_term = 1./(1. - sciantix_variable["Restructured volume fraction"].getFinalValue()) * sciantix_variable["Restructured volume fraction"].getIncrement() / physics_variable["Time step"].getFinalValue(); 
            if(std::isinf(sweeping_term) || std::isnan(sweeping_term))
                sweeping_term = 0.0;

            // from non-restructured grains to non-restructured grain-boundary (bubbles)
            // = - D nabla^2 G
            double gas_in_gb_from_grain(0.0);
            double swept = sweeping_term * physics_variable["Time step"].getFinalValue() * sciantix_variable["Xe in grain"].getInitialValue();
            gas_in_gb_from_grain = 
            sciantix_variable["Xe produced"].getIncrement() -
            sciantix_variable["Xe in grain"].getIncrement() -
            swept;

            // from HBS grains to HBS grain-boundary (gas in solution in HBS grain boundary)
            // = - D nabla^2 G_HBS
            double gas_in_pores_from_hbs_grain(0.0);
            gas_in_pores_from_hbs_grain = 
            sciantix_variable["Xe produced in HBS"].getIncrement() -
            sciantix_variable["Xe in grain HBS"].getIncrement() +
            swept;

            double N_pore = sciantix_variable["HBS pore density"].getFinalValue();
            
            reference = ": Barani T. et al (2020). Journal of Nuclear Materials, 539, 152296. Barani T. et al (2022). Journal of Nuclear Materials, 563, 153627 (linear model).";

            // Solution of the linear model for Np (pore density), A (1st moment), B (2nd moment), C_gb_NR, C_gb_HBS
            double trapping_coeff_HBS = 4.0 * M_PI * D_gb * R_pore * (1.0 + 1.8 * pow(sciantix_variable["HBS porosity"].getFinalValue(), 1.3));
            double total_trapping_rate_HBS = trapping_coeff_HBS * N_pore;
            double dt = physics_variable["Time step"].getFinalValue();

            double coeff_matrix[25];
            double initial_conditions[5];

            for (int i=0; i<25; ++i) coeff_matrix[i] = 0.0;

            coeff_matrix[0] = 1.0 + pore_resolution_rate * dt; 
            
            // Row 1: Xe in HBS pores (A)
            coeff_matrix[6] = 1.0 + pore_resolution_rate * dt;
            coeff_matrix[9] = - total_trapping_rate_HBS * dt;

            // Row 2: Xe in HBS pores - variance (B)
            coeff_matrix[12] = 1.0 + pore_resolution_rate * dt;

            // Row 3: Xe at grain boundary (NR)
            coeff_matrix[18] = 1.0 + sweeping_term * dt;

            // Row 4: Xe at grain boundary HBS
            coeff_matrix[21] = - pore_resolution_rate * dt;
            coeff_matrix[23] = - sweeping_term * dt;
            coeff_matrix[24] = 1.0 + total_trapping_rate_HBS * dt;

            initial_conditions[0] = sciantix_variable["HBS pore density"].getInitialValue() + pore_nucleation_rate * dt;

            initial_conditions[1] = sciantix_variable["Xe in HBS pores"].getInitialValue()
                + 2.0 * pore_nucleation_rate * dt
                + pore_trapping_rate_HBS * dt;

            initial_conditions[2] = sciantix_variable["Xe in HBS pores - variance"].getInitialValue()
                + pow((sciantix_variable["Xe atoms per HBS pore"].getFinalValue() - 2.0), 2.0) * pore_nucleation_rate * dt;

            initial_conditions[3] = sciantix_variable["Xe at grain boundary"].getInitialValue() + gas_in_gb_from_grain;
            initial_conditions[4] = sciantix_variable["Xe at grain boundary HBS"].getInitialValue()
                + gas_in_pores_from_hbs_grain
                - 2.0 * pore_nucleation_rate * dt
                + pore_resolution_rate * sciantix_variable["Xe in HBS pores"].getInitialValue() * dt;

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
            if (sciantix_variable["Xe in HBS pores"].getFinalValue() < 0.0)
            sciantix_variable["Xe in HBS pores"].setFinalValue(0.0);

            // Provisional variables
            pore_trapping_rate_HBS *= scaling_factors["Dummy"].getValue();
            sciantix_variable["trapping rate hbs"].setFinalValue(pore_trapping_rate_HBS);
            sciantix_variable["re-solution rate hbs"].setFinalValue(pore_resolution_rate);
            sciantix_variable["nucleation rate hbs"].setFinalValue(pore_nucleation_rate);
            
            // Xe atoms per HBS pore: n = A / Np
            if (sciantix_variable["HBS pore density"].getFinalValue())
                sciantix_variable["Xe atoms per HBS pore"].setFinalValue(
                    sciantix_variable["Xe in HBS pores"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue()
                );
            
            // HBS pore volume
            double XeHSDiameter = 4.45e-10 * (0.8542 - 0.03996 * log(history_variable["Temperature"].getFinalValue() / 231.2));
            double gasVolumeInPore = M_PI / 6.0 * pow(XeHSDiameter, 3.0);
            double PackingFraction = 0.0;

            if (sciantix_variable["HBS pore volume"].getInitialValue() > 0.0)
                PackingFraction = gasVolumeInPore * sciantix_variable["Xe atoms per HBS pore"].getFinalValue() / sciantix_variable["HBS pore volume"].getInitialValue();
            
            if (PackingFraction > 0.65) PackingFraction = 0.65; // Cap for Hard Spheres

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
                if (psi > 0.7) psi = 0.7; // Guard for cell singularity
                DimensionlessFactor =  10.0 * psi * (1 + pow(psi, 3.0)) / (-pow(psi, 6.0) + 5.0 * pow(psi, 2.0) - 9.0 * psi + 5.0);
            }
        
            if(sciantix_variable["HBS pore radius"].getInitialValue()) equilibrium_pressure = 2.0 * fuel_.getSurfaceTension() / sciantix_variable["HBS pore radius"].getInitialValue() - history_variable["Hydrostatic stress"].getFinalValue() * 1e6;
            
            if(DimensionlessFactor)
            {
                // Barani 2022 Eq. 7: alpha-weighted tilt-angle correction on the
                // grain-boundary vacancy diffusivity. angle_deg was computed at
                // the top of case 2 from the current restructured volume fraction.
                double tilt_factor = sin(angle_deg * M_PI / 180.0) / sin(4.0 * M_PI / 180.0);
                double D_gb_v_tilted = fuel_.getGrainBoundaryVacancyDiffusivity() * tilt_factor;

                // Kinetic saturation of the vacancy flow rate at high porosity.
                //
                // HBS pores do NOT release gas by venting in steady-state
                // operation: they stay closed at 30-100 MPa overpressure
                // (Hiernaut 2008 JNM 377, Noirot 2008 JNM 372). Gas is released
                // only by fragmentation during transients (Kulacsy 2015 JNM 466,
                // Jernkvist 2019 EPJ-N). The observed porosity saturation at
                // xi ~ 0.15-0.20 (Cappia 2016 JNM 480, Spino 2005 JNM 346) is
                // therefore a kinetic phenomenon, not a release.
                //
                // Physical mechanism: at high xi the remaining solid matrix must
                // support the external load on a smaller cross-section, so its
                // local effective stress grows as ~1/(1-xi) and internal vacancy
                // sources (dislocation climb, GB emission) are suppressed. The
                // "infinite vacancy source at the Wigner-Seitz boundary"
                // assumption of the Speight-Beere model breaks down, and the
                // effective volume_flow_rate decreases.
                //
                // Equivalent percolation-theoretic reading: the backbone of the
                // solid matrix transporting vacancies to pores loses connectivity
                // near an effective percolation threshold xi_c; transport scales
                // as (1 - xi/xi_c)^t with t = 2 in 3D (Stauffer & Aharony,
                // Introduction to Percolation Theory, 1994).
                //
                // xi_old is the porosity at the end of the previous time step
                // (step-lagged cap, stable for explicit use). Note that the
                // trapping rate of gas atoms is NOT affected by this cap:
                // gas keeps accumulating in pores that can no longer grow in
                // volume, driving up pressure consistently with the experimental
                // over-pressurization observed in annealing tests.
                double xi_old = sciantix_variable["HBS porosity"].getInitialValue();
                const double xi_sat = 0.22; // saturation porosity (Cappia/Spino upper envelope)
                double linear = std::max(0.0, 1.0 - xi_old / xi_sat);
                double saturation_factor = linear * linear;

                volume_flow_rate = saturation_factor * 2.0 * M_PI * WignerSeitzCellRadius * D_gb_v_tilted / DimensionlessFactor;

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