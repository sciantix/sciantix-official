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
#include <iostream>  // aggiunta 

void Simulation::HighBurnupStructurePorosity()
{
    if (!int(input_variable["iHighBurnupStructurePorosity"].getValue())) return;

    // Model declaration
    Model model_;

    model_.setName("High-burnup structure porosity");
    double porosity_increment = 0.0;

    std::string reference;
    std::vector<double> parameter;

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

        default:
            ErrorMessages::Switch(__FILE__, "HighBurnupStructurePorosity", int(input_variable["HighBurnupStructurePorosity"].getValue()));
            break;
    }

    model_.setParameter(parameter);
    model_.setRef(reference);

    model.push(model_);

    // Model resolution

    double coeff_matrix[9];
    double initial_conditions[3];

    coeff_matrix[0] = 1 + matrices["UO2HBS"].getPoreResolutionRate() * physics_variable["Time step"].getFinalValue();
    coeff_matrix[1] = 0.0;
    coeff_matrix[2] = 0.0;

    coeff_matrix[3] = - (matrices["UO2HBS"].getPoreTrappingRate() - matrices["UO2HBS"].getPoreResolutionRate()) * physics_variable["Time step"].getFinalValue();
    coeff_matrix[4] = 1.0;
    coeff_matrix[5] = 0.0;

    coeff_matrix[6] = - (matrices["UO2HBS"].getPoreTrappingRate() - matrices["UO2HBS"].getPoreResolutionRate()) * physics_variable["Time step"].getFinalValue();
    coeff_matrix[7] = 0.0;
    coeff_matrix[8] = 1.0;

    initial_conditions[0] = sciantix_variable["HBS pore density"].getInitialValue() + matrices["UO2HBS"].getPoreNucleationRate() * physics_variable["Time step"].getFinalValue();
    initial_conditions[1] = sciantix_variable["Xe in HBS pores"].getInitialValue() + 2.0 * matrices["UO2HBS"].getPoreNucleationRate() * physics_variable["Time step"].getFinalValue();
    initial_conditions[2] = sciantix_variable["Xe in HBS pores - variance"].getInitialValue() + matrices["UO2HBS"].getPoreNucleationRate() *(pow(sciantix_variable["Xe atoms per HBS pore"].getFinalValue() - 2.0 , 2.0)) * physics_variable["Time step"].getFinalValue();

    solver.Laplace3x3(coeff_matrix, initial_conditions);

    sciantix_variable["HBS pore density"].setFinalValue(initial_conditions[0]);
    sciantix_variable["Xe in HBS pores"].setFinalValue(initial_conditions[1]);
    sciantix_variable["Xe in HBS pores - variance"].setFinalValue(initial_conditions[2]);

    sciantix_variable["trapping rate hbs"].setFinalValue(matrices["UO2HBS"].getPoreTrappingRate());
    sciantix_variable["re-solution rate hbs"].setFinalValue(matrices["UO2HBS"].getPoreResolutionRate());
    sciantix_variable["nucleation rate hbs"].setFinalValue(matrices["UO2HBS"].getPoreNucleationRate());

    if (sciantix_variable["HBS pore density"].getFinalValue())
        sciantix_variable["Xe atoms per HBS pore"].setFinalValue(
            sciantix_variable["Xe in HBS pores"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue()
    );
   
    //calculation of pore volume 
    // VDW volume
    double XeHSDiameter = 4.45e-10 * (0.8542 - 0.03996 * log(history_variable["Temperature"].getFinalValue() / 231.2));
    double PackingFraction = 0.5235987756 * (pow(XeHSDiameter, 3) * sciantix_variable["Xe in HBS pores"].getFinalValue());
    double gasVolumeInPore(0.0);
    if(sciantix_variable["Xe in HBS pores"].getFinalValue() > 0.0) gasVolumeInPore = PackingFraction / sciantix_variable["Xe in HBS pores"].getFinalValue();

    // std::cout << "Gas volume" << std::endl;
    // std::cout <<gasVolumeInPore<< std::endl;

    // sciantix_system["Xe in UO2HBS"].getGas().setVanDerWaalsVolume(
    //     PackingFraction / sciantix_variable["Xe in HBS pores"].getFinalValue()
    // );
    
    // std::cout << "omega calcolato" << std::endl;
    // std::cout << sciantix_system["Xe in UO2HBS"].getGas().getVanDerWaalsVolume() << std::endl;
    // std::cout << PackingFraction / sciantix_variable["Xe in HBS pores"].getFinalValue() << std::endl;

    //sciantix_variable["vacancies per pore"].getFinalValue() * fuel_.getSchottkyVolume())

    sciantix_variable["HBS pore volume"].setInitialValue(
        sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * gasVolumeInPore + 
        sciantix_variable["Vacancies per HBS pore"].getInitialValue() * matrices["UO2HBS"].getSchottkyVolume()
    ); 

    sciantix_variable["HBS pore radius"].setInitialValue(0.620350491 * pow(sciantix_variable["HBS pore volume"].getInitialValue(), (1.0 / 3.0)));

    // calculation of the contribution of vacancies
    std::vector<double> parameter_2;

    double VacancyDiffusionCoefficient = matrices["UO2HBS"].getGrainBoundaryVacancyDiffusivity();

    double WignerSeitzCellRadius = 1 / 1.611991954 * pow(sciantix_variable["HBS pore density"].getFinalValue(), (-1.0 / 3.0));
    double psi = sciantix_variable["HBS pore radius"].getInitialValue() / WignerSeitzCellRadius;
    double DimensionlessFactor = 10 * psi * (1 + pow(psi, 3)) / (- pow(psi, 6) + 5 * pow(psi, 2) - 9 * psi + 5); 

    double EquilibriumPressure(0.0);
    if(sciantix_variable["HBS pore radius"].getInitialValue()) EquilibriumPressure = 2 / sciantix_variable["HBS pore radius"].getInitialValue() - history_variable["Hydrostatic stress"].getFinalValue(); 

    // std::cout << sciantix_variable["Xe atoms per HBS pore"].getFinalValue() << std::endl;
    // std::cout << sciantix_variable["HBS pore volume"].getInitialValue() << std::endl;

    double volume_flow_rate = 2.0 * M_PI * WignerSeitzCellRadius * VacancyDiffusionCoefficient / DimensionlessFactor;

    // Initial value of the growth rate = 2 pi rho D n / S V
    const double growth_rate = volume_flow_rate * sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * ((1 + PackingFraction + pow(PackingFraction, 2) - pow(PackingFraction, 3)) / (pow(1 - PackingFraction, 3))) / matrices["UO2HBS"].getSchottkyVolume();
    
    // Equilibrium term 
    double EquilibriumTerm(0);
    EquilibriumTerm = - volume_flow_rate * EquilibriumPressure / (boltzmann_constant * history_variable["Temperature"].getFinalValue());
    
    parameter_2.push_back(growth_rate);
    parameter_2.push_back(EquilibriumTerm);

    //std::cout<<"PorePressure = " << PorePressure<<std::endl; 
    //std::cout<<"EquilibriumPressure = " << EquilibriumPressure<<std::endl;
    
    if(DimensionlessFactor)
        sciantix_variable["Vacancies per HBS pore"].setFinalValue(
            solver.LimitedGrowth(
                sciantix_variable["Vacancies per HBS pore"].getInitialValue(),
                parameter_2,
                physics_variable["Time step"].getFinalValue()
            )
        );
    else
        sciantix_variable["Vacancies per HBS pore"].setFinalValue(0.0);

    // if(sciantix_variable["Vacancies per HBS pore"].getFinalValue() < 0.0) sciantix_variable["Vacancies per HBS pore"].setFinalValue(0.0);

    sciantix_variable["HBS pore volume"].setFinalValue(sciantix_variable["Xe atoms per HBS pore"].getFinalValue() * gasVolumeInPore + sciantix_variable["Vacancies per HBS pore"].getFinalValue() *  matrices["UO2HBS"].getSchottkyVolume());
    sciantix_variable["HBS pore radius"].setFinalValue(0.620350491 * pow(sciantix_variable["HBS pore volume"].getFinalValue(), (1.0 / 3.0)));
    
    // std::cout<<"HBS pore radius"<<std::endl;
    // std::cout<<sciantix_variable["HBS pore radius"].getFinalValue()<<std::endl;
    // std::cout<<"HBS pore volume"<<std::endl;
    // std::cout<<sciantix_variable["HBS pore volume"].getFinalValue()<<std::endl;

    // calculation of the porosity 
    sciantix_variable["HBS porosity"].setFinalValue(sciantix_variable["HBS pore volume"].getFinalValue() * sciantix_variable["HBS pore density"].getFinalValue());

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

    if (sciantix_variable["HBS pore density"].getFinalValue())
        sciantix_variable["Xe atoms per HBS pore - variance"].setFinalValue(
            sciantix_variable["Xe in HBS pores - variance"].getFinalValue() / sciantix_variable["HBS pore density"].getFinalValue()
        );
       
}

