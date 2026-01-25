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

void Simulation::GrainVaporisation(bool thermochemistry_module)
{
    if (sciantix_variable["Grain radius"].getFinalValue() <= 0.0) return;
    
    Matrix fuel(matrices[0]);

    double temperature = history_variable["Temperature"].getFinalValue(); // K
    double x = sciantix_variable["Stoichiometry deviation"].getFinalValue();
    double pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue(); // Pa
    double total_metal_content = sciantix_variable["Uranium content"].getFinalValue() + sciantix_variable["Plutonium content"].getFinalValue();
    double initial_total_moles = total_metal_content + sciantix_variable["Oxygen content"].getFinalValue();
    double q = (total_metal_content > 0.0) ? (sciantix_variable["Plutonium content"].getFinalValue() / total_metal_content) : 0.0;
    double u_fraction = 1.0 - q;
    double n_o2 = 2.0 + x;
    double R = 8.314;
    double n_u(0), n_u2(0), n_u4(0), n_u6(0);
    double n_pu(0), n_pu2(0), n_pu3(0), n_pu4(0);
    double p_o2(0);
    double n_o = 2.0;
    
    if (thermochemistry_module)
    {
        std::vector<double> kato_parameters;
        kato_parameters.push_back(temperature); // parameter[0]
        kato_parameters.push_back(q);           // parameter[1]
        kato_parameters.push_back(n_o2);        // parameter[2]

        Solver solver; 
        double p_o2 = solver.BisectionKato(kato_parameters);

        // Calculation of concentrations
        double K_u24 = std::exp(-(78.3e3 / temperature) + 13.6); // constants from Olander
        double K_u46 = std::exp(-(16.4e3 / temperature) + 5.0);
        double K_pu34 = std::exp(-(50.1e3 / temperature) + 10.3);
        double K_pu23 = std::exp(-(92.5e3 / temperature) + 21.3);
        double sqrt_po2 = std::sqrt(p_o2);
        
        // U
        double denom_u = 1.0 + ((K_u24 * n_o) / sqrt_po2) + (sqrt_po2 / (K_u46 * n_o));
        n_u4 = (1.0 - q) / denom_u;
        n_u2 = ((K_u24 * n_o) / sqrt_po2) * n_u4;
        n_u6 = (sqrt_po2 / (K_u46 * n_o)) * n_u4;
        n_u = 0.0;
        
        // Pu
        double ratio_34 = std::sqrt((K_pu34 * n_o) / sqrt_po2);
        double ratio_23 = std::sqrt((K_pu23 * n_o) / sqrt_po2);
        double denom_pu = 1.0 + ratio_34 + (ratio_23 * ratio_34);
        n_pu4 = q / denom_pu;
        n_pu3 = ratio_34 * n_pu4;
        n_pu2 = ratio_23 * n_pu3;

    }


    else if ((sciantix_variable["Oxygen content"].getFinalValue() <= 0) && (total_metal_content > 0))
    {
        n_u4 = 0; n_u6 = 0; n_u = 1.0 - q; n_u2 = 0;
        n_pu4 = 0; n_pu3 = 0; n_pu = q; n_pu2 = 0;
    }
    
        double u_content = sciantix_variable["Uranium content"].getFinalValue();
        double pu_content = sciantix_variable["Plutonium content"].getFinalValue();
        double o_content = sciantix_variable["Oxygen content"].getFinalValue();
    
        double frac_u = (1.0 - q > 1e-9) ? (1.0 / (1.0 - q)) : 0.0;
        double frac_pu = (q > 1e-9) ? (1.0 / q) : 0.0;
    
        // Calculation of partial fraction
        double K_UO = std::exp(-49500.0 / temperature + 11.9); // from table 11.1 pag 158 Olander
        double K_UO2 = std::exp(-74000.0 / temperature + 19.9);
        double K_UO3 = std::exp(-44000.0 / temperature + 11.9);
        double K_PuO = std::exp(-44100.0 / temperature + 11.5);
        double K_PuO2= std::exp(-72500.0 / temperature + 18.8);
    
        double p_uo = 2.0 * K_UO * n_u2;
        double p_uo2 = 4.0 * K_UO2 * n_u4;
        double p_uo3 = 8.0 * K_UO3 * n_u6;
        double p_puo = 2.0 * K_PuO * n_pu2;
        double p_puo2= 4.0 * K_PuO2 * n_pu4;
        double p_u = n_u * std::exp(-58000.0 / temperature + 13.5);
    
    
        thermochemistry_variable["UO (solid, matrix)"].setFinalValue(n_u2 * frac_u * sciantix_variable["Uranium content"].getFinalValue());
        thermochemistry_variable["UO2 (solid, matrix)"].setFinalValue(n_u4 * frac_u * sciantix_variable["Uranium content"].getFinalValue());
        thermochemistry_variable["UO3 (solid, matrix)"].setFinalValue(n_u6 * frac_u * sciantix_variable["Uranium content"].getFinalValue());
        thermochemistry_variable["U (solid, matrix)"].setFinalValue(n_u * frac_u * sciantix_variable["Uranium content"].getFinalValue());
        thermochemistry_variable["PuO (solid, matrix)"].setFinalValue(n_pu2 * frac_pu * sciantix_variable["Plutonium content"].getFinalValue());
        thermochemistry_variable["PuO2 (solid, matrix)"].setFinalValue(n_pu4 * frac_pu * sciantix_variable["Plutonium content"].getFinalValue());

        // Knudsen cell setup: skip if pressure is too high
        if (pressure >= 1e6) return;
        

        struct VapourCompound {
            double partialpressure;
            std::string solidname;
            std::map <std::string, double> stoichiometry;
        };

        std::map<std::string, VapourCompound> vapourdata = {
            {"U (vapour, matrix)",  {p_u,   "U (solid, matrix)", {{"U", n_u}}}},
            {"O2 (vapour, matrix)", {p_o2,  "O2 (solid, matrix)", {{"O", n_o}}}},
            {"UO (vapour, matrix)", {p_uo,  "UO (solid, matrix)", {{"U", n_u2},{"O", n_o2}}}},
            {"UO2 (vapour, matrix)",{p_uo2, "UO2 (solid, matrix)", {{"U", n_u4},{"O", n_o2}}}},
            {"UO3 (vapour, matrix)",{p_uo3, "UO3 (solid, matrix)", {{"U", n_u6},{"O", n_o2}}}},
            {"PuO (vapour, matrix)",  {p_puo,  "PuO2 (solid, matrix)", {{"Pu", n_pu2},{"O", n_o2}}}},
            {"PuO2 (vapour, matrix)", {p_puo2, "PuO2 (solid, matrix)", {{"Pu", n_pu4},{"O", n_o2}}}}
        };

        for (auto &compound : thermochemistry_variable)
        {
            if ((compound.getPhase() == "vapour") && (compound.getLocation() == "matrix"))
            {
                std::string name = compound.getName();
                auto it = vapourdata.find(name);
                if (it == vapourdata.end())
                {
                    std::cerr << "Warning: compound '" << compound.getName() << "' not found. Skipping.\n";
                    continue;
                }
                const VapourCompound &data = it->second;

                double molar_mass_i = MolarMass(compound)*1e-3; //kg/mol
                double flux_i =  pow(2 * M_PI * gas_constant * temperature, -0.5) * (data.partialpressure*pressure/pow(molar_mass_i, 0.5)); // mol m-2 s-1
                
                auto sto = compound.getStoichiometry();
                double mol_u(0.0), mol_o(0.0), mol_pu(0.0);
                if (sto.find("U") != sto.end()) mol_u = sto["U"];
                if (sto.find("O") != sto.end()) mol_o = sto["O"];
                if (sto.find("Pu") != sto.end()) mol_pu = sto["Pu"];

                double fraction_i(0);
                if (mol_u != 0) fraction_i = (thermochemistry_variable[data.solidname].getFinalValue()/sciantix_variable["Uranium content"].getFinalValue() * mol_u);
                else if (mol_pu != 0) fraction_i = (thermochemistry_variable[data.solidname].getFinalValue()/sciantix_variable["Plutonium content"].getFinalValue() * mol_pu);
                else if (mol_o != 0) fraction_i = (thermochemistry_variable[data.solidname].getFinalValue()/sciantix_variable["Oxygen content"].getFinalValue()  * mol_o);

                double porosity = 1 - sciantix_variable["Fuel density"].getFinalValue() / fuel.getTheoreticalDensity(); // porosity of the matrix
                double surfacetovolume = (3/sciantix_variable["Grain radius"].getFinalValue()) * fraction_i * porosity; // only a portion of the surface is available to the compound
                double increment = flux_i*surfacetovolume*physics_variable["Time step"].getFinalValue(); 
                increment = std::min(increment, thermochemistry_variable[data.solidname].getFinalValue());
                if (mol_u != 0) increment = std::min(increment, sciantix_variable["Uranium content"].getFinalValue()/mol_u);
                if (mol_pu != 0) increment = std::min(increment, sciantix_variable["Plutonium content"].getFinalValue()/mol_pu);
                if (mol_o != 0) increment = std::min(increment, sciantix_variable["Oxygen content"].getFinalValue()/mol_o);
                
                if (increment > 0)
                {
                    compound.setFinalValue(compound.getInitialValue() + increment);
                    thermochemistry_variable[data.solidname].setFinalValue(thermochemistry_variable[data.solidname].getFinalValue() - increment);

                    sciantix_variable["Uranium content"].addValue(- mol_u*increment);
                    sciantix_variable["Plutonium content"].addValue(- mol_pu*increment);
                    sciantix_variable["Oxygen content"].addValue(- mol_o*increment);
                }
            }
        }
        double final_total_moles = sciantix_variable["Uranium content"].getFinalValue() + sciantix_variable["Plutonium content"].getFinalValue() + sciantix_variable["Oxygen content"].getFinalValue();

        sciantix_variable["Grain radius"].setFinalValue(sciantix_variable["Grain radius"].getFinalValue() * pow(final_total_moles/initial_total_moles, 1.0/3.0));


    double final_total_metal_content = sciantix_variable["Uranium content"].getFinalValue() + sciantix_variable["Plutonium content"].getFinalValue();

    if ((sciantix_variable["Oxygen content"].getFinalValue() > 0) && (final_total_metal_content> 0))
        sciantix_variable["Stoichiometry deviation"].setFinalValue(sciantix_variable["Oxygen content"].getFinalValue()/final_total_metal_content - 2);
    else
        sciantix_variable["Stoichiometry deviation"].setFinalValue(0);

    if (sciantix_variable["Grain radius"].getFinalValue() <= 0) sciantix_variable["Grain radius"].setFinalValue(0.0);

    fuel.setGrainRadius(sciantix_variable["Grain radius"].getFinalValue());

    if (sciantix_variable["Grain radius"].getIncrement() >= 0) return; // grain growth > vaporisation

    for (auto &system : sciantix_system)
    {
        std::string gas = system.getGasName();

        double UO2vaporisation_IG = (
            sciantix_variable[gas + " in grain"].getInitialValue() -
            solver.Decay(
                sciantix_variable[gas + " in grain"].getInitialValue(),
                1.0,
                0.0,
                - 3 * sciantix_variable["Grain radius"].getIncrement() / sciantix_variable["Grain radius"].getInitialValue()
            )
        );

        double bubblethickness = ( 1.0 - cos(fuel.getSemidihedralAngle()) ) * sciantix_variable["Intergranular bubble radius"].getFinalValue();
        double UO2vaporisation_GB = (
            sciantix_variable[gas + " at grain boundary"].getInitialValue() -
            solver.Decay(
                sciantix_variable[gas + " at grain boundary"].getInitialValue(),
                1.0,
                0.0,
                (sciantix_variable["Initial grain radius"].getFinalValue() - sciantix_variable["Grain radius"].getFinalValue()) / bubblethickness
            )
        );

        double UO2vaporisation_reacted(0);
        if (system.getGas().getChemicallyActive() == 1.0)
        {
            UO2vaporisation_reacted = (
                sciantix_variable[gas + " reacted - GB"].getInitialValue() -
                solver.Decay(
                    sciantix_variable[gas + " reacted - GB"].getInitialValue(),
                    1.0,
                    0.0,
                    (sciantix_variable["Initial grain radius"].getFinalValue() - sciantix_variable["Grain radius"].getFinalValue()) / bubblethickness
                )
            );
        }

        if (UO2vaporisation_IG < 0.0) UO2vaporisation_IG = 0.0;
        if (UO2vaporisation_GB < 0.0) UO2vaporisation_GB = 0.0;
        if (UO2vaporisation_reacted < 0.0) UO2vaporisation_reacted = 0.0;

        sciantix_variable[gas + " released"].setInitialValue(
            sciantix_variable[gas + " released"].getInitialValue() + UO2vaporisation_IG + UO2vaporisation_GB + UO2vaporisation_reacted
        );
        sciantix_variable[gas + " released"].setConstant();
    }

    if (sciantix_variable["Xe at grain boundary"].getInitialValue() <= 0.0)
    {
        input_variable["iGrainBoundaryMicroCracking"].setValue(0);
        input_variable["iGrainBoundaryBehaviour"].setValue(0);
        input_variable["iGrainBoundaryVenting"].setValue(0);
        input_variable["iGrainGrowth"].setValue(0);
    }
}

double Simulation::MolarMass(ThermochemistryVariable& compound) // g/mol
{
    // Implement the molar mass calculation based on the compound's stoichiometry
    double molar_mass = 0.0;

    double conv_fact = sciantix_variable["Fuel density"].getFinalValue() * avogadro_number * 10 * 0.8815 * 100; // to move from atoms/m3 to percentage in atoms (see Initialization.cpp)
	double molar_mass_Uranium = sciantix_variable["U234"].getFinalValue()/conv_fact *pow(234.04095,2)+ sciantix_variable["U235"].getFinalValue()/conv_fact *pow(235.04393,2)+
								sciantix_variable["U236"].getFinalValue()/conv_fact *pow(236.04557,2)+ sciantix_variable["U237"].getFinalValue()/conv_fact *pow(237.04873,2)+
								sciantix_variable["U238"].getFinalValue()/conv_fact *pow(238.05079,2);
    double molar_mass_Plutonium = sciantix_variable["Pu238"].getFinalValue()/conv_fact *pow(238.049, 2)+ sciantix_variable["Pu239"].getFinalValue()/conv_fact *pow(239.05, 2)+ 
                                  sciantix_variable["Pu240"].getFinalValue()/conv_fact *pow(240.06, 2)+ sciantix_variable["Pu241"].getFinalValue()/conv_fact *pow(241.05, 2)+ 
                                  sciantix_variable["Pu242"].getFinalValue()/conv_fact *pow(242.06, 2);                   

    // Iterate through the stoichiometry map to calculate the molar mass
    for (const auto& element : compound.getStoichiometry())
    {
        const std::string& element_name = element.first;
        int number_of_atoms = element.second;

        if (element_name == "U")
            molar_mass += molar_mass_Uranium * number_of_atoms;
        else if (element_name == "Pu")
            molar_mass += molar_mass_Plutonium * number_of_atoms;
        else if (element_name == "O")
            molar_mass += molar_mass_Oxygen * number_of_atoms;
        else
        {
            for (auto& system : sciantix_system)
            {
                if (system.getGasName() == element_name)
                    molar_mass += system.getGas().getAtomicNumber() * number_of_atoms;
            }
        }
    }

    return molar_mass;
}

double Simulation::MolarVolume(ThermochemistryVariable& compound, Matrix& fuel) //m3/mol
{
    // Implement the molar volume calculation based on the compound's stoichiometry
    double molar_volume = 0.0;
    double molar_mass = 0.0;
    double total_atoms = 0.0;
    double theoretical_density = 0.0;

    double lattice_parameter = fuel.getLatticeParameter(); // in meters

    double conv_fact = sciantix_variable["Fuel density"].getFinalValue() * avogadro_number * 10 * 0.8815 * 100; // to move from atoms/m3 to percentage in atoms (see Initialization.cpp)
	double molar_mass_Uranium = sciantix_variable["U234"].getFinalValue()/conv_fact *pow(234.04095,2)+ sciantix_variable["U235"].getFinalValue()/conv_fact *pow(235.04393,2)+
								sciantix_variable["U236"].getFinalValue()/conv_fact *pow(236.04557,2)+ sciantix_variable["U237"].getFinalValue()/conv_fact *pow(237.04873,2)+
								sciantix_variable["U238"].getFinalValue()/conv_fact *pow(238.05079,2);

    // Iterate through the stoichiometry map to calculate the molar volume
    for (const auto& element : compound.getStoichiometry())
    {
        const std::string& element_name = element.first;
        int number_of_atoms = element.second;

        if (element_name == "U")
            molar_mass += molar_mass_Uranium * number_of_atoms;
        else if (element_name == "O")
            molar_mass += molar_mass_Oxygen * number_of_atoms;
        else
        {
            for (auto& system : sciantix_system)
            {
                if (system.getGasName() == element_name)
                    molar_mass += system.getGas().getAtomicNumber() * number_of_atoms;
            }
        }

        total_atoms += number_of_atoms;
    }

    theoretical_density = (12 * molar_mass / (total_atoms * avogadro_number * pow(lattice_parameter, 3))); // (g/m3)

    molar_volume =  molar_mass/theoretical_density;

    return molar_volume;
}