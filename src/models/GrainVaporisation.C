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
    Matrix fuel(matrices[0]);

    double temperature = history_variable["Temperature"].getFinalValue(); // K
    double x = sciantix_variable["Stoichiometry deviation"].getFinalValue();
    double pressure = history_variable["THERMOCHIMICA pressure"].getFinalValue(); // Pa

    // Blackburn for UO2 fuel
    double n_u(0), n_u2(0), n_u4(0), n_u6(0), n_o(0), n_o2(2 + x);
    if ((sciantix_variable["Oxygen content"].getFinalValue() > 0) && (sciantix_variable["Uranium content"].getFinalValue() > 0))
    {
        // Stoichiometry regime
        const double epsilon = 1e-6; // Tolerance
        bool is_hypo = (x < - epsilon);
        bool is_hyper = (x > epsilon);
        
        // Constants from Blackburn's model, 1973
        if (is_hypo)
        {
            n_u2 = (- x);
            n_u4 = (1 + x);
        }
        else
        {
            n_u4 = (1 - x);
            n_u6 = (x);
        }
    }
    else if ((sciantix_variable["Oxygen content"].getFinalValue() <= 0) && (sciantix_variable["Uranium content"].getFinalValue() > 0)) n_u = 1;
    else if ((sciantix_variable["Oxygen content"].getFinalValue() > 0) && (sciantix_variable["Uranium content"].getFinalValue() <= 0)) n_o = 1;
    else return;
    
    thermochemistry_variable["UO (solid, matrix)"].setFinalValue(n_u2*sciantix_variable["Uranium content"].getFinalValue());
    thermochemistry_variable["UO2 (solid, matrix)"].setFinalValue(n_u4*sciantix_variable["Uranium content"].getFinalValue());
    thermochemistry_variable["UO3 (solid, matrix)"].setFinalValue(n_u6*sciantix_variable["Uranium content"].getFinalValue());
    thermochemistry_variable["U (solid, matrix)"].setFinalValue(n_u*sciantix_variable["Uranium content"].getFinalValue());
    thermochemistry_variable["O2 (solid, matrix)"].setFinalValue(n_o/2*sciantix_variable["Oxygen content"].getFinalValue());

    // Knudsen cell setup: skip if pressure is too high
    if (pressure >= 1e6) return;

    // Constants form Olander (Fundamental aspects of nuclear reactor fuel elements, Blackburn's model, Table 11.1 pag 158)
    double p_uo  = n_o2 * n_u2 * exp(- 49500/temperature + 11.9);
    double p_uo2 = pow(n_o2, 2) * n_u4 * exp(- 74000/temperature + 19.9);
    double p_uo3 = pow(n_o2, 3) * n_u6 * exp(- 44000/temperature + 11.9);
    double p_u   = n_u * exp(- 58000/temperature + 13.5);
    double p_o2 = pow(n_o, 2) * exp((- 897000 + 224.8*temperature)/(gas_constant*temperature));

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
        {"UO3 (vapour, matrix)",{p_uo3, "UO3 (solid, matrix)", {{"U", n_u6},{"O", n_o2}}}}
    };

    double volumetric_flux(0.0);
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
            double conversion_factor = 1e5/pressure; // atm --> pascal 
            double flux_i =  pow(2 * M_PI * gas_constant * temperature, -0.5) * (data.partialpressure*conversion_factor/pow(molar_mass_i, 0.5)); // mol m-2 s-1
            double increment = flux_i*surface*physics_variable["Time step"].getFinalValue(); // mol
            
            double mol_u = compound.getStoichiometry().find("U")->second;
            double mol_o = compound.getStoichiometry().find("O")->second;

            increment = std::min(increment, thermochemistry_variable[data.solidname].getFinalValue());
            if (mol_u != 0) increment = std::min(increment, sciantix_variable["Uranium content"].getFinalValue()/mol_u);
            if (mol_o != 0) increment = std::min(increment, sciantix_variable["Oxygen content"].getFinalValue()/mol_o);
            
            if ((increment/surface) > 0)
            {
                compound.setFinalValue(compound.getInitialValue() + increment);

                sciantix_variable["Uranium content"].addValue(- mol_u*increment);
                sciantix_variable["Oxygen content"].addValue(- mol_o*increment);

                volumetric_flux += (molar_mass_i/sciantix_variable["Fuel density"].getFinalValue())*increment/surface;
            }
        }
    }

    double removeterm(0); 
    if ((sciantix_variable["Oxygen content"].getFinalValue() > 0) && (sciantix_variable["Uranium content"].getFinalValue() > 0)) 
        removeterm = (sciantix_variable["Uranium content"].getFinalValue()*sciantix_variable["Oxygen content"].getIncrement() - sciantix_variable["Oxygen content"].getFinalValue()*sciantix_variable["Uranium content"].getIncrement())/pow(sciantix_variable["Uranium content"].getFinalValue(),2);
    sciantix_variable["Stoichiometry deviation"].setFinalValue(x + removeterm);

    if (volumetric_flux > 0); sciantix_variable["Grain radius"].setFinalValue(sciantix_variable["Grain radius"].getFinalValue() - volumetric_flux);

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
        input_variable["iThermochimica"].setValue(0);
        input_variable["iGrainBoundaryMicroCracking"].setValue(0);
        input_variable["iGrainBoundaryBehaviour"].setValue(0);
        input_variable["iGrainBoundaryVenting"].setValue(0);
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

    // Iterate through the stoichiometry map to calculate the molar mass
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