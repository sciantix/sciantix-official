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

#include "SetMatrix.h"

void Simulation::setMatrix()
{
    switch (int(input_variable["iFuelMatrix"].getValue()))
    {
        case 0: 
        {
            matrices.push(UO2(matrices, sciantix_variable, history_variable, input_variable));
            break;
        }

        case 1: 
        {
            matrices.push(UO2(matrices, sciantix_variable, history_variable, input_variable));
            matrices.push(UO2HBS(matrices, sciantix_variable, history_variable, input_variable));
            break;
        }

        case 2: 
        {
            matrices.push(MOX(matrices, sciantix_variable, history_variable, input_variable));
            break;
        }


        default:
            ErrorMessages::Switch(__FILE__, "iFuelMatrix", int(input_variable["iFuelMatrix"].getValue()));
            break;
    }
}

Matrix UO2(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable)
{
    Matrix matrix_;

    matrix_.setName("UO2");
    matrix_.setRef("\n\t");
    matrix_.setTheoreticalDensity(10960.0); // (kg/m3)
    matrix_.setLatticeParameter(5.47e-10);
    matrix_.setGrainBoundaryMobility(int(input_variable["iGrainGrowth"].getValue()), history_variable);
    matrix_.setSurfaceTension(0.7); // (N/m)
    matrix_.setFissionFragmentInfluenceRadius(1.0e-9); // (m)
    matrix_.setFissionFragmentRange(6.0e-6); // (m)
    matrix_.setSchottkyVolume(4.09e-29); // (m3)
    matrix_.setOctahedralInterstitialSite(7.8e-30); // (m3)
    matrix_.setSemidihedralAngle(0.872664626); // (rad)
    matrix_.setGrainBoundaryThickness(5.0e-10); // (m)
    matrix_.setLenticularShapeFactor(0.168610764);
    matrix_.setGrainRadius(sciantix_variable["Grain radius"].getFinalValue()); // (m)
    matrix_.setHealingTemperatureThreshold(1273.15); // K
    matrix_.setGrainBoundaryVacancyDiffusivity(int(input_variable["iGrainBoundaryVacancyDiffusivity"].getValue()), history_variable); // (m2/s)
    matrix_.setPoreNucleationRate(sciantix_variable);
    matrix_.setPoreResolutionRate(sciantix_variable, history_variable);
    matrix_.setPoreTrappingRate(matrices, sciantix_variable);
    matrix_.setChromiumSolubility(0); // (weight%/UO2)	
	matrix_.setChromiaSolubility(0); // (weight%/UO2)	
	matrix_.setChromiumSolution(0); // (at/m3)
	matrix_.setChromiumPrecipitate(0); // (at/m3)
	matrix_.setChromiaSolution(0); // (at/m3)
	matrix_.setChromiaPrecipitate(0); // (at/m3)

    // Mechanical properties
    matrix_.setElasticModulus(2.237e5 * (1 - 2.6 * sciantix_variable["Porosity"].getFinalValue()) * (1 - 1.394e-4 * (history_variable["Temperature"].getFinalValue()-273-20)) * (1 - 0.1506 * (1 - exp(-0.035*sciantix_variable["Burnup"].getFinalValue())))); // (MPa) TRANSURANUS manual
	if (sciantix_variable["Porosity"].getFinalValue() >= 0.2){
		std::cout<<"WARNING: elastic modulus correlation used outside the validity range for fuel porosity (P<0.2)"<<std::endl;
		std::cout<<"Porosity P = "<<sciantix_variable["Porosity"].getFinalValue()<<std::endl;
	}
	matrix_.setPoissonRatio(0.32); // (/) TRANSURANUS manual
	matrix_.setGrainBoundaryFractureEnergy(2); // (J/m2) Jernkvist, L.O. (2020). A review of analytical criteria for fission gas induced fragmentation of oxide fuel in accident conditions. Progress in Nuclear Energy, 119, 103188.
    matrix_.setShearModulus(matrix_.getElasticModulus() / ( 2 * ( 1 + matrix_.getPoissonRatio() ) )); // (MPa)

    return matrix_;
}

Matrix UO2HBS(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable)
{
    Matrix matrix_;
    
    matrix_.setName("UO2HBS");
    matrix_.setRef("\n\t");
    matrix_.setTheoreticalDensity(10960.0); // (kg/m3)
    matrix_.setLatticeParameter(5.47e-10);
    matrix_.setGrainBoundaryMobility(0, history_variable);
    matrix_.setSurfaceTension(0.7); // (N/m)
    matrix_.setFissionFragmentInfluenceRadius(1.0e-9); // (m)
    matrix_.setFissionFragmentRange(6.0e-6); // (m)
    matrix_.setSchottkyVolume(4.09e-29);
    matrix_.setOctahedralInterstitialSite(7.8e-30);
    matrix_.setSemidihedralAngle(0.0);
    matrix_.setGrainBoundaryThickness(0.0);
    matrix_.setLenticularShapeFactor(0.168610764);
    matrix_.setGrainRadius(150e-9); // (m)
    matrix_.setHealingTemperatureThreshold(1273.15); // K
    matrix_.setGrainBoundaryVacancyDiffusivity(5, history_variable); // (m2/s)
    matrix_.setPoreNucleationRate(sciantix_variable);
    matrix_.setPoreResolutionRate(sciantix_variable, history_variable);
    matrix_.setPoreTrappingRate(matrices, sciantix_variable);

    // Mechanical properties
    matrix_.setElasticModulus(2.237e5 * (1 - 2.6 * sciantix_variable["HBS porosity"].getFinalValue()) * (1 - 1.394e-4 * (history_variable["Temperature"].getFinalValue()-273-20)) * (1 - 0.1506 * (1 - exp(-0.035*sciantix_variable["Burnup"].getFinalValue())))); // (MPa) TRANSURANUS manual
	if (sciantix_variable["HBS porosity"].getFinalValue()>=0.2){
		std::cout<<"WARNING: elastic modulus correlation used outside the validity range for fuel porosity (P<0.2)"<<std::endl;
		std::cout<<"Porosity P (/) = "<< sciantix_variable["HBS porosity"].getFinalValue() <<std::endl;
	}
	matrix_.setPoissonRatio(0.32); // (/) TRANSURANUS manual
	matrix_.setGrainBoundaryFractureEnergy(2); // (J/m2) Jernkvist, L.O. (2020). A review of analytical criteria for fission gas induced fragmentation of oxide fuel in accident conditions. Progress in Nuclear Energy, 119, 103188.
    matrix_.setShearModulus(matrix_.getElasticModulus() / ( 2 * ( 1 + matrix_.getPoissonRatio() ) )); // (MPa)

    return matrix_;
}

// MOX

Matrix MOX(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, 
    SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable)
{ 
    Matrix matrix_;

    matrix_.setName("MOX");
    matrix_.setRef("\n\t");

    
    double q = sciantix_variable["q"].getFinalValue(); // q ratio (Olander, 1976) defines MOX composition

    // Pu and U fractions from q = Pu / (U + Pu)
    double Pu_fraction = q; // mole fraction of Pu
    double U_fraction  = 1.0 - q; // mole fraction of U


    // isotopic composition
    matrix_.setInitialUraniumComposition({ 
        U_fraction * double(sciantix_variable["U234"].getFinalValue()),
        U_fraction * double(sciantix_variable["U235"].getFinalValue()),
        U_fraction * double(sciantix_variable["U236"].getFinalValue()),
        U_fraction * double(sciantix_variable["U237"].getFinalValue()),
        U_fraction * double(sciantix_variable["U238"].getFinalValue())
    });

    matrix_.setInitialPlutoniumComposition({
        Pu_fraction * double(sciantix_variable["Pu238"].getFinalValue()),
        Pu_fraction * double(sciantix_variable["Pu239"].getFinalValue()),
        Pu_fraction * double(sciantix_variable["Pu240"].getFinalValue()),
        Pu_fraction * double(sciantix_variable["Pu241"].getFinalValue()),
        Pu_fraction * double(sciantix_variable["Pu242"].getFinalValue())
    });

     // Pu enrichment (q)
    matrix_.setMoxPuEnrichment(Pu_fraction);

     // theoretical density [kg/m3], formula from the original code MOX.C
    matrix_.setTheoreticalDensity(10960.0 + 490 * 0.20); // 20% PuO2 (kg/m3)

    // microstructural and thermal parameters
    matrix_.setGrainBoundaryMobility(int(input_variable["iGrainGrowth"].getValue()), history_variable);
    matrix_.setSurfaceTension(0.626); // N/m (reference Kitano)
    matrix_.setFissionFragmentInfluenceRadius(1.0e-9); // m
    matrix_.setFissionFragmentRange(6.0e-6); // m
    matrix_.setSchottkyVolume(4.09e-29); // m3
    matrix_.setOctahedralInterstitialSite(7.8e-30); // m3 (verificare fonte)
    matrix_.setSemidihedralAngle(0.97); // rad
    matrix_.setGrainBoundaryThickness(5.0e-10); // m
    matrix_.setLenticularShapeFactor(0.168610764); // function

    // lattice parameter: convertion in [m]
    // original formula: 5.47 - 0.074 * enrichment  [Å]
    matrix_.setLatticeParameter(5.47e-10 - 0.074e-10 * 0.20); // 20% PuO2 (m)

    matrix_.setGrainRadius(sciantix_variable["Grain radius"].getFinalValue()); // m
    matrix_.setHealingTemperatureThreshold((2744.0 + 273.15) / 2.0); // K (as original code: half of the melting temperature by now)

    // grain boundary vacancy diffusivity (updated with history_variable)
    matrix_.setGrainBoundaryVacancyDiffusivity(int(input_variable["iGrainBoundaryVacancyDiffusivity"].getValue()), history_variable); // m2/s

    // pores (using the updated versions as in UO₂)
    matrix_.setPoreNucleationRate(sciantix_variable);
    matrix_.setPoreResolutionRate(sciantix_variable, history_variable);
    matrix_.setPoreTrappingRate(matrices, sciantix_variable);

    // Mechanical properties
    matrix_.setElasticModulus((217.99 * (1 - q) + 249.54 * q) * ((pow(1 - sciantix_variable["Porosity"].getFinalValue(), 2)) / (1 + 1.124330 * sciantix_variable["Porosity"].getFinalValue())) * (1 - 2.72739 * sciantix_variable["Stoichiometry deviation"].getFinalValue() + 13.84392 * pow(sciantix_variable["Stoichiometry deviation"].getFinalValue(), 2)) * (1.012527 - 2.282 * pow(10, -5) * history_variable["Temperature"].getFinalValue() - 6.312 * pow(10, -8) * pow(history_variable["Temperature"].getFinalValue(), 2))); // pag 124 Nuclear Science NEA/NSC/R(2024)1
	matrix_.setPoissonRatio((0.32051 * (1 - q) + 0.31882 * q) * (1 - 1.03223 * sciantix_variable["Porosity"].getFinalValue()) * (1 + 0.69962 * sciantix_variable["Stoichiometry deviation"].getFinalValue() - 7.52905 * pow(sciantix_variable["Stoichiometry deviation"].getFinalValue(), 2)) * (1.017906 - 6.420 * pow(10, -5) * history_variable["Temperature"].getFinalValue() + 1.506 * pow(10, -8) * pow(history_variable["Temperature"].getFinalValue(),2))); // pag 124 Nuclear Science NEA/NSC/R(2024)1
	matrix_.setGrainBoundaryFractureEnergy(2); // (J/m2) Jernkvist, L.O. (2020). A review of analytical criteria for fission gas induced fragmentation of oxide fuel in accident conditions. Progress in Nuclear Energy, 119, 103188.
    matrix_.setShearModulus(matrix_.getElasticModulus() / ( 2 * ( 1 + matrix_.getPoissonRatio() ) )); // pag 125 Nuclear Science NEA/NSC/R(2024)1

    return matrix_;
}
