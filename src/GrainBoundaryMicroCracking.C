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
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "GrainBoundaryMicroCracking.h"


void GrainBoundaryMicroCracking()
{
	if (!input_variable[iv["iGrainBoundaryMicroCracking"]].getValue()) return;

	switch (int(input_variable[iv["iGrainBoundaryMicroCracking"]].getValue()))
	{
		case 1:
		{
			model.emplace_back();
			int model_index = int(model.size()) - 1;
			model[model_index].setName("Grain-boundary micro-cracking");
			std::vector<double> parameter;

			std::cout << "Barani --------" <<std::endl;

			const double dTemperature = history_variable[hv["Temperature"]].getIncrement();

			const bool heating = (dTemperature > 0.0) ? 1 : 0;
			const double transient_type = heating ? +1.0 : -1.0;
			const double span = 10.0;

			// microcracking parameter
			const double inflection = 1773.0 + 520.0 * exp(-sciantix_variable[sv["Burnup"]].getFinalValue() / (10.0 * 0.8814));
			const double exponent = 33.0;
			const double arg = (transient_type / span) * (history_variable[hv["Temperature"]].getFinalValue() - inflection);
			const double microcracking_parameter = (transient_type / span) * exp(arg) * pow((exponent * exp(arg) + 1), -1. / exponent - 1.); // dm/dT

			parameter.push_back(microcracking_parameter);

			// healing parameter
			const double healing_parameter = 1.0 / 0.8814; // 1 / (u * burnup)
			parameter.push_back(healing_parameter);

			model[model_index].setParameter(parameter);
			model[model_index].setRef("from Barani et al. (2017), JNM");
			
			break;
		}
		case 2:
		{
			model.emplace_back();
			int model_index = int(model.size()) - 1;
			model[model_index].setName("Grain-boundary micro-cracking");
			std::vector<double> parameter;

			const double boltzmann_constant = CONSTANT_NUMBERS_H::PhysicsConstants::boltzmann_constant;
			const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

			///////////////////// BOOTH model - not used //////////////////////////////7
			double FGRi(0.0), FGRf(0.0), FGRincrement(0.0);
			
			double Tf = history_variable[hv["Temperature"]].getFinalValue();
			double Ff = history_variable[hv["Fission rate"]].getFinalValue();

			double df = 7.6e-10 * exp(-4.86e-19 / (boltzmann_constant * Tf)) + 4.0 * 1.41e-25 * sqrt(Ff) * exp(-1.91e-19 / (boltzmann_constant * Tf)) + 8.0e-40 * Ff;

			FGRi = pow(sciantix_variable[sv["sourcefraction"]].getInitialValue(),2);//*df/di;
			if (Ff <= 1e15)
			{
				FGRincrement = 36.0 * (df) *physics_variable[pv["Time step"]].getFinalValue()/(pi*pow(sciantix_variable[sv["Grain radius"]].getFinalValue(),2));
			}
			else
			{
				FGRincrement = 16.0 * (df) *physics_variable[pv["Time step"]].getFinalValue()/(pi*pow(sciantix_variable[sv["Grain radius"]].getFinalValue(),2));
			}
			FGRf = pow(FGRi+FGRincrement,0.5);
			
			if (FGRf<=0.75)
			{
				sciantix_variable[sv["sourcefraction"]].setFinalValue(FGRf);
			}
			else
			{
				sciantix_variable[sv["sourcefraction"]].setFinalValue(0.75);
			}
			/////////////////////////////////////////////////////////////////////////

			// Material properties
			double E =  matrix[sma["UO2"]].getElasticModulus() * 1e6; // Pa
			double nu =  matrix[sma["UO2"]].getPoissonRatio();
			double G_gb =  matrix[sma["UO2"]].getGrainBoundaryFractureEnergy(); // J/m2

			// Fracture toughness
			// K_IC = sqrt(elasticmodulus*grainboundaryenergy/(1-poissonratio**2))
			sciantix_variable[sv["Fracture toughness"]].setFinalValue(sqrt(E * G_gb / (1.0 - pow(nu, 2))) * 1e-6); // (MPa m0.5)
			
			// Stress intensification at GB tip
			//
			// By theoretical calculations: kt = 1 + crackdiameter / crackheight
			//double stressintensification = 1 + 2*sin(matrix[sma["UO2"]].getSemidihedralAngle())/(1-cos(matrix[sma["UO2"]].getSemidihedralAngle()));
			//
			// By ABAQUS 3D fitting: kt = 3.25
			double stressintensification = 3.25; 

			// Equilibrium pressure by capillary pressure and hydrostatic stress
			// P = (2*effective_surface_tension)/bubble radius + Phydrostatic
			double equilibriumpressure = 2.0 * matrix[sma["UO2"]].getSurfaceTension()*(1-cos(matrix[sma["UO2"]].getSemidihedralAngle())) / sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() -
						history_variable[hv["Hydrostatic stress"]].getFinalValue() * 1e6; //Pa
			sciantix_variable[sv["Equilibrium bubble pressure"]].setFinalValue(equilibriumpressure * 1e-6);

			// Geometrical factor accounting for fractional coverage of the grain face h(F_c, Y)
			// defined only if F_c > 0 otherwise tends to 1. 
			double geometrical_factor = 3.28; // = Y
			double hc(1.0);
			if (sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue()>0.01)
			{
				hc = 1/(1-1/(pi*geometrical_factor*(2*sqrt(pow(sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue(), -0.5)-1))));
			}

			// Pcrit = Peq + fracture stress
			//
			// fracture stress = (1-1/(pi*F))*Kic*sqrt(pi/radius)*(1/kt) //Pa
			double fracture_stress = sciantix_variable[sv["Fracture toughness"]].getFinalValue()*1e6*sqrt(pi/(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue()*sin(matrix[sma["UO2"]].getSemidihedralAngle())))*(1/stressintensification)*(1/hc)*(1/pi);
			sciantix_variable[sv["Fracture stress"]].setFinalValue(fracture_stress*1e-6); //MPa
			// Critical pressure: 
			double critical_bubble_pressure = equilibriumpressure + fracture_stress;   //Pa
			sciantix_variable[sv["Critical intergranular bubble pressure"]].setFinalValue(critical_bubble_pressure*1e-6); //MPa
			
			// Upper limit for atom-to-vacancy ratio:  it limits the pressure value
			double maxatompervacancy = 1.0*sf_dummy;
			double atompervacancyf = 0.0;
			if (sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() !=0)
			{			
				atompervacancyf = sciantix_variable[sv["Intergranular atoms per bubble"]].getFinalValue() / sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue();
			}
			if (atompervacancyf > maxatompervacancy)
			{
				atompervacancyf = maxatompervacancy;
			}

			// Bubble gas pressure
			double bubble_pressure  = (boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue() * atompervacancyf / matrix[sma["UO2"]].getSchottkyVolume()); //Pa
			sciantix_variable[sv["Intergranular bubble pressure"]].setFinalValue(bubble_pressure*1e-6); //MPa		
			
			double microcracking_parameter = 0.0;
			if (bubble_pressure > critical_bubble_pressure && bubble_pressure > equilibriumpressure)
				microcracking_parameter = (bubble_pressure-equilibriumpressure)*1e-6*sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue()/G_gb;
			
			// Microcracking parameter
			parameter.push_back(microcracking_parameter);

			// healing parameter
			const double healing_parameter = 1.0 / 0.8814; // 1 / (u * burnup)
			parameter.push_back(healing_parameter);

			model[model_index].setParameter(parameter);
			model[model_index].setRef("Under development");

			break;
		}
		default:
		{
			ErrorMessages::Switch(__FILE__, "iGrainBoundaryMicroCracking", int(input_variable[iv["iGrainBoundaryMicroCracking"]].getValue()));
			break;
		}
	}
}

