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

			std::cout << "Cappellari --------" <<std::endl;

			const double boltzmann_constant = CONSTANT_NUMBERS_H::PhysicsConstants::boltzmann_constant;
			const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

			////////// BOOTH model ////////
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

			double E =  matrix[sma["UO2"]].getElasticModulus() * 1e6; // Pa
			double nu =  matrix[sma["UO2"]].getPoissonRatio();

			double G_gb =  matrix[sma["UO2"]].getGrainBoundaryFractureEnergy()*(1-sciantix_variable[sv["sourcefraction"]].getFinalValue());//*(1-sf_geometrical_parameter*sciantix_variable[sv["sourcefraction"]].getFinalValue()); // J/m2

			// Fracture toughness
			// K_IC = sqrt(elasticmodulus*grainboundaryenergy/(1-poissonratio**2))
			sciantix_variable[sv["Fracture toughness"]].setFinalValue(sqrt(E * G_gb / (1.0 - pow(nu, 2))) * 1e-6); // (MPa m0.5)
			
			// Stress intensification at GB tip
			// kt = 1 + crackdiameter / crackheight
			//double stressintensification = 1 + 2*sin(matrix[sma["UO2"]].getSemidihedralAngle())/(1-cos(matrix[sma["UO2"]].getSemidihedralAngle()));
			double stressintensification = 3.25;
			std::cout << "Stress intensification factor ="<< stressintensification <<std::endl;

			// Equilibrium pressure by capillary pressure and hydrostatic stress
			// P = (2*effective_surface_tension)/bubble radius + Phydrostatic
			double equilibriumpressure = 2.0 * matrix[sma["UO2"]].getSurfaceTension()*(1-cos(matrix[sma["UO2"]].getSemidihedralAngle())) / sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue() -
						history_variable[hv["Hydrostatic stress"]].getFinalValue() * 1e6; //Pa
			sciantix_variable[sv["Equilibrium bubble pressure"]].setFinalValue(equilibriumpressure * 1e-6);

			// JERNKVIST 2019

			// Polynomial fit for the (dimensionless) stress intensity factor
			// @ref Jernkvist 2019: Fi = (+ 0.568Fc**2 + 0.059Fc + 0.5587)
			// double factorJ = (0.568 * pow(sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue(),2) + 0.059 * sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue() + 0.5587);
			
			// Pcrit = Peq + (1/(pi*F))*Kic*sqrt(pi/radius)*(1/kt)  //Pa
			// double critical_bubble_pressureJ = equilibriumpressure + 
				// (1 / (pi*factorJ)) * sciantix_variable[sv["Fracture toughness"]].getFinalValue()*1e6* sqrt(pi/(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue()*sin(matrix[sma["UO2"]].getSemidihedralAngle()))) * 1/stressintensification;    
		
			// THIS WORK

			// Geometrical factor accounting for fractional coverage of the grain face
			// F = 2*(sqrt(1/sqrt(Fc)-1)
			double factor = 2*sqrt(pow(sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue(), -0.5)-1);
			
			// Pcrit = Peq + (1-1/(pi*F))*Kic*sqrt(pi/radius)*(1/kt) //Pa
			double geometrical_factor = 3.28;
			double hc = 1/(1-1/(pi*geometrical_factor*factor));

			//double fracture_stress = sciantix_variable[sv["Fracture toughness"]].getFinalValue()*1e6*sqrt(pi/(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue()*sin(matrix[sma["UO2"]].getSemidihedralAngle())))*(1/stressintensification)*(geometrical_factor)*(1-1/(factor*pi*geometrical_factor));
			double fracture_stress = sciantix_variable[sv["Fracture toughness"]].getFinalValue()*1e6*sqrt(pi/(sciantix_variable[sv["Intergranular bubble radius"]].getFinalValue()*sin(matrix[sma["UO2"]].getSemidihedralAngle())))*(1/stressintensification)*(1/hc)*(1/pi);
			double critical_bubble_pressure = equilibriumpressure + fracture_stress;    //Pa
			
			sciantix_variable[sv["Fracture stress"]].setFinalValue(fracture_stress*1e-6); //MPa
			sciantix_variable[sv["Critical intergranular bubble pressure"]].setFinalValue(critical_bubble_pressure*1e-6); //MPa
			
			double bubble_pressure=0;
			if (sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() !=0)
			{
				bubble_pressure = (boltzmann_constant*history_variable[hv["Temperature"]].getFinalValue() * //Pa
				sciantix_variable[sv["Intergranular atoms per bubble"]].getFinalValue() /
				(sciantix_variable[sv["Intergranular vacancies per bubble"]].getFinalValue() * matrix[sma["UO2"]].getSchottkyVolume()));
			}
			sciantix_variable[sv["Intergranular bubble pressure"]].setFinalValue(bubble_pressure*1e-6); //MPa		
			
			// microcracking parameter
			double a = 1;
			double b = sf_span_parameter;
			double inflection = 1*sf_cent_parameter;
			double microcracking_parameter = a*b*exp(b*(bubble_pressure/(inflection*critical_bubble_pressure)-1))/
				(inflection*critical_bubble_pressure*1e-6*pow(1+a*exp(b*(bubble_pressure/(inflection*critical_bubble_pressure)-1)),2));

			parameter.push_back(microcracking_parameter);

			// healing parameter
			const double healing_parameter = 1.0 / 0.8814; // 1 / (u * burnup)
			parameter.push_back(healing_parameter);

			std::cout << "Grain Boundary Energy (J/m2)= "<< G_gb <<std::endl;
			if (bubble_pressure >= critical_bubble_pressure)
			{
				std::cout <<"WARNING: critical bubble pressure exceeded"<<std::endl;
			}    
			std::cout << "Critical pressure (Pa): "<<critical_bubble_pressure<<std::endl;
			std::cout << "Bubble pressure (Pa): "<<bubble_pressure<<std::endl;
			std::cout << "Fractional coverage (/): " <<sciantix_variable[sv["Intergranular fractional coverage"]].getFinalValue() <<std::endl;

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

