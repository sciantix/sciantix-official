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

void Simulation::CsIProduction()
{
    double T = history_variable["Temperature"].getFinalValue();
    double thermalenergy =  boltzmann_constant*T;
    double Cs = sciantix_variable["Cs at grain boundary"].getFinalValue();
    double I = sciantix_variable["I at grain boundary"].getFinalValue();
    double CsI = sciantix_variable["CsI at grain boundary"].getFinalValue();

    double A = (-523534);
    double B = (419.454);
    double C = (-29.576);
    double D = (0.0000675);
    double E = (0);
    double G_o =  A + B*T + C*T*log(T) + D*pow(T,2) + E/T;

    double a = -G_o/avogadro_number;
    double b = G_o/avogadro_number*(Cs + I) -1;
    double c = -Cs*I*G_o/avogadro_number;

    double delta = pow(b,2) - 4*a*c;
    std::cout<<"----------------"<<std::endl;
    std::cout<<Cs<<std::endl;
    std::cout<<I<<std::endl;
    std::cout<<CsI<<std::endl;

    double z1 = 0;
    double z2 = 0;

    if (delta >= 0)
    {   
        z1 = (-b + sqrt(delta))/(2*a);
        std::cout<<"Z1="<<z1<<std::endl;
        z2 = (-b - sqrt(delta))/(2*a);
        std::cout<<"Z2="<<z2<<std::endl;
    }

    sciantix_variable["Cs at grain boundary"].setFinalValue(Cs - (z2-CsI));
    sciantix_variable["Cs reacted"].addValue(z2-CsI);
    sciantix_variable["I at grain boundary"].setFinalValue(I - (z2-CsI));
    sciantix_variable["I reacted"].addValue(z2-CsI);
    sciantix_variable["CsI at grain boundary"].setFinalValue(CsI + (z2-CsI));
    sciantix_variable["CsI produced"].addValue(z2-CsI);
}