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

std::map<int, std::string> update_sciantix_variable = {
    {0, "Grain radius"},
    {1, "Xe produced"},
    {100, "Xe produced in HBS"},
    {2, "Xe in grain"},
    {92, "Xe in grain HBS"},
    {3, "Xe in intragranular solution"},
    {4, "Xe in intragranular bubbles"},
    {5, "Xe at grain boundary"},
    {6, "Xe released"},
    {7, "Kr produced"},
    {8, "Kr in grain"},
    {9, "Kr in intragranular solution"},
    {10, "Kr in intragranular bubbles"},
    {11, "Kr at grain boundary"},
    {12, "Kr released"},
    {13, "He produced"},
    {14, "He in grain"},
    {15, "He in intragranular solution"},
    {16, "He in intragranular bubbles"},
    {17, "He at grain boundary"},
    {18, "He released"},
    {19, "Intragranular bubble concentration"},
    {20, "Intragranular bubble radius"},
    {21, "Intragranular Xe atoms per bubble"},
    {22, "Intragranular Kr atoms per bubble"},
    {23, "Intragranular He atoms per bubble"},
    {24, "Intragranular gas bubble swelling"},
    {68, "Intragranular gas solution swelling"},
    {25, "Intergranular bubble concentration"},
    {26, "Intergranular Xe atoms per bubble"},
    {27, "Intergranular Kr atoms per bubble"},
    {28, "Intergranular He atoms per bubble"},
    {29, "Intergranular atoms per bubble"},
    {30, "Intergranular vacancies per bubble"},
    {31, "Intergranular bubble radius"},
    {32, "Intergranular bubble area"},
    {33, "Intergranular bubble volume"},
    {34, "Intergranular fractional coverage"},
    {35, "Intergranular saturation fractional coverage"},
    {36, "Intergranular gas swelling"},
    {37, "Intergranular fractional intactness"},
    {38, "Burnup"},
    {39, "Effective burnup"},
    {40, "Fuel density"},
    {41, "U234"},
    {42, "U235"},
    {43, "U236"},
    {44, "U237"},
    {45, "U238"},
    {46, "Intergranular vented fraction"},
    {47, "Intergranular venting probability"},
    {48, "Xe133 produced"},
    {49, "Xe133 in grain"},
    {50, "Xe133 in intragranular solution"},
    {51, "Xe133 in intragranular bubbles"},
    {52, "Xe133 decayed"},
    {53, "Xe133 at grain boundary"},
    {54, "Xe133 released"},
    {55, "Restructured volume fraction"},
    {56, "HBS porosity"},
    {57, "Kr85m produced"},
    {58, "Kr85m in grain"},
    {59, "Kr85m in intragranular solution"},
    {60, "Kr85m in intragranular bubbles"},
    {61, "Kr85m decayed"},
    {62, "Kr85m at grain boundary"},
    {63, "Kr85m released"},
    {64, "Intragranular similarity ratio"},
    {65, "Irradiation time"},
    {66, "Stoichiometry deviation"},
    {67, "Fuel oxygen partial pressure"},
    {69, "FIMA"},
    {80, "HBS pore density"},
    {81, "HBS pore volume"},
    {82, "HBS pore radius"},
    {83, "Xe in HBS pores"},
    {85, "Xe in HBS pores - variance"},
    {86, "Xe atoms per HBS pore"},
    {88, "Xe atoms per HBS pore - variance"},
    {101, "Cs produced"},
    {102, "Cs in grain"},
    {103, "Cs in intragranular solution"},
    {104, "Cs in intragranular bubbles"},
    {105, "Cs at grain boundary"},
    {106, "Cs released"},
    {107, "Cs reacted"},
    {108, "Intragranular Cs atoms per bubble"},
    {109, "Intergranular Cs atoms per bubble"},
    {111, "I produced"},
    {112, "I in grain"},
    {113, "I in intragranular solution"},
    {114, "I in intragranular bubbles"},
    {115, "I at grain boundary"},
    {116, "I released"},
    {117, "I reacted"},
    {118, "Intragranular I atoms per bubble"},
    {119, "Intergranular I atoms per bubble"},
    {121, "CsI"},
    {122, "CsO2"},
    {123, "Cs2O2"},
    {124, "Cs2O"},
    // {125, "CsI at grain boundary"},
    // {126, "CsI released"},
    // {127, "CsI reacted"},
    // {128, "Intragranular CsI atoms per bubble"},
    // {129, "Intergranular CsI atoms per bubble"},
    {150,"Chromium content"},
    {151,"Lattice parameter"},
    {152,"Theoretical density"},
    {153,"Chromium solubility"},
    {154,"Chromia solubility"},
    {155,"Chromium solution"},
    {156,"Chromium precipitate"},
    {157,"Chromia solution"},
    {158,"Chromia precipitate"},
    {160,"Diffusion coefficient"},
};

void Simulation::update(double Sciantix_variables[], double Sciantix_diffusion_modes[])
{
    for (int i = 0; i < n_modes; ++i)
    {
        for (int j = 0; j <= 24; j++)
        {
            Sciantix_diffusion_modes[j * n_modes + i] = modes_initial_conditions[j * n_modes + i];	
        }
    }

    for (std::map<int, std::string>::iterator it = update_sciantix_variable.begin(); it != update_sciantix_variable.end(); it++)
    {
        Sciantix_variables[it->first] = sciantix_variable[it->second].getFinalValue();
    }
}
