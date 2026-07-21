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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"
#include "ThermochemistryManifest.h"

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
    {70, "Porosity"},
    {71, "Fabrication porosity"},
    {72, "Open porosity"},
    {73, "Residual porosity"},
    {74, "Densification factor"},
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
#if defined(COUPLING_TU)
    {110, "Cs in the gap"},
#endif
    // Non-volatile FPs
    {130, "Mo produced"},
    {131, "Mo in solution"},
    {132, "Mo reacted"},
    {133, "Ba produced"}, 
    {134, "Ba in solution"},
    {135, "Ba reacted"}, 
    {136, "Tc produced"},
    {137, "Tc in solution"},
    {138, "Tc reacted"},
    {139, "Ru produced"},
    {140, "Ru in solution"},
    {141, "Ru reacted"},
    {142, "Rh produced"},
    {143, "Rh in solution"},
    {144, "Rh reacted"},
    {145, "Pd produced"},
    {146, "Pd in solution"},
    {147, "Pd reacted"},
    // Chromium
    {150, "Chromium content"},
    {151, "Lattice parameter"},
    {152, "Theoretical density"},
    {153, "Chromium solubility"},
    {154, "Chromia solubility"},
    {155, "Chromium solution"},
    {156, "Chromium precipitate"},
    {157, "Chromia solution"},
    {158, "Chromia precipitate"},
    {160, "Diffusion coefficient"},
    // Matrix
    {161, "U content"},
    {162, "O content"},
    {163, "Pu content"},
    {164, "Fuel oxygen potential"},
    {166, "O available content"},
    {171, "Pu238"},
    {172, "Pu239"},
    {173, "Pu240"},
    {174, "Pu241"},
    {175, "Pu242"},
    {177, "q"},
};
 
void Simulation::update(double Sciantix_variables[], double Sciantix_diffusion_modes[], double Sciantix_thermochemistry[])
{
    for (int i = 0; i < n_modes; ++i)
    {
        for (int j = 0; j <= 17; j++)
        {
            Sciantix_diffusion_modes[j * n_modes + i] = modes_initial_conditions[j * n_modes + i];
        }
    }

    for (std::map<int, std::string>::iterator it = update_sciantix_variable.begin();
         it != update_sciantix_variable.end();
         it++)
    {
        Sciantix_variables[it->first] = sciantix_variable[it->second].getFinalValue();
    }

    if (thermochemistry_variable.empty())
        return;

    // thermochemistry_density_offset (declared in ThermochemistryManifest.h, enforced against
    // the manifest's entry count in LoadThermochemistryManifest) packs theoretical densities at
    // the same manifest index, offset by that value, in the same shared Sciantix_thermochemistry
    // array (read by TU as sciantix_thermochemistry(itb + 123), see SetTUVariablesfromSciantix.f95).
    constexpr int thermochemistry_array_size = 300;

    for (auto& variable : thermochemistry_variable)
    {
        Sciantix_thermochemistry[variable.getIndex()] = variable.getFinalValue();

        // Entries without a meaningful density (e.g. the derived
        // liquid-composition site fractions) still get a slot reserved by
        // the offset above; guard against overrunning Sciantix_thermochemistry
        // if the manifest ever grows close to its 300-slot budget.
        const int density_index = variable.getIndex() + thermochemistry_density_offset;
        if (density_index < thermochemistry_array_size)
            Sciantix_thermochemistry[density_index] = variable.getTheoreticalDensity();
    }
}
