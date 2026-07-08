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
//  Version: 2.2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "Simulation.h"

std::map<int, std::string> update_sciantix_variable = {
    {0, "Grain radius"},
    {1, "Xe produced"},
    {100, "Xe produced in HBS"}, // not present
    {2, "Xe in grain"},
    {92, "Xe in grain HBS"}, // not present
    {3, "Xe in intragranular solution"},
    {4, "Xe in intragranular bubbles"},
    {5, "Xe at grain boundary"},
    {6, "Xe released"}, // tu calculated
    {7, "Kr produced"},
    {8, "Kr in grain"},
    {9, "Kr in intragranular solution"},
    {10, "Kr in intragranular bubbles"},
    {11, "Kr at grain boundary"},
    {12, "Kr released"}, // tu calculated
    {13, "He produced"},
    {14, "He in grain"},
    {15, "He in intragranular solution"},
    {16, "He in intragranular bubbles"},
    {17, "He at grain boundary"},
    {18, "He released"}, // tu calculated
    {19, "Intragranular bubble concentration"},
    {20, "Intragranular bubble radius"},
    {21, "Intragranular Xe atoms per bubble"}, // in sciantix
    {22, "Intragranular Kr atoms per bubble"}, // in sciantix
    {23, "Intragranular He atoms per bubble"}, // in sciantix
    {24, "Intragranular gas bubble swelling"},// not present
    {68, "Intragranular gas solution swelling"}, // not present
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
    {38, "Burnup"}, // not present
    {39, "Effective burnup"}, // not present
    {40, "Fuel density"},
    {41, "U234"}, // not present --> added
    {42, "U235"}, // not present --> added
    {43, "U236"}, // not present --> added
    {44, "U237"}, // not present --> added
    {45, "U238"}, // not present --> added
    {46, "Intergranular vented fraction"}, // not present
    {47, "Intergranular venting probability"}, 
    {48, "Xe133 produced"},
    {49, "Xe133 in grain"},
    {50, "Xe133 in intragranular solution"},
    {51, "Xe133 in intragranular bubbles"},
    {52, "Xe133 decayed"},
    {53, "Xe133 at grain boundary"},
    {54, "Xe133 released"}, // tu calculated
    {55, "Restructured volume fraction"}, // not present
    {56, "HBS porosity"}, // not present
    {57, "Kr85m produced"}, // not present
    {58, "Kr85m in grain"}, // not present
    {59, "Kr85m in intragranular solution"}, // not present
    {60, "Kr85m in intragranular bubbles"}, // not present
    {61, "Kr85m decayed"}, // not present
    {62, "Kr85m at grain boundary"}, // not present
    {63, "Kr85m released"}, // not present
    {64, "Intragranular similarity ratio"}, // not present
    {65, "Irradiation time"}, // not present
    {66, "Stoichiometry deviation"}, // not present --> added
    {67, "Fuel oxygen partial pressure"}, // not present
    {69, "FIMA"}, // not present
    {70, "Porosity"},
    {71, "Fabrication porosity"},
    {72, "Open porosity"},
    {73, "Residual porosity"},
    {74, "Densification factor"}, // not present
    {80, "HBS pore density"}, // not present
    {81, "HBS pore volume"}, // not present
    {82, "HBS pore radius"}, // not present
    {83, "Xe in HBS pores"}, // not present
    {85, "Xe in HBS pores - variance"}, // not present
    {86, "Xe atoms per HBS pore"}, // not present
    {88, "Xe atoms per HBS pore - variance"}, // not present
    // CODE DEVELOPMENT : TO BE REDUCED // not present
    {101, "Cs produced"}, // added
    {102, "Cs in grain"}, // added
    {103, "Cs in intragranular solution"}, // added
    {104, "Cs in intragranular bubbles"}, // added
    {105, "Cs at grain boundary"}, // added, it accounts also for reacted in TU
    {106, "Cs released"}, // tu calculated
    {107, "Cs reacted"}, //added
    {108, "Intragranular Cs atoms per bubble"},  // in sciantix
    {109, "Intergranular Cs atoms per bubble"},  // in sciantix
#if defined(COUPLING_TU)
    {110, "Cs in the gap"},
#endif
    {111, "I produced"}, // not added
    {112, "I in grain"}, // not added
    {113, "I in intragranular solution"}, // not added
    {114, "I in intragranular bubbles"}, // not added
    {115, "I at grain boundary"}, // not added
    {116, "I released"}, // not added
    {117, "I reacted"}, // not added
    {118, "Intragranular I atoms per bubble"}, // not added
    {119, "Intergranular I atoms per bubble"}, // not added
    {121, "Te produced"}, // not added
    {122, "Te in grain"}, // not added
    {123, "Te in intragranular solution"}, // not added
    {124, "Te in intragranular bubbles"}, // not added
    {125, "Te at grain boundary"}, // not added
    {126, "Te released"}, // not added
    {127, "Te reacted"}, // not added
    {128, "Intragranular Te atoms per bubble"}, // not added
    {129, "Intergranular Te atoms per bubble"}, // not added
    // Non-volatile FPs
    {130, "Mo produced"}, // added
    {131, "Mo in solution"}, // not added, calculated
    {132, "Mo reacted"}, // added
    {133, "Ba produced"}, // to be added
    {134, "Ba in solution"}, // not added, calculated
    {135, "Ba reacted"}, // to be added
    {136, "Tc produced"}, // added
    {137, "Tc in solution"}, // not added, calculated
    {138, "Tc reacted"}, // added
    {139, "Ru produced"}, // added
    {140, "Ru in solution"}, // not added, calculated
    {141, "Ru reacted"}, // added
    {142, "Rh produced"}, // added
    {143, "Rh in solution"}, // not added, calculated
    {144, "Rh reacted"}, // added
    {145, "Pd produced"}, // added
    {146, "Pd in solution"}, // not added, calculated
    {147, "Pd reacted"}, // added
    // Chromium
    {150, "Chromium content"}, // not present
    {151, "Lattice parameter"}, // not present
    {152, "Theoretical density"}, // not present
    {153, "Chromium solubility"}, // not present
    {154, "Chromia solubility"}, // not present
    {155, "Chromium solution"}, // not present
    {156, "Chromium precipitate"}, // not present
    {157, "Chromia solution"}, // not present
    {158, "Chromia precipitate"}, // not present
    {160, "Diffusion coefficient"}, // not present
    // Matrix
    {161, "U content"}, // added
    {162, "O content"}, // added
    {163, "Pu content"}, // added
    {164, "Fuel oxygen potential"}, // added
    {166, "O available content"}, // added
    {171, "Pu238"}, // added
    {172, "Pu239"}, // added
    {173, "Pu240"}, // added
    {174, "Pu241"}, // added
    {175, "Pu242"}, // added
    {177, "q"}, // added
    // 
    {180, "Mo/Ru in HCP_A3"},
    {181, "Mo in oxide fraction"},
    {182, "Mo oxide valence"},
    {183, "Ba/Mo in oxide compounds"},
    {184, "Ba in oxide fraction"},
    {185, "Ba oxide valence"},
    //
    {190, "JOG (Cs2MoO4)"},
    {191, "JOG (BaMoO4)"},
    {192, "JOG (liquid)"},
    {194, "JOG (Ba3MoO6)"},
    {195, "JOG (Ba2MoO5)"},
};
 
// CODE DEVELOPMENT: DIFFUSION MODES FROM 17 TO 26
void Simulation::update(double Sciantix_variables[], double Sciantix_diffusion_modes[], double Sciantix_thermochemistry[])
{
    for (int i = 0; i < n_modes; ++i)
    {
        for (int j = 0; j <= 26; j++)
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

    // CODE DEVELOPMENT : update thermochemistry variables
    if (thermochemistry_variable.empty())
        return;

    // Theoretical densities are packed at the same manifest index, offset by
    // thermochemistry_density_offset, in the same shared Sciantix_thermochemistry
    // array (read by TU as sciantix_thermochemistry(itb + 123), see
    // SetTUVariablesfromSciantix.f95). The offset must be >= the manifest's
    // entry count (indices are contiguous from 0, enforced in
    // ThermochemistryManifest.C) so a variable's own value slot never
    // collides with another variable's density slot.
    constexpr int thermochemistry_density_offset = 123;
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
