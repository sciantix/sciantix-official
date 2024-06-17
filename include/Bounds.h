#include <map>
#include <string>
#include <iostream>
#include <fstream>
#include "ErrorMessages.h"

namespace Configuration
{
    struct Bounds
    {
        int min;
        int max;
    };

    std::map<int, std::pair<std::string, Bounds>> boundsMap = {
        {0, {"Grain radius", {0, 1000}}},
        {1, {"Xe produced", {0, 1000}}},
        {2, {"Xe in grain", {0, 1000}}},
        {3, {"Xe in intragranular solution", {0, 1000}}},
        {4, {"Xe in intragranular bubbles", {0, 1000}}},
        {5, {"Xe at grain boundary", {0, 1000}}},
        {6, {"Xe released", {0, 1000}}},
        {7, {"Kr produced", {0, 1000}}},
        {8, {"Kr in grain", {0, 1000}}},
        {9, {"Kr in intragranular solution", {0, 1000}}},
        {10, {"Kr in intragranular bubbles", {0, 1000}}},
        {11, {"Kr at grain boundary", {0, 1000}}},
        {12, {"Kr released", {0, 1000}}},
        {13, {"He produced", {0, 1000}}},
        {14, {"He in grain", {0, 1000}}},
        {15, {"He in intragranular solution", {0, 1000}}},
        {16, {"He in intragranular bubbles", {0, 1000}}},
        {17, {"He at grain boundary", {0, 1000}}},
        {18, {"He released", {0, 1000}}},
        {19, {"Intragranular bubble concentration", {0, 1000}}},
        {20, {"Intragranular bubble radius", {0, 1000}}},
        {21, {"Intragranular Xe atoms per bubble", {0, 1000}}},
        {22, {"Intragranular Kr atoms per bubble", {0, 1000}}},
        {23, {"Intragranular He atoms per bubble", {0, 1000}}},
        {24, {"Intragranular gas bubble swelling", {0, 1000}}},
        {25, {"Intergranular bubble concentration", {0, 1000}}},
        {26, {"Intergranular Xe atoms per bubble", {0, 1000}}},
        {27, {"Intergranular Kr atoms per bubble", {0, 1000}}},
        {28, {"Intergranular He atoms per bubble", {0, 1000}}},
        {29, {"Intergranular atoms per bubble", {0, 1000}}},
        {30, {"Intergranular vacancies per bubble", {0, 1000}}},
        {31, {"Intergranular bubble radius", {0, 1000}}},
        {32, {"Intergranular bubble area", {0, 1000}}},
        {33, {"Intergranular bubble volume", {0, 1000}}},
        {34, {"Intergranular fractional coverage", {0, 1000}}},
        {35, {"Intergranular saturation fractional coverage", {0, 1000}}},
        {36, {"Intergranular gas swelling", {0, 1000}}},
        {37, {"Intergranular fractional intactness", {0, 1000}}},
        {38, {"Burnup", {0, 1000}}},
        {39, {"Effective burnup", {0, 1000}}},
        {40, {"Fuel density", {0, 1000}}},
        {41, {"U234", {0, 1000}}},
        {42, {"U235", {0, 1000}}},
        {43, {"U236", {0, 1000}}},
        {44, {"U237", {0, 1000}}},
        {45, {"U238", {0, 1000}}},
        {46, {"Intergranular vented fraction", {0, 1000}}},
        {47, {"Intergranular venting probability", {0, 1000}}},
        {48, {"Xe133 produced", {0, 1000}}},
        {49, {"Xe133 in grain", {0, 1000}}},
        {50, {"Xe133 in intragranular solution", {0, 1000}}},
        {51, {"Xe133 in intragranular bubbles", {0, 1000}}},
        {52, {"Xe133 decayed", {0, 1000}}},
        {53, {"Xe133 at grain boundary", {0, 1000}}},
        {54, {"Xe133 released", {0, 1000}}},
        {55, {"Restructued volume fraction", {0, 1000}}},
        {56, {"HBS porosity", {0, 1000}}},
        {57, {"Kr85m produced", {0, 1000}}},
        {58, {"Kr85m in grain", {0, 1000}}},
        {59, {"Kr85m in intragranular solution", {0, 1000}}},
        {60, {"Kr85m in intragranular bubbles", {0, 1000}}},
        {61, {"Kr85m decayed", {0, 1000}}},
        {62, {"Kr85m at grain boundary", {0, 1000}}},
        {63, {"Kr85m released", {0, 1000}}},
        {64, {"Intragranular similarity ratio", {0, 1000}}},
        {65, {"Irradiation time", {0, 1000}}},
        {66, {"Initial stoichiometry deviation", {0, 1000}}},
        {67, {"Fuel oxygen partial pressure", {0, 1000}}},
        {68, {"Intragranular gas solution swelling", {0, 1000}}},
        {69, {"FIMA", {0, 1000}}},
        {70, {"Porosity", {0, 1000}}},
        {71, {"Fabrication porosity", {0, 1000}}},
        {72, {"Open porosity", {0, 1000}}},
        {80, {"HBS pore density", {0, 1000}}},
        {81, {"HBS pore volume", {0, 1000}}},
        {82, {"HBS pore radius", {0, 1000}}},
        {83, {"Xe in HBS pores", {0, 1000}}},
        {85, {"Xe in HBS pores - variance", {0, 1000}}},
        {86, {"Xe atoms per HBS pore", {0, 1000}}},
        {88, {"Xe atoms per HBS pore - variance", {0, 1000}}},
        {89, {"Residual porosity", {0, 1000}}},
        {90, {"Densification factor", {0, 1000}}},
        {91, {"Diffusion coefficient", {0, 1000}}},
        {92, {"Xe in grain HBS", {0, 1000}}},
        {100, {"Xe produced in HBS", {0, 1000}}}};


    /**
     * \brief Check if the variable is in the bounds
     * @param Sciantix_Variables[] Array of double representing current variables in the simulation.
     */
    void CheckBounds(double Sciantix_Variables[], int size)
    {

        for (int i = 0; i < 101; ++i)
        {
            auto it = boundsMap.find(i);
            if (it != boundsMap.end())
            {
                double value = Sciantix_Variables[i];
                const Bounds &bounds = it->second.second;
                if (value < bounds.min || value > bounds.max)
                {
                    ErrorMessages::Switch("CheckBounds", it->second.first, value);
                }
            }
        }
    }

}
