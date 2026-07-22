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

#include "Constants.h"
#include "ThermochemistryVariable.h"

#include <algorithm>
#include <cctype>
#include <iostream>
#include <map>

namespace ThermochemistryVariableDetail
{
    std::string normalizeElementName(std::string element)
    {
        if (element.empty())
            return element;

        std::transform(element.begin(), element.end(), element.begin(), [](unsigned char c) { return std::tolower(c); });
        element[0] = static_cast<char>(std::toupper(static_cast<unsigned char>(element[0])));
        return element;
    }
}  // namespace ThermochemistryVariableDetail

using namespace ThermochemistryVariableDetail;

void ThermochemistryVariable::setLocation(std::string loc)
{
    location = loc;
}

std::string ThermochemistryVariable::getLocation()
{
    return location;
}

void ThermochemistryVariable::setPhase(std::string ph)
{
    phase = ph;
}

std::string ThermochemistryVariable::getPhase()
{
    return phase;
}

void ThermochemistryVariable::setComposition(std::map<std::string, double> composition_map)
{
    composition = composition_map;
}

std::map<std::string, double> ThermochemistryVariable::getComposition()
{
    return composition;
}

void ThermochemistryVariable::setSublatticeComposition(std::map<int, std::map<std::string, double>> composition_map)
{
    sublattice_composition = composition_map;
}

std::map<int, std::map<std::string, double>> ThermochemistryVariable::getSublatticeComposition()
{
    return sublattice_composition;
}

double ThermochemistryVariable::getMolarMass()
{
    double molar_mass = 0.0;

    std::map<std::string, double> molar_mass_composition;
    if (!composition.empty())
    {
        for (const auto& term : composition)
            molar_mass_composition[normalizeElementName(term.first)] += term.second;

        for (const auto& term : molar_mass_composition)
        {
            const auto atomic_mass = thermochemistry_atomic_masses.find(term.first);
            if (atomic_mass == thermochemistry_atomic_masses.end())
            {
                std::cerr << "Error: Atomic mass not available for element " << term.first
                          << " in thermochemistry variable " << name << std::endl;
                exit(1);
            }

            molar_mass += term.second * atomic_mass->second;
        }
        return molar_mass;
    }
    else
        std::cerr << "Error in computing the molar mass of variable: " << name << ". No composition available."
                  << std::endl;

    return molar_mass;
}

double ThermochemistryVariable::getMass()
{
    return final_value;
}
