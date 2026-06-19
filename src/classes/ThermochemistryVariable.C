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
//  Version: under development                                                      //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E.Cappellari                                    //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "ThermochemistryVariable.h"
#include "Constants.h"

#include <algorithm>
#include <cctype>
#include <iostream>
#include <map>

namespace
{
std::string normalizeElementName(std::string element)
{
    if (element.empty())
        return element;

    std::transform(element.begin(), element.end(), element.begin(), [](unsigned char c) { return std::tolower(c); });
    element[0] = static_cast<char>(std::toupper(static_cast<unsigned char>(element[0])));
    return element;
}

std::string compoundNameFromVariableName(const std::string& variable_name)
{
    const size_t parenthesis_pos = variable_name.find(" (");
    std::string compound_name =
        parenthesis_pos == std::string::npos ? variable_name : variable_name.substr(0, parenthesis_pos);

    const size_t hash_pos = compound_name.find('#');
    if (hash_pos != std::string::npos)
        compound_name = compound_name.substr(0, hash_pos);

    const size_t auto_pos = compound_name.find("_AUTO");
    if (auto_pos != std::string::npos)
        compound_name = compound_name.substr(0, auto_pos);

    const size_t chkd_pos = compound_name.find("_CHKD");
    if (chkd_pos != std::string::npos)
        compound_name = compound_name.substr(0, chkd_pos);

    return compound_name;
}

std::map<std::string, double> parseCompoundStoichiometry(
    const std::string& compound_name,
    const std::map<std::string, double>& atomic_masses)
{
    std::map<std::string, std::string> known_elements;
    for (const auto& atomic_mass : atomic_masses)
    {
        std::string upper = atomic_mass.first;
        std::transform(upper.begin(), upper.end(), upper.begin(), [](unsigned char c) { return std::toupper(c); });
        known_elements[upper] = atomic_mass.first;
    }

    std::map<std::string, double> parsed_composition;
    size_t i = 0;
    while (i < compound_name.size())
    {
        const unsigned char character = static_cast<unsigned char>(compound_name[i]);

        if (compound_name[i] == '+' || compound_name[i] == '-')
        {
            ++i;
            while (i < compound_name.size() && std::isdigit(static_cast<unsigned char>(compound_name[i])))
                ++i;
            continue;
        }

        if (compound_name[i] == '_' || compound_name[i] == ':' || !std::isalpha(character))
        {
            ++i;
            continue;
        }

        std::string element;
        if (i + 2 <= compound_name.size())
        {
            std::string candidate = compound_name.substr(i, 2);
            std::transform(candidate.begin(), candidate.end(), candidate.begin(), [](unsigned char c) { return std::toupper(c); });
            const auto element_it = known_elements.find(candidate);
            if (element_it != known_elements.end())
            {
                element = element_it->second;
                i += 2;
            }
        }

        if (element.empty())
        {
            std::string candidate = compound_name.substr(i, 1);
            std::transform(candidate.begin(), candidate.end(), candidate.begin(), [](unsigned char c) { return std::toupper(c); });
            const auto element_it = known_elements.find(candidate);
            if (element_it == known_elements.end())
            {
                ++i;
                continue;
            }

            element = element_it->second;
            ++i;
        }

        double coefficient = 1.0;
        const size_t coefficient_begin = i;
        while (i < compound_name.size() && std::isdigit(static_cast<unsigned char>(compound_name[i])))
            ++i;
        if (i > coefficient_begin)
            coefficient = std::stod(compound_name.substr(coefficient_begin, i - coefficient_begin));

        parsed_composition[element] += coefficient;
    }

    return parsed_composition;
}
}

void ThermochemistryVariable::rescaleInitialValue(const double factor)
{
    initial_value *= factor;
}

void ThermochemistryVariable::rescaleFinalValue(const double factor)
{
    final_value *= factor;
}

void ThermochemistryVariable::addValue(const double v)
{
    final_value += v;
}

void ThermochemistryVariable::setUOM(std::string s)
{
    uom = s;
}

std::string ThermochemistryVariable::getUOM()
{
    return uom;
}

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

void ThermochemistryVariable::setComposition(std::map <std::string, double> composition_map)
{
    composition = composition_map;
}

std::map <std::string, double> ThermochemistryVariable::getComposition()
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

void ThermochemistryVariable::setConstant()
{
    final_value = initial_value;
}

void ThermochemistryVariable::resetValue()
{
    initial_value = final_value;
}

void ThermochemistryVariable::setFinalValue(double FinalValue)
{
    final_value = FinalValue;
}

void ThermochemistryVariable::setInitialValue(double InitialValue)
{
    initial_value = InitialValue;
}

double ThermochemistryVariable::getFinalValue()
{
    return final_value;
}

double ThermochemistryVariable::getInitialValue()
{
    return initial_value;
}

double ThermochemistryVariable::getIncrement()
{
    return final_value - initial_value;
}

void ThermochemistryVariable::setOutput(bool io)
{
    to_output = io;
}

bool ThermochemistryVariable::getOutput()
{
    return to_output;
}

double ThermochemistryVariable::getMolarMass()
{
    static const std::map<std::string, double> atomic_masses = {
        {"Cs", 132.90545196},
        {"I", 126.90447},
        {"Ba", 137.327},
        {"Mo", 95.95},
        {"O", 15.999},
        {"Te", 127.60},
        {"U", 238.02891},
        {"Pu", 239.052},
        {"Va", 0.0},
        {"Pd", 106.42},
        {"Rh", 102.91},
        {"Ru", 101.07},
        {"Tc", 98.906}
    };

    double molar_mass = 0.0;

    std::map<std::string, double> molar_mass_composition;
    if (!composition.empty())
    {
        for (const auto& term : composition)
            molar_mass_composition[normalizeElementName(term.first)] += term.second;

        for (const auto& term : molar_mass_composition)
        {
            const auto atomic_mass = atomic_masses.find(term.first);
            if (atomic_mass == atomic_masses.end())
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
        std::cerr << "Error in computing the molar mass of variable: "<< name << ". No composition available." << std::endl;


    return molar_mass;
}

double ThermochemistryVariable::getMass()
{
    return final_value;
}
