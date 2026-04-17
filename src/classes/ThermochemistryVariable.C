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

#include <map>

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

void ThermochemistryVariable::setStoichiometry(std::map <std::string, int> stoic)
{
    stoichiometry = stoic;
}

std::map <std::string, int> ThermochemistryVariable::getStoichiometry()
{
    return stoichiometry;
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
        {"Cr", 51.9961},
        {"I", 126.90447},
        {"Mo", 95.95},
        {"O", 15.999},
        {"Te", 127.60},
        {"U", 238.02891},
        {"Pu", 239.052},
        {"Va", 0.0}
    };

    double molar_mass = 0.0;

    for (const auto& term : stoichiometry)
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
