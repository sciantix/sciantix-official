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

#include "ThermochemistryVariable.h"

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
