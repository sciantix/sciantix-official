#include "PhysicsVariable.h"

void SciantixVariable::rescaleInitialValue(const double factor)
{
    // Function to rescale the final value
    initial_value *= factor;
}

void SciantixVariable::rescaleFinalValue(const double factor)
{
    // Function to rescale the final value
    final_value *= factor;
}

void SciantixVariable::addValue(const double v)
{
    // Function to increase final_value by v
    final_value += v;
}

void SciantixVariable::setUOM(std::string s)
{
    uom = s;
}

std::string SciantixVariable::getUOM()
{
    return uom;
}

void SciantixVariable::setConstant()
{
    final_value = initial_value;
}

void SciantixVariable::resetValue()
{
    initial_value = final_value;
}

void SciantixVariable::setFinalValue(double FinalValue)
{
    final_value = FinalValue;
}

void SciantixVariable::setInitialValue(double InitialValue)
{
    initial_value = InitialValue;
}

double SciantixVariable::getFinalValue()
{
    return final_value;
}

double SciantixVariable::getInitialValue()
{
    return initial_value;
}

double SciantixVariable::getIncrement()
{
    return final_value - initial_value;
}

void SciantixVariable::setOutput(bool io)
{
    to_output = io;
}

bool SciantixVariable::getOutput()
{
    return to_output;
}
