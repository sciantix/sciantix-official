#include "PhysicsVariable.h"

void PhysicsVariable::rescaleInitialValue(const double factor)
{
    // Function to rescale the final value
    initial_value *= factor;
}

void PhysicsVariable::rescaleFinalValue(const double factor)
{
    // Function to rescale the final value
    final_value *= factor;
}

void PhysicsVariable::addValue(const double v)
{
    // Function to increase final_value by v
    final_value += v;
}

void PhysicsVariable::setUOM(std::string s)
{
    uom = s;
}

std::string PhysicsVariable::getUOM()
{
    return uom;
}

void PhysicsVariable::setConstant()
{
    final_value = initial_value;
}

void PhysicsVariable::resetValue()
{
    initial_value = final_value;
}

void PhysicsVariable::setFinalValue(double FinalValue)
{
    final_value = FinalValue;
}

void PhysicsVariable::setInitialValue(double InitialValue)
{
    initial_value = InitialValue;
}

double PhysicsVariable::getFinalValue()
{
    return final_value;
}

double PhysicsVariable::getInitialValue()
{
    return initial_value;
}

double PhysicsVariable::getIncrement()
{
    return final_value - initial_value;
}

void PhysicsVariable::setOutput(bool io)
{
    to_output = io;
}

bool PhysicsVariable::getOutput()
{
    return to_output;
}
