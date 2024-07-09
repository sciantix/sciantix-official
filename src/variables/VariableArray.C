#include "VariableArray.h"

VariableArray::VariableArray(){}

VariableArray::VariableArray(std::vector<Variable> data)
{
    array = data;
}

void VariableArray::push(Variable element)
{
    array.push_back(element);
    
}

void VariableArray::clear()
{

}