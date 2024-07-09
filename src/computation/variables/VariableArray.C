#include "VariableArray.h"

VariableArray::VariableArray(){}

VariableArray::VariableArray(std::vector<Variable> data)
{
    array = data;

    for (int i = 0; i < data.size(); i++)
    {
        map[data[i].getName()] = i;
    }
}

void VariableArray::push(Variable element)
{
    if (map.find(element.getName()) != map.end())
    {
        array[map[element.getName()]] = element;
    }
    else 
    {
        map[element.getName()] = array.size();
        array.push_back(element);
    }    
}

void VariableArray::clear()
{
    array.clear();
    map.clear();
}

Variable VariableArray::operator[](int index)
{
    return array[index];
}

Variable VariableArray::operator[](std::string variable_name)
{
    if (map.find(variable_name) == map.end())
    {
        std::cerr << "Variable " << variable_name << " not defined in the array" << std::endl;
        exit(-1);
    }
    else
    {
        return array[map.at(variable_name)];
    }
}