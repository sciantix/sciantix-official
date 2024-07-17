#include "VariableArray.h"


template <class T>
VariableArray<T>::VariableArray(){}


template <class T>
VariableArray<T>::VariableArray(std::vector<T> data)
{
    array = data;

    for (int i = 0; i < data.size(); i++)
    {
        map[data[i].getName()] = i;
    }
}


template <class T>
void VariableArray<T>::push(T element)
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


template <class T>
void VariableArray<T>::clear()
{
    array.clear();
    map.clear();
}

template <class T>
bool VariableArray<T>::empty()
{
    return array.empty();
}


template <class T>
T VariableArray<T>::operator[](int index)
{
    return array[index];
}


template <class T>
T VariableArray<T>::operator[](std::string variable_name)
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