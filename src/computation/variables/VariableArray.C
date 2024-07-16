#include "VariableArray.h"


template <class T>
VariableArray<T>::VariableArray(){
    // Check if template class is a subclass of Variable class (or Variable itself)
    static_assert(std::is_base_of<Variable, T>, "The class used for the VariableArray must be a subclass of the Variable class, or the Variable class itself");
}


template <class T>
VariableArray<T>::VariableArray(std::vector<T> data)
{
    // Check if template class is a subclass of Variable class (or Variable itself)
    static_assert(std::is_base_of<Variable, T>, "The class used for the VariableArray must be a subclass of the Variable class, or the Variable class itself");
    
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




// VariableArray::VariableArray(){}

// VariableArray::VariableArray(std::vector<Variable> data)
// {
//     array = data;

//     for (int i = 0; i < data.size(); i++)
//     {
//         map[data[i].getName()] = i;
//     }
// }

// void VariableArray::push(Variable element)
// {
//     if (map.find(element.getName()) != map.end())
//     {
//         array[map[element.getName()]] = element;
//     }
//     else 
//     {
//         map[element.getName()] = array.size();
//         array.push_back(element);
//     }    
// }

// void VariableArray::clear()
// {
//     array.clear();
//     map.clear();
// }

// Variable VariableArray::operator[](int index)
// {
//     return array[index];
// }

// Variable VariableArray::operator[](std::string variable_name)
// {
//     if (map.find(variable_name) == map.end())
//     {
//         std::cerr << "Variable " << variable_name << " not defined in the array" << std::endl;
//         exit(-1);
//     }
//     else
//     {
//         return array[map.at(variable_name)];
//     }
// }