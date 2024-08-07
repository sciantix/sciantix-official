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
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//  Contributors: F. Bastien                                                        //
//////////////////////////////////////////////////////////////////////////////////////

// #include "SciantixArray.h"


// template <class T>
// SciantixArray<T>::SciantixArray(){}


// template <class T>
// SciantixArray<T>::SciantixArray(std::vector<T> data)
// {
//     array = data;

//     for (int i = 0; i < data.size(); i++)
//     {
//         map[data[i].getName()] = i;
//     }
// }


// template <class T>
// void SciantixArray<T>::push(T element)
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


// template <class T>
// void SciantixArray<T>::clear()
// {
//     array.clear();
//     map.clear();
// }

// template <class T>
// bool SciantixArray<T>::empty()
// {
//     return array.empty();
// }




// template <class T>
// T& SciantixArray<T>::operator[](int index)
// {
//     return array[index];
// }


// template <class T>
// T& SciantixArray<T>::operator[](std::string variable_name)
// {
//     if (map.find(variable_name) == map.end())
//     {
//         std::cerr << "Variable " << variable_name << " not defined in the array" << std::endl;
//         exit(-1);
//     }
//     else
//     {
//         return array[map[variable_name]];
//     }
// }

// template <class T>
// bool SciantixArray<T>::isElementPresent(std::string element_name)
// {
//     if (map.find(element_name) == map.end()) return false;
//     else return true;
// }