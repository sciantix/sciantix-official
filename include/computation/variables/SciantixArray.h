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

#ifndef __VARIABLE_ARRAY_H__
#define __VARIABLE_ARRAY_H__

#include "Variable.h"

#include <vector>
#include <map>
#include <string>
#include <iostream>

#include <type_traits> 
#include "SciantixVariable.h"


template <class T>
class SciantixArray
{
private:
    std::vector<T> array;
    std::map<std::string, int> map;

public:
    SciantixArray(){}
    SciantixArray(std::vector<T> data)
    {
        array = data;

        for (int i = 0; i < data.size(); i++)
        {
            map[data[i].getName()] = i;
        }
    }

    void push(T element)
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

    void clear()
    {
        array.clear();
        map.clear();
    }

    bool empty()
    {
        return array.empty();
    }


    T& operator[](int index)
    {
        return array[index];
    }

    T& operator[](std::string variable_name)
    {
        if (map.find(variable_name) == map.end())
        {
            std::cerr << "Variable " << variable_name << " not defined in the array" << std::endl;
            exit(-1);
        }
        else
        {
            return array[map[variable_name]];
        }
    }


    // Iterators to easily make loops on the vector
    typename std::vector<T>::iterator begin() {return array.begin();}
    typename std::vector<T>::iterator end() {return array.end();}
    typename std::vector<T>::const_iterator begin() const {return array.begin();}
    typename std::vector<T>::const_iterator end() const {return array.end();}

    bool isElementPresent(std::string element_name)
    {
        if (map.find(element_name) == map.end()) return false;
        else return true;
    }
    };


#endif