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
//  Authors: F. Bastien                                                             //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////


#ifndef __VARIABLE_ARRAY_H__
#define __VARIABLE_ARRAY_H__

#include "Variable.h"

#include <vector>
#include <map>
#include <string>
#include <iostream>


template <class T>
class VariableArray
{
private:
    std::vector<T> array;
    std::map<std::string, int> map;

public:
    VariableArray();
    VariableArray(std::vector<T> data);

    void push(T element);
    void clear();
    bool empty();

    T operator[](int index);
    T operator[](std::string variable_name);


    // Iterators to easily make loops on the vector
    typename std::vector<T>::iterator begin() {return array.begin();}
    typename std::vector<T>::iterator end() {return array.end();}
    typename std::vector<T>::const_iterator begin() const {return array.begin();}
    typename std::vector<T>::const_iterator end() const {return array.end();}
};


#endif