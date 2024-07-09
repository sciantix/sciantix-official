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

class VariableArray
{
private:
    std::vector<Variable> array;
    std::map<std::string, int> map;

public:
    VariableArray();
    VariableArray(std::vector<Variable> data);
    void push(Variable element);
    void clear();
    Variable operator[](int index);
    Variable operator[](std::string variable_name);
};

#endif