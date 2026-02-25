
.. _program_listing_file_include_classes_SciantixArray.h:

Program Listing for File SciantixArray.h
========================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_SciantixArray.h>` (``include/classes/SciantixArray.h``)

.. |exhale_lsh| unicode:: U+021B0 .. UPWARDS ARROW WITH TIP LEFTWARDS

.. code-block:: cpp

   //       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
   //      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
   //     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
   //      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
   //  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
   //  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
   //                                                                                  //
   //  Originally developed by D. Pizzocri & T. Barani                                 //
   //                                                                                  //
   //  Version: 2.2.1                                                                    //
   //  Year: 2025                                                                      //
   //  Authors: D. Pizzocri, G. Zullo.                                                 //
   //                                                                                  //
   
   #ifndef __VARIABLE_ARRAY_H__
   #define __VARIABLE_ARRAY_H__
   
   #include "Variable.h"
   
   #include <iostream>
   #include <map>
   #include <string>
   #include <vector>
   
   #include <type_traits>
   
   template <class T> class SciantixArray
   {
     private:
       std::vector<T>             array;  // Vector storing the elements of type T
       std::map<std::string, int> map;    // Map for storing the relationship between element names and
                                          // their indices in the array
   
     public:
       SciantixArray()
       {
       }
   
       ~SciantixArray()
       {
       }
   
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
           std::map<std::string, int>::iterator pos = map.find(element.getName());
           if (pos != map.end())
           {
               array[pos->second] = element;
           }
           else
           {
               map.insert(std::pair<std::string, int>(element.getName(), array.size()));
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
           std::map<std::string, int>::iterator pos = map.find(variable_name);
           if (pos == map.end())
           {
               std::cerr << "Variable " << variable_name << " not defined in the array" << std::endl;
               exit(-1);
           }
           else
           {
               return array[pos->second];
           }
       }
   
       typename std::vector<T>::iterator begin()
       {
           return array.begin();
       }
   
       typename std::vector<T>::iterator end()
       {
           return array.end();
       }
   
       typename std::vector<T>::const_iterator begin() const
       {
           return array.begin();
       }
   
       typename std::vector<T>::const_iterator end() const
       {
           return array.end();
       }
   
       bool isElementPresent(std::string element_name)
       {
           return map.find(element_name) != map.end();
       }
   };
   
   #endif
