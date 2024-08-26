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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef __VARIABLE_ARRAY_H__
#define __VARIABLE_ARRAY_H__

#include "Variable.h"

#include <vector>
#include <map>
#include <string>
#include <iostream>

#include <type_traits> 

/**
 * @class SciantixArray
 * @brief A template class for managing a collection of variables with fast access by name or index.
 * 
 * @param T The type of elements stored in the array. The type T must have a `getName()` method returning a `std::string`.
 * 
 * This class allows for efficient storage, retrieval, and modification of elements by their name or index. It internally uses
 * a `std::vector` for storing the elements and a `std::map` for mapping names to indices for quick lookup.
 * 
 * @author F. Bastien
 * @author G. Zullo
 * 
 */
template <class T>
class SciantixArray
{
private:
    std::vector<T> array; // Vector storing the elements of type T
    std::map<std::string, int> map; // Map for storing the relationship between element names and their indices in the array

public:
    /**
     * @brief Constructor
     */
    SciantixArray(){}

	/**
	 * @brief Destructor
	 */
	~SciantixArray() {}

    /**
     * @brief Constructs a SciantixArray from a given vector of elements.
     * 
     * @param data A vector of elements to initialize the array.
     * 
     * The constructor also populates the internal map with the names of the elements and their corresponding indices.
     */
    SciantixArray(std::vector<T> data)
    {
        array = data;

        for (int i = 0; i < data.size(); i++)
        {
            map[data[i].getName()] = i;
        }
    }

    /**
     * @brief Adds or replaces an element in the array.
     * 
     * @param element The element to be added or replaced.
     * 
     * If an element with the same name already exists in the array, it will be replaced with the new element.
     * Otherwise, the element is added to the end of the array.
     */
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

    /**
     * @brief Clears all elements from the array.
     * 
     * This method clears both the internal vector and the map, effectively resetting the array.
     */
    void clear()
    {
        array.clear();
        map.clear();
    }

    /**
     * @brief Checks if the array is empty.
     * 
     * @return True if the array is empty, False otherwise.
     */
    bool empty()
    {
        return array.empty();
    }

    /**
     * @brief Accesses an element by its index.
     * 
     * @param index The index of the element to access.
     * @return A reference to the element at the specified index.
     */
    T& operator[](int index)
    {
        return array[index];
    }

    /**
     * @brief Accesses an element by its name.
     * 
     * @param variable_name The name of the element to access.
     * @return A reference to the element with the specified name.
     * 
     * If the element is not found, an error message is printed and the program exits.
     */
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

    /**
     * @brief Provides an iterator to the beginning of the array.
     * 
     * @return An iterator pointing to the first element of the array.
     */
    typename std::vector<T>::iterator begin() {return array.begin();}

    /**
     * @brief Provides an iterator to the end of the array.
     * 
     * @return An iterator pointing to the past-the-end element of the array.
     */
    typename std::vector<T>::iterator end() {return array.end();}

    /**
     * @brief Provides a constant iterator to the beginning of the array.
     * 
     * @return A constant iterator pointing to the first element of the array.
     */
    typename std::vector<T>::const_iterator begin() const {return array.begin();}

    /**
     * @brief Provides a constant iterator to the end of the array.
     * 
     * @return A constant iterator pointing to the past-the-end element of the array.
     */
    typename std::vector<T>::const_iterator end() const {return array.end();}

    /**
     * @brief Checks if an element with the given name is present in the array.
     * 
     * @param element_name The name of the element to check.
     * @return true if the element is present, false otherwise.
     */
    bool isElementPresent(std::string element_name)
    {
        return map.find(element_name) != map.end();
    }
};

#endif
