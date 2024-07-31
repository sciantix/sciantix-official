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
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef VARIABLE_H
#define VARIABLE_H

#include <string>
#include <vector>
#include <iostream>

/**
 * \class Variable
 * \brief Class derived from Entity, inherited by PhysicsVariable and InputVariable.
 *
 * The Variable class inherits from Entity and is intended to be the base class for different types
 * of variables used in a system, encapsulating common attributes such as name and reference.
 */
class Variable
{
protected:
	std::string name;
	std::string reference;

public:
	/**
	 * \brief Default constructor for the Variable class
	 */
	Variable() { }
	/**
	 * \brief Destructor for the Variable class
	 */
	~Variable() { }

	/**
	 * \brief Sets the name of the entity.
	 * @param n The new name to be set for the entity.
	 */
	void setName(std::string n)
	{
		/// Member function to set the name of the object
		name = n;
	}

	/**
	 * \brief Retrieves the name of the entity.
	 * @return A string that is the current name of the entity.
	 */
	std::string getName()
	{
		/// Member function to get the name of the object
		return name;
	}

	/**
	 * \brief Sets a reference for the entity.
	 * @param n The reference string to be set.
	 */
	void setRef(std::string n)
	{
		/// Member function to set the reference field of the object
		reference = n;
	}

	/**
	 * \brief Retrieves the reference or identifier of the entity.
	 * @return A string that is the current reference of the entity.
	 */
	std::string getRef()
	{
		/// Member function to get the reference field of the object
		return reference;
	}
};

#endif
