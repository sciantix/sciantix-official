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
#include "Entity.h"

/**
 * \class Variable
 * \brief Class derived from Entity, inherited by PhysicsVariable and InputVariable.
 *
 * The Variable class inherits from Entity and is intended to be the base class for different types
 * of variables used in a system, encapsulating common attributes such as name and reference.
 */
class Variable : virtual public Entity
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
};

#endif
