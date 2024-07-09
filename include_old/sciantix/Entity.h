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

#ifndef ENTITY_H
#define ENTITY_H
#include <string>

/**
 * \class Entity
 * \brief Base class for Variable and Material classes, providing common attributes and methods.
 *
 * Entity serves as a foundational class that encapsulates common properties such as
 * a name and a reference, which are used across derived classes. It provides basic
 * functionalities to set and retrieve these properties.
 */
class Entity
{
protected:
	std::string reference;
	std::string name;

public:
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

	/**
	 * \brief Default constructor for the Entity class.
	 */
	Entity() {}

	/**
	 * \brief Destructor for the Entity class.
	 */
	~Entity() {}
};

#endif 	// ENTITY_H
