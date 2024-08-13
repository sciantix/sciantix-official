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

#ifndef MATERIAL_H
#define MATERIAL_H

#include <string>

/**
 * @class Material
 * @brief Class for the materials used in SCIANTIX (e.g., fuel matrix, fission gas, etc.).
 * 
 * @authors 
 * G. Zullo
 */
class Material
{
public:
	/**
	 * @brief Default constructor for the Material class.
	 */
	Material() {}
	/**
	 * Destructor for the Material class.
	 */
	~Material() {}

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

protected:
	std::string name;
	std::string reference;
};

#endif // MATERIAL_H
