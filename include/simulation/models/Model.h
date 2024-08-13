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

#ifndef MODEL_H
#define MODEL_H

#include <vector>
#include <string>
#include <iterator>
#include <map>
#include <string>

#include "InputVariable.h"
#include "Matrix.h"
#include "Gas.h"
#include "System.h"
#include "Material.h"

/**
 * @brief Represents the models used in the SCIANTIX simulation.
 *
 * The Model class is an integral part of the SCIANTIX simulation software, serving as a base for defining
 * various simulation models.
 * 
 * @author G. Zullo
 * @author F. Bastien
 * 
 */
class Model: public Material
{
protected:
	std::string overview;
	std::vector<double> parameter;

public:
	/**
	 * @brief Sets the parameters of the model.
	 * @param p Vector of doubles representing the parameters to be applied to the model.
	 */
	void setParameter(std::vector<double> p)
	{
		parameter = p;
	}

	/**
	 * @brief Retrieves the parameters of the model.
	 * @return Vector of doubles containing the current parameters of the model.
	 */
	std::vector<double> getParameter()
	{
		return parameter;
	}

	/**
	 * @brief Default constructor for Model class.
	 */
	Model() {}
	/**
	 * @brief Destructor for Model class.
	 */
	~Model() {}
};

#endif // MODEL_H
