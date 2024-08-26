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
//  Year: 2023                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef INPUT_VARIABLE_H
#define INPUT_VARIABLE_H

#include "Variable.h"

/**
 * @class InputVariable
 * @brief A derived class specifically for handling input variables used in simulations or models.
 *
 * This class extends the `Variable` class, providing a structured way to manage input settings,
 * such as parameters for simulations, ensuring that these inputs can be easily modified and retrieved.
 * 
 * @author G. Zullo
 * @author F. Bastien
 * 
 */
class InputVariable : virtual public Variable
{
public:

	InputVariable(std::string name, int value)
	{
		this->name = name;
		this->value = value;
	}

	/**
	 * @brief Sets the value of the input variable.
	 *
	 * @param v The new value to assign to the input variable.
	 */
	void setValue(double v)
	{
		value = v;
	}

	/**
	 * @brief Retrieves the value of the input variable.
	 *
	 * @return The current value of the input variable.
	 */
	double getValue()
	{
		return value;
	}

	/**
	 * @brief Constructor for InputVariable.
	 */
	InputVariable() {}

	/**
	 * @brief Destructor for InputVariable.
	 */
	~InputVariable() {}

protected:
	double value;
};

#endif //INPUT_VARIABLE_H
