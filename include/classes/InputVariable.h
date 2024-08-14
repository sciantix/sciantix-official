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
 * 
 */
class InputVariable : virtual public Variable
{
public:
	/**
	 * @brief Sets the value of the input variable.
	 *
	 * @param v The new value to assign to the input variable.
	 */
	void setValue(double v)
	{
		/// Member function to set the setting value of the declared object.
		value = v;
	}

	/**
	 * @brief Retrieves the value of the input variable.
	 *
	 * @return The current value of the input variable.
	 */
	double getValue()
	{
		/// Member function to get the setting value of the object.
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
