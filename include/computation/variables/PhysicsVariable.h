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

#ifndef PHYSICS_VARIABLE_H
#define PHYSICS_VARIABLE_H

#include "Variable.h"

/**
 * \class PhysicsVariable
 * \brief A specialized variable class that extends the Variable class with physical attributes and functionalities.
 *
 * PhysicsVariable includes features such as unit of measure (UOM), final and initial values,
 * and mechanisms for adjusting these values through specific operations.
 */
class PhysicsVariable : virtual public Variable
{
protected:
	std::string uom;
	double final_value;
	double initial_value;
	bool to_output;

public:

	PhysicsVariable(std::string name, std::string uom, double initial_value, double final_value, bool output)
	{
		this->name = name;
		this->uom = uom;
		this->initial_value = initial_value;
		this->final_value = final_value;
		this->to_output = output;
	}

	/**
	 * @brief Rescales the initial value by a specified factor.
	 * @param factor The multiplier to apply to the initial value.
	 */
	void rescaleInitialValue(const double factor)
	{
		// Function to rescale the final value
		initial_value *= factor;
	}

	/**
	 * @brief Rescales the final value by a specified factor.
	 * @param factor The multiplier to apply to the final value.
	 */
	void rescaleFinalValue(const double factor)
	{
		// Function to rescale the final value
		final_value *= factor;
	}

	/**
	 * @brief Adds a specified value to the final value.
	 * @param v The value to add to the final value.
	 */
	void addValue(const double v)
	{
		// Function to increase final_value by v
		final_value += v;
	}

	/**
	 * @brief Sets the unit of measure for this variable.
	 * @param s The string representing the unit of measure.
	 */
	void setUOM(std::string s)
	{
		uom = s;
	}

	/**
	 * @brief Retrieves the unit of measure.
	 * @return The unit of measure as a string.
	 */
	std::string getUOM()
	{
		return uom;
	}

	/**
	 * @brief Sets the final value equal to the initial value, making the variable constant over time.
	 */
	void setConstant()
	{
		final_value = initial_value;
	}

	/**
	 * @brief Resets the initial value to match the final value, synchronizing them.
	 */
	void resetValue()
	{
		initial_value = final_value;
	}

	/**
	 * @brief Sets the final value to a specified value.
	 * @param FinalValue The value to set as the final value.
	 */
	void setFinalValue(double FinalValue)
	{
		final_value = FinalValue;
	}

	/**
	 * @brief Sets the initial value to a specified value.
	 * @param InitialValue The value to set as the initial value.
	 */
	void setInitialValue(double InitialValue)
	{
		initial_value = InitialValue;
	}

	/**
	 * @brief Retrieves the final value of the variable.
	 * @return The final value.
	 */
	double getFinalValue()
	{
		return final_value;
	}

	/**
	 * @brief Retrieves the initial value of the variable.
	 * @return The initial value.
	 */
	double getInitialValue()
	{
		return initial_value;
	}

	/**
	 * @brief Calculates and returns the increment between the final and initial values.
	 * @return The calculated increment.
	 */
	double getIncrement()
	{
		return final_value - initial_value;
	}

	/**
	 * @brief Sets whether the variable should be included in output.
	 * @param io A boolean value indicating output inclusion.
	 */
	void setOutput(bool io)
	{
		to_output = io;
	}

	/**
	 * @brief Checks if the variable is marked for output.
	 * @return True if the variable is to be included in output, false otherwise.
	 */
	bool getOutput()
	{
		return to_output;
	}

	/**
	 * @brief Default constructor for PhysicsVariable.
	 */
	PhysicsVariable() {}
	
	/**
	 * @brief Destructor for PhysicsVariable.
	 */
	~PhysicsVariable() {}
};

#endif // PHYSICS_VARIABLE_H
