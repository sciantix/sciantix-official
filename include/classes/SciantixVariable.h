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

#ifndef SCIANTIX_VARIABLE_H
#define SCIANTIX_VARIABLE_H

#include "Variable.h"

/**
 * @class SciantixVariable
 * @brief A specialized variable class that extends the Variable class with physical attributes and functionalities.
 *
 * SciantixVariable includes features such as unit of measure (UOM), final and initial values,
 * and mechanisms for adjusting these values through specific operations.
 * 
 * @author G. Zullo
 * @author F. Bastien
 * 
 */
class SciantixVariable : virtual public Variable
{
protected:
	std::string uom;
	double final_value;
	double initial_value;
	bool to_output;

public:

	SciantixVariable(std::string name, std::string uom, double initial_value, double final_value, bool output)
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
	void rescaleInitialValue(const double factor);

	/**
	 * @brief Rescales the final value by a specified factor.
	 * @param factor The multiplier to apply to the final value.
	 */
	void rescaleFinalValue(const double factor);

	/**
	 * @brief Adds a specified value to the final value.
	 * @param v The value to add to the final value.
	 */
	void addValue(const double v);

	/**
	 * @brief Sets the unit of measure for this variable.
	 * @param s The string representing the unit of measure.
	 */
	void setUOM(std::string s);

	/**
	 * @brief Retrieves the unit of measure.
	 * @return The unit of measure as a string.
	 */
	std::string getUOM();

	/**
	 * @brief Sets the final value equal to the initial value, making the variable constant over time.
	 */
	void setConstant();

	/**
	 * @brief Resets the initial value to match the final value, synchronizing them.
	 */
	void resetValue();

	/**
	 * @brief Sets the final value to a specified value.
	 * @param FinalValue The value to set as the final value.
	 */
	void setFinalValue(double FinalValue);

	/**
	 * @brief Sets the initial value to a specified value.
	 * @param InitialValue The value to set as the initial value.
	 */
	void setInitialValue(double InitialValue);

	/**
	 * @brief Retrieves the final value of the variable.
	 * @return The final value.
	 */
	double getFinalValue();

	/**
	 * @brief Retrieves the initial value of the variable.
	 * @return The initial value.
	 */
	double getInitialValue();

	/**
	 * @brief Calculates and returns the increment between the final and initial values.
	 * @return The calculated increment.
	 */
	double getIncrement();

	/**
	 * @brief Sets whether the variable should be included in output.
	 * @param io A boolean value indicating output inclusion.
	 */
	void setOutput(bool io);

	/**
	 * @brief Checks if the variable is marked for output.
	 * @return True if the variable is to be included in output, false otherwise.
	 */
	bool getOutput();

	/**
	 * @brief Constructor
	 */
	SciantixVariable() {}
	
	/**
	 * @brief Destructor
	 */
	~SciantixVariable() {}
};

#endif
