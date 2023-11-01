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

/// Derived class inherited by Variable

class PhysicsVariable : virtual public Variable
{
protected:
	std::string uom;
	double final_value;
	double initial_value;
	bool to_output;

public:
	void rescaleInitialValue(const double factor)
	{
		// Function to rescale the final value
		initial_value *= factor;
	}

	void rescaleFinalValue(const double factor)
	{
		// Function to rescale the final value
		final_value *= factor;
	}

	void addValue(const double v)
	{
		// Function to increase final_value by v
		final_value += v;
	}

	void setUOM(std::string s)
	{
		uom = s;
	}

	std::string getUOM()
	{
		return uom;
	}

	void setConstant()
	{
		final_value = initial_value;
	}

	void resetValue()
	{
		initial_value = final_value;
	}

	void setFinalValue(double FinalValue)
	{
		final_value = FinalValue;
	}

	void setInitialValue(double InitialValue)
	{
		initial_value = InitialValue;
	}

	double getFinalValue()
	{
		return final_value;
	}

	double getInitialValue()
	{
		return initial_value;
	}

	double getIncrement()
	{
		return final_value - initial_value;
	}

	void setOutput(bool io)
	{
		to_output = io;
	}

	bool getOutput()
	{
		return to_output;
	}

	PhysicsVariable() { }
	~PhysicsVariable() { }

};

#endif
