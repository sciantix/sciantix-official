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
//  Year: 2024                                                                      //
//  Authors: G. Nicodemo                                                            //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef INPUT_PROPERTY_H
#define INPUT_PROPERTY_H

#include "Variable.h"

/// Derived class for the input property (e.g., input settings for material property selection).
class InputProperty : virtual public Variable
{
public:
	void setValue(double v)
	{
		/// Member function to set the setting value of the declared object.
		value = v;
	}

	double getValue()
	{
		/// Member function to get the setting value of the object.
		return value;
	}

	InputProperty() { }
	~InputProperty() { }

protected:
	double value;

};

#endif