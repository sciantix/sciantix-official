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

#include "HistoryVariable.h"
#include "SciantixVariable.h"
#include "InputVariable.h"
#include "Matrix.h"
#include "Gas.h"
#include "System.h"

/// Derived class for the SCIANTIX models.

class Model : public HistoryVariable, public SciantixVariable, public InputVariable, public System
{
protected:
	std::string overview;
	std::vector<double> parameter;

public:

	void setParameter(std::vector<double> p)
	{
		parameter = p;
	}

	std::vector<double> getParameter()
	{
		return parameter;
	}

	// Constructor & destructor
	Model() {}
	~Model() {}

};

#endif
