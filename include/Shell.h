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

#ifndef SHELL_H
#define SHELL_H

#include "Matrix.h"
#include <string>

/// Class for Triso shells derived from the Matrix class
class Shell : public Matrix
{
protected:

	double inner_radius;
	double outer_radius;

public:

	void setInnerRadius(double innerRadius){

		inner_radius = innerRadius;

	}

	double getInnerRadius() const{

		return inner_radius;

	}

	void setOuterRadius(double outerRadius){

		outer_radius = outerRadius;

	}

	double getOuterRadius() const{

		return outer_radius;

	}
	
	Shell() { }
	~Shell() { }
};

#endif // SHELL_H