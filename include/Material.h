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
#include "Entity.h"

/// Derived class for the materials used in SCIANTIX (e.g., fuel matrix, fission gas, etc.).

class Material : virtual public Entity
{
public:
	Material() { }
	~Material() { }
	
	void setFuel(bool value) {
	
		fuel = value;
	
	};
	
	bool getFuel (){
	
		return fuel;
	
	}
	
		void setDensity(double m)
	{
		/// Member function to set the matrix theoretical density (kg/m3)
		density = m;
	}

	double getDensity()
	{
		/// Member function to get the matrix theoretical density (kg/m3)
		return density;
	}	
	
protected:
	std::string name;
	bool fuel;
	double density;
};

#endif // MATERIAL_H
