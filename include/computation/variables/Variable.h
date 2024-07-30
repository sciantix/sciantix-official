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

#ifndef VARIABLE_H
#define VARIABLE_H

#include <string>
#include <vector>
#include <iostream>
#include "Entity.h"
#include "ErrorMessages.h"

/**
 * \class Variable
 * \brief Class derived from Entity, inherited by PhysicsVariable and InputVariable.
 *
 * The Variable class inherits from Entity and is intended to be the base class for different types
 * of variables used in a system, encapsulating common attributes such as name and reference.
 */
class Variable : virtual public Entity
{
protected:
    double bounds[2]; // bounds[0] is the minimum value, bounds[1] is the maximum value
    bool bounded;

public:

    /**
     * \brief Sets the bounds for the variable
     * \param minBound The minimum bound
     * \param maxBound The maximum bound
     */
    void setBounds(double minBound, double maxBound) {
        bounds[0] = minBound;
        bounds[1] = maxBound;
    }

    /**
     * \brief Gets the bounds of the variable
     * \return A pointer to the bounds array
     */
    const double* getBounds() const {
        return bounds;
    }

    /**
     * \brief Sets whether to check the bounds
     * \param check A boolean value indicating whether to check the bounds
     */
    void setBounded(bool check) {
        bounded = check;
    }

    /**
     * \brief Gets whether to check the bounds
     * \return A boolean value indicating whether to check the bounds
     */
    bool getBounded() const {
        return bounded;
    }

	/**
     * \brief Checks if a value is within the bounds, calls Switch if not
     * \param name The name of the variable
     * \param value The value to check
     */
    void isWithinBounds(double value) const {
        if (bounded) {
            double excess = 0.0;
            if (value < bounds[0]) {
                excess = bounds[0] - value;
                ErrorMessages::errorBounds(name, value, excess);
            } else if (value > bounds[1]) {
                excess = value - bounds[1];
                ErrorMessages::errorBounds(name, value, excess);
            }
        }
    }
    
	/**
	 * \brief Default constructor for the Variable class
	 */
	Variable() { }
	/**
	 * \brief Destructor for the Variable class
	 */
	~Variable() { }
};

#endif
