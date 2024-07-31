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

#ifndef SCIANTIX_VARIABLE_H
#define SCIANTIX_VARIABLE_H

#include "PhysicsVariable.h"

/**
 * @brief Extends the PhysicsVariable class to include specific behaviors for variables used in the SCIANTIX simulation framework.
 */
class SciantixVariable : virtual public PhysicsVariable
{
private:
    bool to_output;       ///< Flag indicating whether the variable should be included in output
    double validation[2]; // Bounds for the variables used in validation
    bool isValidation;    // Flag indicating whether the variable is used for validation

public:

/**
     * \brief Sets the bounds for the variable
     * \param minBound The minimum bound
     * \param maxBound The maximum bound
     */
    void setValidation(double minBound, double maxBound) {
        validation[0] = minBound;
        validation[1] = maxBound;
    }

    /**
     * \brief Sets whether to check the bounds of the validation variable
     * \param check A boolean value indicating whether to check the bounds
     */
    void setIsValidation(bool check)
    {
        isValidation = check;
    }

    /**
     * \brief Checks if a value is within the bounds, calls Switch if not
     * \param name The name of the variable
     * \param value The value to check
     */
    void isWithinBoundsValidation(double value)
    {
        if (isValidation)
        {
            double excess = 0.0;
            if (value < validation[0])
            {
                excess = validation[0] - value;
                ErrorMessages::errorBounds(name + " in validation", value, excess);
            }
            else if (value > validation[1])
            {
                excess = value - validation[1];
                ErrorMessages::errorBounds(name + " in validation", value, excess);
            }
        }
    }

    /**
     * @brief Default constructor for the SciantixVariable class.
     */
    SciantixVariable() : to_output(false) {}

    /**
     * @brief Destructor for the SciantixVariable class.
     */
    virtual ~SciantixVariable() {}
};

#endif // SCIANTIXVARIABLE_H
