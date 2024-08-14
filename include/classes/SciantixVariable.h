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
class SciantixVariable : virtual public PhysicsVariable
{
private:
    bool to_output; ///< Flag indicating whether the variable should be included in output

public:
    /**
     * @brief Default constructor.
     */
    SciantixVariable() : to_output(false) {}

    /**
     * @brief Destructor.
     */
    virtual ~SciantixVariable() {}
};

#endif
