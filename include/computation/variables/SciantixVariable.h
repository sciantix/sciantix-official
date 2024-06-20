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
    bool to_output; ///< Flag indicating whether the variable should be included in output

public:
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
