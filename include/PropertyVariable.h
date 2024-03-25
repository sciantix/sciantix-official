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
//  Authors: G.Nicodemo                                                             //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef PROPERTY_VARIABLE_H
#define PROPERTY_VARIABLE_H

#include "PhysicsVariable.h"

/**
 * @brief Derived class for property variables.
 *
 */

class PropertyVariable : virtual public PhysicsVariable
{
private:
    bool to_output; ///< Flag indicating whether the variable should be included in output

public:
    /**
     * @brief Default constructor.
     */
    PropertyVariable() : to_output(false) {}

    /**
     * @brief Destructor.
     */
    virtual ~PropertyVariable() {}
};

#endif