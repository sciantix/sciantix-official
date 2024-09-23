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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef MATERIAL_H
#define MATERIAL_H

#include <string>

/**
 * @class Material
 * @brief Class for materials used in SCIANTIX (e.g., fuel matrix, fission gas, etc.).
 * 
 * @author G. Zullo
 */
class Material
{
public:
    /**
     * Constructor
     */
    Material() {}

    /**
     * Destructor
     */
    ~Material() {}

    /**
     * @brief Sets the name of the material.
     * @param n The new name to be set for the material.
     */
    void setName(std::string n)
    {
        name = n;
    }

    /**
     * @brief Retrieves the name of the material.
     * @return A string that is the current name of the material.
     */
    std::string getName()
    {
        return name;
    }

    /**
     * @brief Sets a reference for the material.
     * @param n The reference string to be set.
     */
    void setRef(std::string n)
    {
        reference = n;
    }

    /**
     * @brief Retrieves the reference or identifier of material.
     * @return A string that is the current reference.
     */
    std::string getRef()
    {
        return reference;
    }

protected:
    std::string name;
    std::string reference;
};

#endif // MATERIAL_H
