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

#ifndef PARTICLE_H
#define PARTICLE_H

#include "Material.h"

/**
 * @class Particle
 * @brief A derived class from Material.
 *
 * This class extends the Material class to include specific properties that are unique to particle used in fission processes,
 * such as atomic number, mass number, decay rate, and a precursor factor.
 * 
 * @author G. Zullo
 */
class Particle : virtual public Material
{
protected:
    int atomic_number;
    double mass_number;
    double decay_rate;
    double chemically_active;
    double precursor_factor;

public:
    /**
     * @brief Sets the atomic number of the gas.
     * @param y The atomic number to be set.
     */
    void setAtomicNumber(int y)
    {
        atomic_number = y;
    }

    /**
     * @brief Returns the atomic number of the gas.
     * @return The atomic number of the gas.
     */
    int getAtomicNumber()
    {
        return atomic_number;
    }

    /**
     * @brief Sets the mass number of the gas.
     * @param y The mass number to be set.
     */
    void setMassNumber(double y)
    {
        mass_number = y;
    }

    /**
     * @brief Returns the mass number of the gas.
     * @return The mass number of the gas.
     */
    double getMassNumber()
    {
        return mass_number;
    }

    /**
     * @brief Sets the decay rate of the gas.
     * @param l The decay rate to be set.
     */
    void setDecayRate(double l)
    {
        decay_rate = l;
    }

    /**
     * @brief Returns the decay rate of the gas.
     * @return The decay rate of the gas.
     */
    double getDecayRate()
    {
        return decay_rate;
    }

    /**
     * @brief
     * @param g
     */
    void setChemicallyActive(double g)
    {
        chemically_active = g;
    }

    /**
     * @brief 
     * @return 
     */
    double getChemicallyActive()
    {
        return chemically_active;
    }

    /**
     * @brief Sets the precursor factor for nuclear reactions.
     * @param h The precursor factor to be set.
     */
    void setPrecursorFactor(double h)
    {
        precursor_factor = h;
    }

    /**
     * @brief Returns the precursor factor for nuclear reactions.
     * @return The precursor factor of the gas.
     */
    double getPrecursorFactor()
    {
        return precursor_factor;
    }

    /**
     * @brief Constructor
     */
    Particle() {}

    /**
     * @brief Destructor
     */
    ~Particle() {}
};

#endif // PARTICLE_H
