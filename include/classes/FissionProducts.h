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
//  Version: under development                                                      //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E.Cappellari                                    //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef FISSION_PRODUCTS_H
#define FISSION_PRODUCTS_H

#include "Material.h"

/**
 * @class FissionProducts
 * @brief A derived class from Material, specifically for modeling fission products.
 *
 * This class extends the Material class to include specific properties that are unique to fission products
 * used in fission processes, such as atomic number, mass number, Van der Waals volume, decay rate,
 * and a precursor factor.
 *
 * @author G. Zullo, E. Cappellari
 */
class FissionProducts : virtual public Material
{
  protected:
    int    atomic_number;
    double mass_number;
    double van_der_waals_volume;
    double decay_rate;
    double precursor_factor;
    bool chemically_active;

  public:
    /**
     * @brief Sets the atomic number of the fission product.
     * @param y The atomic number to be set.
     */
    void setAtomicNumber(int y)
    {
        atomic_number = y;
    }

    /**
     * @brief Returns the atomic number of the fission product.
     * @return The atomic number of the fission product.
     */
    int getAtomicNumber()
    {
        return atomic_number;
    }

    /**
     * @brief Sets the mass number of the fission product.
     * @param y The mass number to be set.
     */
    void setMassNumber(double y)
    {
        mass_number = y;
    }

    /**
     * @brief Returns the mass number of the fission product.
     * @return The mass number of the fission product.
     */
    double getMassNumber()
    {
        return mass_number;
    }

    /**
     * @brief Sets the Van der Waals volume of the gas.
     * @param y The Van der Waals volume to be set.
     */
    void setVanDerWaalsVolume(double y)
    {
        van_der_waals_volume = y;
    }

    /**
     * @brief Returns the Van der Waals volume of the gas.
     * @return The Van der Waals volume of the gas.
     */
    double getVanDerWaalsVolume()
    {
        return van_der_waals_volume;
    }

    /**
     * @brief Sets the decay rate of the fission product.
     * @param l The decay rate to be set.
     */
    void setDecayRate(double l)
    {
        decay_rate = l;
    }

    /**
     * @brief Returns the decay rate of the fission product.
     * @return The decay rate of the fission product.
     */
    double getDecayRate()
    {
        return decay_rate;
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
     * @return The precursor factor of the fission product.
     */
    double getPrecursorFactor()
    {
        return precursor_factor;
    }

    /**
     * @brief Sets whether the fission product is chemically active.
     */
    void setChemicallyActive(bool g)
    {
        chemically_active = g;
    }

    /**
     * @brief Returns whether the fission product is chemically active.
     * @return The chemical activity of the fission product.
     */
    double getChemicallyActive()
    {
        return chemically_active;
    }

    /**
     * @brief Constructor
     */
    FissionProducts()
    {
    }

    /**
     * @brief Destructor
     */
    ~FissionProducts()
    {
    }
};


#endif  // FISSION_PRODUCTS_H
