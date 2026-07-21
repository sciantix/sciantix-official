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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef THERMOCHEMISTRY_VARIABLE_H
#define THERMOCHEMISTRY_VARIABLE_H

#include "SciantixVariable.h"
#include <map>


/**
 * @class ThermochemistryVariable
 * @brief A specialized variable class that extends SciantixVariable with the additional physical
 * attributes needed by the thermochemistry module: manifest index, chemical state and location,
 * phase composition, and theoretical density.
 *
 * Value bookkeeping (UOM, initial/final values, rescaling, output flag) is inherited from
 * SciantixVariable rather than reimplemented here.
 *
 * @author E. Cappellari
 *
 */
class ThermochemistryVariable : public SciantixVariable
{
protected:
    int index;
    std::string location;
    std::string phase;
    std::map <std::string, double> composition;
    std::map<int, std::map<std::string, double>> sublattice_composition;
    double theoretical_density;

public:

    ThermochemistryVariable(int index, std::string name, std::string uom, double initial_value, double final_value, std::string  phase, std::string  location, bool output, double theoretical_density = 0.0) :
        SciantixVariable(name, uom, initial_value, final_value, output)
    {
        this->index = index;
        this->phase = phase;
        this->location = location;
        this->theoretical_density = theoretical_density;
    }

    void setIndex(int i)
    {
        index = i;
    }

    int getIndex()
    {
        return index;
    }

    /**
     * @brief Sets the location for this variable.
     * @param loc The string representing the location.
     */
    void setLocation(std::string loc);

    /**
     * @brief Retrieves the location of the compounds.
     * @return The unit compound location as a string.
     */
    std::string getLocation();

    /**
     * @brief Sets the phase for this variable.
     * @param ph The string representing the phase of the compound.
     */
    void setPhase(std::string ph);

    /**
     * @brief Retrieves the compound phase.
     * @return The phase as a string.
     */
    std::string getPhase();

    /**
     * @brief Sets the dynamic composition for the compound from thermochemistry outputs.
     * @param composition_map The map representing composition ratios per element.
     */
    void setComposition(std::map <std::string, double> composition_map);

    /**
     * @brief Retrieves the dynamic composition of the compound.
     * @return The composition as a map.
     */
    std::map <std::string, double> getComposition();

    /**
     * @brief Sets the dynamic constituent fractions by sublattice.
     * @param composition_map The map from sublattice index to constituent fractions.
     */
    void setSublatticeComposition(std::map<int, std::map<std::string, double>> composition_map);

    /**
     * @brief Retrieves the dynamic constituent fractions by sublattice.
     * @return The sublattice composition map.
     */
    std::map<int, std::map<std::string, double>> getSublatticeComposition();

    /**
     * @brief Calculates the molar mass from the compound stoichiometry.
     * @return The molar mass in g/mol.
     */
    double getMolarMass();

    /**
     * @brief Retrieves the tracked mass stored in the variable.
     * @return The mass concentration in g/m3.
     */
    double getMass();

    /**
     * @brief Sets the theoretical (crystallographic) density of the compound.
     * @param density The theoretical density in g/m3.
     */
    void setTheoreticalDensity(double density)
    {
        theoretical_density = density;
    }

    /**
     * @brief Retrieves the theoretical (crystallographic) density of the compound.
     * @return The theoretical density in g/m3, or 0.0 if not provided in the manifest.
     */
    double getTheoreticalDensity()
    {
        return theoretical_density;
    }

    /**
     * @brief Constructor
     */
    ThermochemistryVariable() {}
    
    /**
     * @brief Destructor
     */
    ~ThermochemistryVariable() {}
};

#endif
