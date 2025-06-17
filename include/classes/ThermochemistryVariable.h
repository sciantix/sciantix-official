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

#ifndef THERMOCHEMISTRY_VARIABLE_H
#define THERMOCHEMISTRY_VARIABLE_H

#include "Variable.h"
#include <map>   

/**
 * @class ThermochemistryVariable
 * @brief A specialized variable class that extends the Variable class with physical attributes and functionalities: thermochemistry module.
 *
 * ThermochemistryVariable includes features such as unit of measure (UOM), final and initial values, chemical state and location, stoichiometry composition
 * and mechanisms for adjusting these values through specific operations.
 * 
 * @author E. Cappellari
 * 
 */
class ThermochemistryVariable : virtual public Variable
{
protected:
    std::string uom;
    double final_value;
    double initial_value;
    std::string location;
    std::string phase;
    std::map <std::string, int> stoichiometry;
    bool to_output;

public:

    ThermochemistryVariable(std::string name, std::string uom, double initial_value, double final_value, std::string  phase, std::string  location, std::map <std::string, int> stoichiometry, bool output)
    {
        this->name = name;
        this->uom = uom;
        this->initial_value = initial_value;
        this->final_value = final_value;
        this->phase = phase;
        this->location = location;
        this->stoichiometry = stoichiometry;
        this->to_output = output;
    }

    /**
     * @brief Rescales the initial value by a specified factor.
     * @param factor The multiplier to apply to the initial value.
     */
    void rescaleInitialValue(const double factor);

    /**
     * @brief Rescales the final value by a specified factor.
     * @param factor The multiplier to apply to the final value.
     */
    void rescaleFinalValue(const double factor);

    /**
     * @brief Adds a specified value to the final value.
     * @param v The value to add to the final value.
     */
    void addValue(const double v);

    /**
     * @brief Sets the unit of measure for this variable.
     * @param s The string representing the unit of measure.
     */
    void setUOM(std::string s);

    /**
     * @brief Retrieves the unit of measure.
     * @return The unit of measure as a string.
     */
    std::string getUOM();

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
     * @brief Sets the stoichiometry for the compound.
     * @param stoic The map representing the stoichiometry of the compound.
     */
    void setStoichiometry(std::map <std::string, int> stoic);

    /**
     * @brief Retrieves the stoichiometry of the compound.
     * @return The stoichiometry as a map.
     */
    std::map <std::string, int> getStoichiometry();

    /**
     * @brief Sets the final value equal to the initial value, making the variable constant over time.
     */
    void setConstant();

    /**
     * @brief Resets the initial value to match the final value, synchronizing them.
     */
    void resetValue();

    /**
     * @brief Sets the final value to a specified value.
     * @param FinalValue The value to set as the final value.
     */
    void setFinalValue(double FinalValue);

    /**
     * @brief Sets the initial value to a specified value.
     * @param InitialValue The value to set as the initial value.
     */
    void setInitialValue(double InitialValue);

    /**
     * @brief Retrieves the final value of the variable.
     * @return The final value.
     */
    double getFinalValue();

    /**
     * @brief Retrieves the initial value of the variable.
     * @return The initial value.
     */
    double getInitialValue();

    /**
     * @brief Calculates and returns the increment between the final and initial values.
     * @return The calculated increment.
     */
    double getIncrement();

    /**
     * @brief Sets whether the variable should be included in output.
     * @param io A boolean value indicating output inclusion.
     */
    void setOutput(bool io);

    /**
     * @brief Checks if the variable is marked for output.
     * @return True if the variable is to be included in output, false otherwise.
     */
    bool getOutput();

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
