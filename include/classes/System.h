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

#ifndef SYSTEM_H
#define SYSTEM_H

#include "Matrix.h"
#include "Gas.h"
#include "Particle.h"
#include "Constants.h"
#include "ErrorMessages.h"
#include "SciantixArray.h"
#include "SciantixVariable.h"
#include "InputVariable.h"
#include <cmath>
#include <vector>

/**
 * @class System
 * @brief Class derived from Material to include the properties that depend on both the fission gas and the fuel matrix.
 * 
 * @author G. Zullo
 * @author F. Bastien
 */
class System: virtual public Material
{
protected:

    std::string reference;
    std::string name;

    double yield;
    double radius_in_lattice;
    double volume_in_lattice;
    double diffusivity;
    double bubble_diffusivity;
    double henry_constant;
    double resolution_rate;
    double trapping_rate;
    double nucleation_rate;
    double pore_nucleation_rate;
    std::vector<double> modes;
    double production_rate;
    bool restructured_matrix;

    Gas gas;
    Matrix matrix;
    Particle particle;

public:
    /**
     * @brief Sets the matrix restructuring status.
     * @param y True if the matrix is restructured, false otherwise.
     */
    void setRestructuredMatrix(bool y);

    /**
     * @brief Gets the restructuring status of the matrix.
     * @return True if the matrix is restructured, false otherwise.
     */
    bool getRestructuredMatrix();

    /**
     * @brief Sets the cumulative yield of the fission gas.
     * @param y Yield to set (atoms per fission).
     */
    void setYield(SciantixArray<SciantixVariable> &sciantix_variable);

    /**
     * @brief Gets the cumulative yield of the fission gas.
     * @return Yield of the gas (atoms per fission).
     */
    double getYield();

    /**
     * @brief Sets the radius of the fission gas atom in the matrix lattice.
     * @param r Radius to set (meters).
     */
    void setRadiusInLattice(double r);
    /**
     * @brief Gets the radius of the fission gas atom in the matrix lattice.
     * @return Radius of the gas atom (meters).
     */
    double getRadiusInLattice();

    /**
     * @brief Sets the name of the gas.
     * @param n Name of the gas to set.
     */
    void setGas(Gas g);

    Gas getGas();

    /**
     * @brief Gets the name of the gas.
     * @return Name of the gas.
     */
    std::string getGasName();
    /**
     * @brief Sets the name of the matrix.
     * @param n Name of the matrix to set.
     */
    void setMatrix(Matrix m);

    Matrix getMatrix();

    /**
     * @brief Gets the name of the matrix.
     * @return Name of the matrix.
     */
    std::string getMatrixName();
    /**
     * @brief Sets the particle (Five Metals) in the system.
     * @param p Particle object.
     */
    void setParticle(Particle p);

    /**
     * @brief Gets the particle object.
     * @return Particle object.
     */
    Particle getParticle();

    /**
     * @brief Gets the name of the particle.
     * @return Name of the particle (e.g., "Mo", "Ru").
     */
    std::string getParticleName();
    /**
     * @brief Gets the volume occupied by the gas in the matrix.
     * @return Volume occupied by the gas.
     */
    double getVolumeInLattice();
    /**
     * @brief Sets the volume occupied by the gas in the matrix.
     * @param v Volume to set.
     */
    void setVolumeInLattice(double v);

    /**
     * @brief Sets the diffusivity of bubbles within the matrix based on input values.
     * @param input_value The model selection index for bubble diffusivity.
     */
    void setBubbleDiffusivity(int input_value, SciantixArray<SciantixVariable> &sciantix_variable, 
        SciantixArray<SciantixVariable> &history_variable, SciantixArray<Matrix> &matrices);

    /**
     * @brief Retrieves the diffusivity of bubbles within the matrix.
     * @return The current bubble diffusivity.
     */
    double getBubbleDiffusivity();

    /**
     * @brief Sets the helium diffusivity within the matrix based on the selected model.
     * The intra-granular helium diffusivity within the fuel grain is set according to the input_variable iHeDiffusivity
     * @param input_value The model selection index for helium diffusivity.
     */
    void setHeliumDiffusivity(int input_value, SciantixArray<SciantixVariable> &history_variable);

    /**
     * @brief Retrieves the helium diffusivity within the matrix.
     * @return The current helium diffusivity.
     */
    double getHeliumDiffusivity();

    /**
     * @brief Sets the diffusivity of fission gases within the matrix based on the selected model.
     * The intra-granular fission gas (xenon and krypton) diffusivity within the fuel grain is set according to the input_variable iFissionGasDiffusivity
     * @param input_value The model selection index for fission gas diffusivity.
     */
    void setFissionGasDiffusivity(int input_value, SciantixArray<SciantixVariable> &sciantix_variable,
        SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors);

    /**
     * @brief Retrieves the diffusivity of fission gases within the matrix.
     * @return The current fission gas diffusivity.
     */
    double getFissionGasDiffusivity();

    /**
     * @brief Sets the Henry's constant value.
     * @param h The new value for Henry's constant.
     */
    void setHenryConstant(double h);

    /**
     * @brief Retrieves the Henry's constant value.
     * @return The current value of Henry's constant.
     */
    double getHenryConstant();

    /**
     * @brief Sets the resolution rate for isotopes from nanobubbles in the matrix.
     * The helium intra-granular resolution rate is set according to the input_variable iResolutionRate.
     * @param input_value The model selection index for setting the resolution rate.
     */
    void setResolutionRate(int input_value, SciantixArray<SciantixVariable> &sciantix_variable, 
        SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors, SciantixArray<Matrix> &matrices);

    /**
     * @brief Retrieves the resolution rate for isotopes from nanobubbles in the matrix.
     * @return The current resolution rate.
     */
    double getResolutionRate();

    /**
     * @brief Sets the trapping rate for isotopes in nanobubbles within the matrix.
     * The krypton intra-granular trapping rate is set according to the input_variable iTrappingRate.
     * @param input_value The model selection index for setting the trapping rate.
     */
    void setTrappingRate(int input_value, SciantixArray<SciantixVariable> &sciantix_variable, 
        SciantixArray<InputVariable> &scaling_factors);

    /**
     * @brief Retrieves the trapping rate for isotopes in nanobubbles within the matrix.
     * @return The current trapping rate.
     */
    double getTrappingRate();

    /**
     * @brief Sets the nucleation rate based on the selected model.
     * Evaluation of the nucleation rate of intragranular gas bubble inside the UO<sub>2</sub> matrix
     * @param input_value The model selection index for nucleation rate.
     */
    void setNucleationRate(int input_value, SciantixArray<SciantixVariable> &history_variable, 
        SciantixArray<InputVariable> &scaling_factors);

    /**
     * @brief Retrieves the nucleation rate.
     * @return The current nucleation rate.
     */
    double getNucleationRate();

    /**
     * @brief Sets the pore nucleation rate.
     * @param t The new pore nucleation rate.
     */
    void setPoreNucleationRate(double t);

    /**
     * @brief Retrieves the pore nucleation rate.
     * @return The current pore nucleation rate.
     */
    double getPoreNucleationRate();

    /**
     * @brief Sets the production rate based on the selected model.
     * @param input_value The model selection index for setting the production rate.
     */
    void setProductionRate(int input_value, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable,
        SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<InputVariable> &scaling_factors);

    /**
     * @brief  Member function to get the production rate of the sciantix_system.
     * @return The current production rate.
     */
    double getProductionRate();
    /**
     * @brief Default constructor for the System class.
     */
    System() {}
    /**
     * @brief Destructor for the System class
     */
    ~System() {}
};

#endif // SYSTEM_H
