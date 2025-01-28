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

#ifndef MATRIX_H
#define MATRIX_H

#include <cmath>

#include "Material.h"
#include "Constants.h"
#include "ErrorMessages.h"
#include "SciantixArray.h"
#include "SciantixVariable.h"

/**
 * @class Matrix
 * @brief Represents the fuel matrix material such as UO2, UO2-HBS, or MOX, derived from the Material class.
 *
 * This class extends the Material class to incorporate properties specific to nuclear fuel matrices,
 * including physical and nuclear properties like lattice parameters, grain boundary properties, and fission fragment characteristics.
 * 
 * @author G. Zullo
 */
class Matrix : virtual public Material
{
public:
    double matrix_density;
    double lattice_parameter;
    double grain_boundary_mobility;
    double ff_range;
    double ff_influence_radius;
    double surface_tension;
    double schottky_defect_volume;
    double ois_volume;
    double grain_boundary_thickness;
    double grain_boundary_diffusivity;
    double semidihedral_angle;
    double lenticular_shape_factor;
    double grain_radius;
    double healing_temperature_threshold;
    double nucleation_rate;
    double pore_nucleation_rate;
    double pore_resolution_rate;
    double pore_trapping_rate;
    double chromium_content;
	double chromium_solubility;
	double Cr2O3_solubility;
	double chromium_solution;
	double chromium_precipitate;
	double chromia_solution;
	double chromia_precipitate;

	double elastic_modulus;
	double poisson_ratio;
    double thermal_conductivity;

    /**
     * @brief Sets the theoretical density of the matrix.
     * @param m The density to set (kg/m3).
     */
    void setTheoreticalDensity(double m)
    {
        matrix_density = m;
    }

    /**
     * @brief Retrieves the theoretical density of the matrix.
     * @return A double that is the density of the matrix (kg/m3).
     */
    double getTheoreticalDensity()
    {
        return matrix_density;
    }

    /**
     * @brief Sets the lattice parameter of the matrix.
     * @param m The lattice parameter to set.
     */
    void setLatticeParameter(double m)
    {
        lattice_parameter = m;
    }

    /**
     * @brief Retrieves the lattice parameter of the matrix.
     * @return The lattice parameter of the matrix.
     */
    double getLatticeParameter()
    {
        return lattice_parameter;
    }

    /**
     * @brief Sets the surface tension of the matrix material.
     * @param r The surface tension to set (N/m).
     */
    void setSurfaceTension(double r)
    {
        surface_tension = r;
    }

    /**
     * @brief Retrieves the surface tension of the matrix material.
     * @return The surface tension of the matrix (N/m).
     */
    double getSurfaceTension()
    {
        return surface_tension;
    }

    /**
     * @brief Sets the volume of a Schottky defect in the matrix.
     * @param v The volume of the Schottky defect to set (m3).
     */
    void setSchottkyVolume(double v)
    {
        schottky_defect_volume = v;
    }

    /**
     * @brief Retrieves the volume of a Schottky defect in the matrix.
     * @return The volume of the Schottky defect (m3).
     */
    double getSchottkyVolume()
    {
        return schottky_defect_volume;
    }

    /**
     * @brief Sets the volume of an octahedral interstitial site in the matrix.
     * @param v The volume to set (m3).
     */
    void setOctahedralInterstitialSite(double v)
    {
        ois_volume = v;
    }

    /**
     * @brief Retrieves the volume of an octahedral interstitial site in the matrix.
     * @return The volume of the OIS (m3).
     */
    double getOctahedralInterstitialSite()
    {
        return ois_volume;
    }

    /**
     * @brief Sets the grain boundary mobility based on the selected model.
     *
     * @param input_value The model selection for grain boundary mobility.
     */
    void setGrainBoundaryMobility(int input_value, SciantixArray<SciantixVariable> &history_variable);

    /**
     * @brief Retrieves the mobility of the grain boundaries of the matrix.
     * @return The grain boundary mobility.
     */
    double getGrainBoundaryMobility()
    {
        return grain_boundary_mobility;
    }

    /**
     * @brief Sets the average range of the fission fragments in the matrix.
     * @param r The range to set (m).
     */
    void setFissionFragmentRange(double r)
    {
        ff_range = r;
    }

    /**
     * @brief Retrieves the average range of the fission fragments in the matrix.
     * @return The range of the fission fragments (m).
     */
    double getFissionFragmentRange()
    {
        return ff_range;
    }

    /**
     * @brief Sets the radius of influence of the fission fragment track.
     * @param r The radius to set (m).
     */
    void setFissionFragmentInfluenceRadius(double r)
    {
        ff_influence_radius = r;
    }

    /**
     * @brief Retrieves the radius of influence of the fission fragment track.
     * @return The radius of influence (m).
     */
    double getFissionFragmentInfluenceRadius()
    {
        return ff_influence_radius;
    }

    /**
     * @brief Sets the semidihedral angle.
     * @param sda The semidihedral angle to set.
     */
    void setSemidihedralAngle(double sda)
    {
        semidihedral_angle = sda;
    }

    /**
     * @brief Retrieves the semidihedral angle.
     * @return The semidihedral angle.
     */
    double getSemidihedralAngle()
    {
        return semidihedral_angle;
    }

    /**
     * @brief Sets the thickness of the grain boundary.
     * @param gbt The thickness to set (m).
     */
    void setGrainBoundaryThickness(double gbt)
    {
        grain_boundary_thickness = gbt;
    }

    /**
     * @brief Retrieves the thickness of the grain boundary.
     * @return The thickness of the grain boundary (m).
     */
    double getGrainBoundaryThickness()
    {
        return grain_boundary_thickness;
    }

    /**
     * @brief Sets the diffusivity of vacancies on the grain boundaries based on the input model.
     * @param input_value The model selection for grain boundary vacancy diffusivity.
     */
    void setGrainBoundaryVacancyDiffusivity(int input_value, SciantixArray<SciantixVariable> &history_variable);

    /**
     * @brief Retrieves the vacancy diffusivity on the grain boundaries.
     * @return The grain boundary vacancy diffusivity.
     */
    double getGrainBoundaryVacancyDiffusivity()
    {
        return grain_boundary_diffusivity;
    }

    /**
     * @brief Sets the lenticular shape factor.
     * @param lsf The lenticular shape factor to set (/).
     */
    void setLenticularShapeFactor(double lsf)
    {
        lenticular_shape_factor = lsf;
    }

    /**
     * @brief Retrieves the lenticular shape factor.
     * @return The lenticular shape factor (/).
     */
    double getLenticularShapeFactor()
    {
        return lenticular_shape_factor;
    }

    /**
     * @brief Sets the nucleation rate under irradiation.
     * @param n The nucleation rate to set (1/s).
     */
    void setNucleationRate(double n)
    {
        nucleation_rate = n;
    }

    /**
     * @brief Retrieves the nucleation rate under irradiation.
     * @return The nucleation rate (1/s).
     */
    double getNucleationRate()
    {
        return nucleation_rate;
    }

    /**
     * @brief Sets the nucleation rate of pores in high burnup structures (HBS).
     *
     * Calculates the nucleation rate based on current simulation parameters and a predefined model.
     * The model used is defined by @ref <a href="https://www.sciencedirect.com/science/article/pii/S0022311522001234" target="_blank">Barani T. et al (2022). Journal of Nuclear Materials, 563, 153627.</a>
     */
    void setPoreNucleationRate(SciantixArray<SciantixVariable> &sciantix_variable);

    /**
     * @brief Retrieves the nucleation rate of pores.
     * @return The pore nucleation rate (1/s).
     * 
     * The model used is defined by @ref <a href="https://www.sciencedirect.com/science/article/pii/S0022311522001234" target="_blank">Barani T. et al (2022). Journal of Nuclear Materials, 563, 153627.</a>
     */
    double getPoreNucleationRate()
    {
        return pore_nucleation_rate;
    }

    /**
     * @brief Sets the resolution rate of gas atoms from HBS pores.
     *
     * The re-solution rate is calculated based on current simulation parameters and a model from
     * Barani et al., JNM 563 (2022) 153627.
     */
    void setPoreResolutionRate(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable);

    /**
     * @brief Retrieves the resolution rate of gas atoms from pores.
     * @return The pore resolution rate (1/s).
     */
    double getPoreResolutionRate()
    {
        return pore_resolution_rate;
    }

    /**
     * @brief Sets the trapping rate of gas atoms in HBS pores.
     *
     * The trapping rate is calculated based on current simulation parameters and a model from
     * Barani et al., JNM 563 (2022) 153627.
     */
    void setPoreTrappingRate(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable);

    /**
     * @brief Retrieves the trapping rate of gas atoms in pores.
     * @return The pore trapping rate (1/s).
     */
    double getPoreTrappingRate()
    {
        return pore_trapping_rate;
    }

    /**
     * @brief Sets the grain radius of the matrix.
     * @param gr The grain radius to set.
     */
    void setGrainRadius(double gr)
    {
        grain_radius = gr;
    }

    /**
     * @brief Retrieves the grain radius of the matrix.
     * @return The grain radius.
    */
    double getGrainRadius()
    {
        return grain_radius;
    }

    /**
     * @brief Sets the temperature limit for complete healing of extended defects in the fuel matrix.
     * @param t The temperature threshold to set (K).
     */
    void setHealingTemperatureThreshold(double t)
    {
        healing_temperature_threshold = t;
    }

    /**
     * @brief Retrieves the temperature limit for complete healing of extended defects.
     * @return The healing temperature threshold (K).
     */
    double getHealingTemperatureThreshold()
    {
        return healing_temperature_threshold;
    }

    void setChromiumContent(double cc)
	{
		chromium_content = cc;
	}

	double getChromiumContent()
	{	
		/// Member function to set the chromium content (µg/g)
		return chromium_content;
	}

	void setChromiumSolubility(double cs)
	{
		chromium_solubility = cs;
	}

	double getChroimumSolubility()
	{
		/// Member function to set the chromium solubility (weight%/UO2)	
		return chromium_solubility;
	}

	void setChromiaSolubility(double crs)
	{
		Cr2O3_solubility = crs;
	}

	double getChromiaSolubility()
	{
		/// Member function to set the chromia (Cr2O3) solubility (weight%/UO2)	
		return Cr2O3_solubility;
	}

	void setChromiumSolution(double cr_sol)
	{
		chromium_solution= cr_sol;
	}

	double getChromiumSolution()
	{
		/// Member function to set the chromium solution (kg)	
		return chromium_solution;
	}

	void setChromiumPrecipitate(double cr_p)
	{
		chromium_precipitate= cr_p;
	}

	double getChromiumPrecipitate()
	{
		/// Member function to set the chromium precipitate (kg)	
		return chromium_precipitate;
	}

	void setChromiaSolution(double chromia_sol)
	{
		chromia_solution = chromia_sol;
	}

	double getChromiaSolution()
	{
		/// Member function to set the chromia (Cr2O3) solution (kg)	
		return chromia_solution;
	}

	void setChromiaPrecipitate(double chromia_p)
	{
		chromia_precipitate = chromia_p;
	}

	double getChromiaPrecipitate()
	{
		/// Member function to set the chromia (Cr2O3) precipitate (kg)	
		return chromia_precipitate;
	}

    /**
     * @brief Sets the elastic modulus.
     * @param input_value The correlation selection for the elastic modulus.
     */
    void setElasticModulus(int input_value, SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variables, SciantixArray<SciantixVariable> &history_variable);

    /**
     * @brief Retrieves the elastic modulus.
     * @return The elastic modulus to set (GPa).
     */
    double getElasticModulus()
    {
        return elastic_modulus;
    }

    /**
     * @brief Sets the Poisson ratio.
     * @param input_value The correlation selection for the Poisson ratio.
     */
    void setPoissonRatio(int input_value);

    /**
     * @brief Retrieves Poisson ratio.
     * @return The Poisson ratio to set.
     */
    double getPoissonRatio()
    {
        return poisson_ratio;
    }

    /**
     * @brief Sets the thermal conductivity.
     * @param input_value The correlation selection for the thermal consuctivity.
     */
    void setThermalConductivity(int input_value, SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variables, SciantixArray<SciantixVariable> &history_variable);

    /**
     * @brief Retrieves the thermal conductivity.
     * @return thermal conductivity to set (W/m K).
     */
    double getThermalConductivity()
    {
        return thermal_conductivity;
    }

    /**
     * @brief Constructor
     */
    Matrix() {}

    /**
     * @brief Destructor
     */
    ~Matrix() {}
};

#endif // MATRIX_H
