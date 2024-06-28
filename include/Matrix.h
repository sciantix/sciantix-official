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

#ifndef MATRIX_H
#define MATRIX_H

#include "Material.h"

/**
 * \class Matrix
 * \brief Represents the fuel matrix material such as UO2, UO2-HBS, or MOX, derived from the Material class.
 *
 * This class extends the Material class to incorporate properties specific to nuclear fuel matrices,
 * including physical and nuclear properties like lattice parameters, grain boundary properties, and fission fragment characteristics.
 */
class Matrix : virtual public Material
{
protected:
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
	std::string matrix_name;

public:
	/**
	 * \brief Sets the theoretical density of the matrix.
	 * @param m The density to set (kg/m3).
	 */
	void setTheoreticalDensity(double m)
	{
		/// Member function to set the matrix theoretical density (kg/m3)
		matrix_density = m;
	}

	/**
	 * \brief Retrieves the theoretical density of the matrix.
	 * @return A double that is the density of the matrix (kg/m3).
	 */
	double getTheoreticalDensity()
	{
		/// Member function to get the matrix theoretical density (kg/m3)
		return matrix_density;
	}

	/**
	 * \brief Sets the lattice parameter of the matrix.
	 * @param m The lattice parameter to set.
	 */
	void setLatticeParameter(double m)
	{
		/// Member function to set the matrix lattice parameter
		lattice_parameter = m;
	}

	/**
	 * \brief Retrieves the lattice parameter of the matrix.
	 * @return The lattice parameter of the matrix.
	 */
	double getLatticeParameter()
	{
		/// Member function to get the matrix lattice parameter
		return lattice_parameter;
	}

	/**
	 * \brief Sets the surface tension of the matrix material.
	 * @param r The surface tension to set (N/m).
	 */
	void setSurfaceTension(double r)
	{
		/// Member function to set the surface tension of the matrix material (N/m)
		surface_tension = r;
	}

	/**
	 * \brief Retrieves the surface tension of the matrix material.
	 * @return The surface tension of the matrix (N/m).
	 */
	double getSurfaceTension()
	{
		/// Member function to get the surface tension of the matrix material (N/m)
		return surface_tension;
	}

	/**
	 * \brief Sets the volume of a Schottky defect in the matrix.
	 * @param v The volume of the Schottky defect to set (m3).
	 */
	void setSchottkyVolume(double v)
	{
		/// Member function to set the volume of a Schottky volume in the matrix (m3).
		schottky_defect_volume = v;
	}

	/**
	 * \brief Retrieves the volume of a Schottky defect in the matrix.
	 * @return The volume of the Schottky defect (m3).
	 */
	double getSchottkyVolume()
	{
		/// Member function to get the volume of a Schottky volume in the matrix (m3).
		return schottky_defect_volume;
	}

	/**
	 * \brief Sets the volume of an octahedral interstitial site in the matrix.
	 * @param v The volume to set (m3).
	 */
	void setOIS(double v)
	{
		/// Member function to set the volume of an octahedral interstitial site (OIS) (m3).
		ois_volume = v;
	}

	/**
	 * \brief Retrieves the volume of an octahedral interstitial site in the matrix.
	 * @return The volume of the OIS (m3).
	 */
	double getOIS()
	{
		/// Member function to get the volume of an octahedral interstitial site (OIS) (m3).
		return ois_volume;
	}

	/**
	 * \brief Sets the grain boundary mobility based on the selected model.
	 *
	 * @param input_value The model selection for grain boundary mobility.
	 */
	void setGrainBoundaryMobility(int input_value);

	/**
	 * \brief Retrieves the mobility of the grain boundaries of the matrix.
	 * @return The grain boundary mobility.
	 */
	double getGrainBoundaryMobility()
	{
		/// Member function to get the mobility of the grain boundaries of the matrix.
		return grain_boundary_mobility;
	}

	/**
	 * \brief Sets the average range of the fission fragments in the matrix.
	 * @param r The range to set (m).
	 */
	void setFFrange(double r)
	{
		/// Member function to set the (average) range of the fission fragments in the matrix (m).
		ff_range = r;
	}

	/**
	 * \brief Retrieves the average range of the fission fragments in the matrix.
	 * @return The range of the fission fragments (m).
	 */
	double getFFrange()
	{
		/// Member function to get the (average) range of the fission fragments in the matrix (m).
		return ff_range;
	}

	/**
	 * \brief Sets the radius of influence of the fission fragment track.
	 * @param r The radius to set (m).
	 */
	void setFFinfluenceRadius(double r)
	{
		/// Member function to set the radius of influence of the fission fragment track (m).
		ff_influence_radius = r;
	}

	/**
	 * \brief Retrieves the radius of influence of the fission fragment track.
	 * @return The radius of influence (m).
	 */
	double getFFinfluenceRadius()
	{
		/// Member function to get the radius of influence of the fission fragment track (m).
		return ff_influence_radius;
	}

	/**
	 * \brief Sets the semidihedral angle.
	 * @param sda The semidihedral angle to set.
	 */
	void setSemidihedralAngle(double sda)
	{
		/// Member function to set the semidihedral angle.
		semidihedral_angle = sda;
	}

	/**
	 * \brief Retrieves the semidihedral angle.
	 * @return The semidihedral angle.
	 */
	double getSemidihedralAngle()
	{
		/// Member function to get the semidihedral angle.
		return semidihedral_angle;
	}

	/**
	 * \brief Sets the thickness of the grain boundary.
	 * @param gbt The thickness to set (m).
	 */
	void setGrainBoundaryThickness(double gbt)
	{
		/// Member function to set the grain-boundary thickness (m)
		grain_boundary_thickness = gbt;
	}

	/**
	 * \brief Retrieves the thickness of the grain boundary.
	 * @return The thickness of the grain boundary (m).
	 */
	double getGrainBoundaryThickness()
	{
		/// Member function to get the grain-boundary thickness (m)
		return grain_boundary_thickness;
	}

	/**
	 * \brief Sets the diffusivity of vacancies on the grain boundaries based on the input model.
	 *
	 * @param input_value The model selection for grain boundary vacancy diffusivity.
	 */
	void setGrainBoundaryVacancyDiffusivity(int input_value);

	/**
	 * \brief Retrieves the vacancy diffusivity on the grain boundaries.
	 * @return The grain boundary vacancy diffusivity.
	 */
	double getGrainBoundaryVacancyDiffusivity()
	{
		/// Member function to get the grain-boundary vacancy diffusivity.
		return grain_boundary_diffusivity;
	}

	/**
	 * \brief Sets the lenticular shape factor.
	 * @param lsf The lenticular shape factor to set (/).
	 */
	void setLenticularShapeFactor(double lsf)
	{
		/// Member function to set the lenticular shape factor (/).
		lenticular_shape_factor = lsf;
	}

	/**
	 * \brief Retrieves the lenticular shape factor.
	 * @return The lenticular shape factor (/).
	 */
	double getLenticularShapeFactor()
	{
		/// Member function to get the lenticular shape factor (/).
		return lenticular_shape_factor;
	}

	/**
	 * \brief Sets the nucleation rate under irradiation.
	 * @param n The nucleation rate to set (1/s).
	 */
	void setNucleationRate(double n)
	{
		/// Member function to set the nucleation rate of the matrix under irradiation (1/s).
		nucleation_rate = n;
	}

	/**
	 * \brief Retrieves the nucleation rate under irradiation.
	 * @return The nucleation rate (1/s).
	 */
	double getNucleationRate()
	{
		/// Member function to get the nucleation rate of the matrix under irradiation (1/s).
		return nucleation_rate;
	}

	/**
	 * \brief Sets the nucleation rate of pores in high burnup structures (HBS).
	 *
	 * Calculates the nucleation rate based on current simulation parameters and a predefined model.
	 * The model used is defined by Barani et al., JNM 563 (2022) 153627.
	 */
	void setPoreNucleationRate();

	/**
	 * \brief Retrieves the nucleation rate of pores.
	 * @return The pore nucleation rate (1/s).
	 */
	double getPoreNucleationRate()
	{
		/// Member function to get the pore nucleation rate of the matrix.
		return pore_nucleation_rate;
	}

	/**
	 * \brief Sets the resolution rate of gas atoms from HBS pores.
	 *
	 * The re-solution rate is calculated based on current simulation parameters and a model from
	 * Barani et al., JNM 563 (2022) 153627.
	 */
	void setPoreResolutionRate();

	/**
	 * \brief Retrieves the resolution rate of gas atoms from pores.
	 * @return The pore resolution rate (1/s).
	 */
	double getPoreResolutionRate()
	{
		/// Member function to get the pore resolution rate of the matrix.
		return pore_resolution_rate;
	}

	/**
	 * \brief Sets the trapping rate of gas atoms in HBS pores.
	 *
	 * The trapping rate is calculated based on current simulation parameters and a model from
	 * Barani et al., JNM 563 (2022) 153627.
	 */
	void setPoreTrappingRate();

	/**
	 * \brief Retrieves the trapping rate of gas atoms in pores.
	 * @return The pore trapping rate (1/s).
	 */
	double getPoreTrappingRate()
	{
		/// Member function to get the pore trapping rate of the matrix.
		return pore_trapping_rate;
	}

	/**
	 * \brief Sets the grain radius of the matrix.
	 * @param gr The grain radius to set.
	 */
	void setGrainRadius(double gr)
	{
		/// Member function to set the grain radius of the matrix.
		grain_radius = gr;
	}

	/**
	 * \brief Retrieves the grain radius of the matrix.
	 * @return The grain radius.
	*/
	double getGrainRadius()
	{
		/// Member function to get the grain radius of the matrix.
		return grain_radius;
	}

	/**
	 * \brief Sets the temperature limit for complete healing of extended defects in the fuel matrix.
	 * @param t The temperature threshold to set (K).
	 */
	void setHealingTemperatureThreshold(double t)
	{
		/// Member function to set the (estimated) temperature limit for complete healing of the extended defects in the fuel matrix (K).
		healing_temperature_threshold = t;
	}

	/**
	 * \brief Retrieves the temperature limit for complete healing of extended defects.
	 * @return The healing temperature threshold (K).
	 */
	double getHealingTemperatureThreshold()
	{
		/// Member function to get the (estimated) temperature limit for complete healing of the extended defects in the fuel matrix (K).
		return healing_temperature_threshold;
	}

	/**
	 * \brief Default constructor for the Matrix class.
	 */
	Matrix() {}
	/**
	 * \brief Destructor for the Matrix class.
	 */
	~Matrix() {}
};

#endif // MATRIX_H
