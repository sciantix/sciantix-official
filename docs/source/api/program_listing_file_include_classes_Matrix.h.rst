
.. _program_listing_file_include_classes_Matrix.h:

Program Listing for File Matrix.h
=================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_Matrix.h>` (``include/classes/Matrix.h``)

.. |exhale_lsh| unicode:: U+021B0 .. UPWARDS ARROW WITH TIP LEFTWARDS

.. code-block:: cpp

   //       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
   //      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
   //     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
   //      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
   //  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
   //  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
   //                                                                                  //
   //  Originally developed by D. Pizzocri & T. Barani                                 //
   //                                                                                  //
   //  Version: 2.2.1                                                                    //
   //  Year: 2025                                                                      //
   //  Authors: D. Pizzocri, G. Zullo.                                                 //
   //                                                                                  //
   
   #ifndef MATRIX_H
   #define MATRIX_H
   
   #include <cmath>
   
   #include "Constants.h"
   #include "ErrorMessages.h"
   #include "Material.h"
   #include "SciantixArray.h"
   #include "SciantixVariable.h"
   
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
   
       // Mechanical properties
       double elastic_modulus;
       double poisson_ratio;
       double grain_boundary_fracture_energy;
       double shear_modulus;
   
       void setTheoreticalDensity(double m)
       {
           matrix_density = m;
       }
   
       double getTheoreticalDensity()
       {
           return matrix_density;
       }
   
       void setLatticeParameter(double m)
       {
           lattice_parameter = m;
       }
   
       double getLatticeParameter()
       {
           return lattice_parameter;
       }
   
       void setSurfaceTension(double r)
       {
           surface_tension = r;
       }
   
       double getSurfaceTension()
       {
           return surface_tension;
       }
   
       void setSchottkyVolume(double v)
       {
           schottky_defect_volume = v;
       }
   
       double getSchottkyVolume()
       {
           return schottky_defect_volume;
       }
   
       void setOctahedralInterstitialSite(double v)
       {
           ois_volume = v;
       }
   
       double getOctahedralInterstitialSite()
       {
           return ois_volume;
       }
   
       void setGrainBoundaryMobility(int                              input_value,
                                     SciantixArray<SciantixVariable>& history_variable);
   
       double getGrainBoundaryMobility()
       {
           return grain_boundary_mobility;
       }
   
       void setFissionFragmentRange(double r)
       {
           ff_range = r;
       }
   
       double getFissionFragmentRange()
       {
           return ff_range;
       }
   
       void setFissionFragmentInfluenceRadius(double r)
       {
           ff_influence_radius = r;
       }
   
       double getFissionFragmentInfluenceRadius()
       {
           return ff_influence_radius;
       }
   
       void setSemidihedralAngle(double sda)
       {
           semidihedral_angle = sda;
       }
   
       double getSemidihedralAngle()
       {
           return semidihedral_angle;
       }
   
       void setGrainBoundaryThickness(double gbt)
       {
           grain_boundary_thickness = gbt;
       }
   
       double getGrainBoundaryThickness()
       {
           return grain_boundary_thickness;
       }
   
       void setGrainBoundaryVacancyDiffusivity(int                              input_value,
                                               SciantixArray<SciantixVariable>& history_variable);
   
       double getGrainBoundaryVacancyDiffusivity()
       {
           return grain_boundary_diffusivity;
       }
   
       void setLenticularShapeFactor(double lsf)
       {
           lenticular_shape_factor = lsf;
       }
   
       double getLenticularShapeFactor()
       {
           return lenticular_shape_factor;
       }
   
       void setNucleationRate(double n)
       {
           nucleation_rate = n;
       }
   
       double getNucleationRate()
       {
           return nucleation_rate;
       }
   
       void setPoreNucleationRate(SciantixArray<SciantixVariable>& sciantix_variable);
   
       double getPoreNucleationRate()
       {
           return pore_nucleation_rate;
       }
   
       void setPoreResolutionRate(SciantixArray<SciantixVariable>& sciantix_variable,
                                  SciantixArray<SciantixVariable>& history_variable);
   
       double getPoreResolutionRate()
       {
           return pore_resolution_rate;
       }
   
       void setPoreTrappingRate(SciantixArray<Matrix>&           matrices,
                                SciantixArray<SciantixVariable>& sciantix_variable);
   
       double getPoreTrappingRate()
       {
           return pore_trapping_rate;
       }
   
       void setGrainRadius(double gr)
       {
           grain_radius = gr;
       }
   
       double getGrainRadius()
       {
           return grain_radius;
       }
   
       void setHealingTemperatureThreshold(double t)
       {
           healing_temperature_threshold = t;
       }
   
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
           return chromium_content;
       }
   
       void setChromiumSolubility(double cs)
       {
           chromium_solubility = cs;
       }
   
       double getChroimumSolubility()
       {
           return chromium_solubility;
       }
   
       void setChromiaSolubility(double crs)
       {
           Cr2O3_solubility = crs;
       }
   
       double getChromiaSolubility()
       {
           return Cr2O3_solubility;
       }
   
       void setChromiumSolution(double cr_sol)
       {
           chromium_solution = cr_sol;
       }
   
       double getChromiumSolution()
       {
           return chromium_solution;
       }
   
       void setChromiumPrecipitate(double cr_p)
       {
           chromium_precipitate = cr_p;
       }
   
       double getChromiumPrecipitate()
       {
           return chromium_precipitate;
       }
   
       void setChromiaSolution(double chromia_sol)
       {
           chromia_solution = chromia_sol;
       }
   
       double getChromiaSolution()
       {
           return chromia_solution;
       }
   
       void setChromiaPrecipitate(double chromia_p)
       {
           chromia_precipitate = chromia_p;
       }
   
       double getChromiaPrecipitate()
       {
           return chromia_precipitate;
       }
   
       void setElasticModulus(double e)
       {
           // Member function to set the elastic (Young) modulus of the material (MPa)
           elastic_modulus = e;
       }
   
       double getElasticModulus()
       {
           // Member function to return the elastic (Young) modulus of the material (MPa)
           return elastic_modulus;
       }
   
       // Poisson ratio
       void setPoissonRatio(double v)
       {
           // Member function to set the Poisson ratio of the material
           poisson_ratio = v;
       }
   
       double getPoissonRatio()
       {
           // Member function to return the Poisson ratio of the material
           return poisson_ratio;
       }
   
       // Grain-boundary fracture energy
       void setGrainBoundaryFractureEnergy(double v)
       {
           // Member function to set the grain-boundary fracture energy of the material (J/m2)
           grain_boundary_fracture_energy = v;
       }
   
       double getGrainBoundaryFractureEnergy()
       {
           // Member function to return the grain-boundary fracture energy of the material (J/m2)
           return grain_boundary_fracture_energy;
       }
   
       void setShearModulus(double g)
       {
           // Member function to set the shear modulus of the material (MPa)
           shear_modulus = g;
       }
   
       double getShearModulus()
       {
           // Member function to return the shear modulus of the material (MPa)
           return shear_modulus;
       }
   
       Matrix() {}
   
       ~Matrix() {}
   };
   
   #endif  // MATRIX_H
