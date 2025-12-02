
.. _program_listing_file_include_classes_System.h:

Program Listing for File System.h
=================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_System.h>` (``include/classes/System.h``)

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
   //  Version: 2.1                                                                    //
   //  Year: 2024                                                                      //
   //  Authors: D. Pizzocri, G. Zullo.                                                 //
   //                                                                                  //
   
   #ifndef SYSTEM_H
   #define SYSTEM_H
   
   #include "Matrix.h"
   #include "Gas.h"
   #include "Constants.h"
   #include "ErrorMessages.h"
   #include "SciantixArray.h"
   #include "SciantixVariable.h"
   #include "InputVariable.h"
   #include <cmath>
   #include <vector>
   
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
   
   public:
       void setRestructuredMatrix(bool y);
   
       bool getRestructuredMatrix();
   
       void setYield(double y);
   
       double getYield();
   
       void setRadiusInLattice(double r);
       double getRadiusInLattice();
   
       void setGas(Gas g);
   
       Gas getGas();
   
       std::string getGasName();
       void setMatrix(Matrix m);
   
       Matrix getMatrix();
   
       std::string getMatrixName();
   
       double getVolumeInLattice();
       void setVolumeInLattice(double v);
   
       void setBubbleDiffusivity(int input_value, SciantixArray<SciantixVariable> &sciantix_variable, 
           SciantixArray<SciantixVariable> &history_variable, SciantixArray<Matrix> &matrices);
   
       double getBubbleDiffusivity();
   
       void setHeliumDiffusivity(int input_value, SciantixArray<SciantixVariable> &history_variable);
   
       double getHeliumDiffusivity();
   
       void setFissionGasDiffusivity(int input_value, SciantixArray<SciantixVariable> &sciantix_variable,
           SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors);
   
       double getFissionGasDiffusivity();
   
       void setHenryConstant(double h);
   
       double getHenryConstant();
   
       void setResolutionRate(int input_value, SciantixArray<SciantixVariable> &sciantix_variable, 
           SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &scaling_factors, SciantixArray<Matrix> &matrices);
   
       double getResolutionRate();
   
       void setTrappingRate(int input_value, SciantixArray<SciantixVariable> &sciantix_variable, 
           SciantixArray<InputVariable> &scaling_factors);
   
       double getTrappingRate();
   
       void setNucleationRate(int input_value, SciantixArray<SciantixVariable> &history_variable, 
           SciantixArray<InputVariable> &scaling_factors);
   
       double getNucleationRate();
   
       void setPoreNucleationRate(double t);
   
       double getPoreNucleationRate();
   
       void setProductionRate(int input_value, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable,
           SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<InputVariable> &scaling_factors);
   
       double getProductionRate();
       System() {}
       ~System() {}
   };
   
   #endif // SYSTEM_H
