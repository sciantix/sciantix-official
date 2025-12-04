
.. _program_listing_file_include_classes_Solver.h:

Program Listing for File Solver.h
=================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_Solver.h>` (``include/classes/Solver.h``)

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
   
   #ifndef SOLVER_h
   #define SOLVER_h
   
   #include <vector>
   #include <string>
   #include <cmath>
   #include "InputVariable.h"
   
   class Solver : virtual public InputVariable
   {
   public:
   
       double Integrator(double initial_value, double parameter, double increment);
   
   
       double LimitedGrowth(double initial_value, std::vector<double> parameter, double increment);
   
   
       double Decay(double initial_condition, double decay_rate, double source_term, double increment);
   
   
       double BinaryInteraction(double initial_condition, double interaction_coefficient, double increment);
   
   
       double SpectralDiffusion(double *initial_condition, std::vector<double> parameter, double increment);
   
   
       double dotProduct1D(std::vector<double> u, double v[], int n);
   
   
       void dotProduct2D(double A[], double v[], int n_rows, const int n_col, double result[]);
   
       void SpectralDiffusion2equations(double &gas_1, double &gas_2, double *initial_condition_gas_1, double *initial_condition_gas_2, std::vector<double> parameter, double increment);
   
   
       void SpectralDiffusion3equations(double &gas_1, double &gas_2, double &gas_3, double *initial_condition_gas_1, double *initial_condition_gas_2, double *initial_condition_gas_3, std::vector<double> parameter, double increment);
   
       void Laplace2x2(double A[], double b[]);
   
   
       void Laplace3x3(double A[], double b[]);
   
   
       double det(int N, double A[]);
   
       void Laplace(int N, double A[], double b[]);
   
       double QuarticEquation(std::vector<double> parameter);
   
       void modeInitialization(int n_modes, double mode_initial_condition, double *diffusion_modes);
   
       double NewtonBlackburn(std::vector<double> parameter);
       double NewtonLangmuirBasedModel(double initial_value, std::vector<double> parameter, double increment);
   
       Solver() {}
       ~Solver() {}
   };
   
   #endif // SOLVER_H
