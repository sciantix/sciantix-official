
.. _program_listing_file_include_operations_SetVariables.h:

Program Listing for File SetVariables.h
=======================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_operations_SetVariables.h>` (``include/operations/SetVariables.h``)

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
   
   #ifndef SET_VARIABLE_H
   #define SET_VARIABLE_H
   
   #include <vector>
   
   #include "InputVariable.h"
   #include "SciantixArray.h"
   #include "SetVariablesFunctions.h"
   
   void SetVariables(int Sciantix_options[], double Sciantix_history[], double Sciantix_variables[],
                     double Sciantix_scaling_factors[], double Sciantix_diffusion_modes[],
                     SciantixArray<InputVariable>&    input_variable,
                     SciantixArray<SciantixVariable>& history_variable,
                     SciantixArray<SciantixVariable>& sciantix_variable,
                     SciantixArray<SciantixVariable>& physics_variable,
                     std::vector<double>&             modes_initial_conditions,
                     SciantixArray<Variable>&         scaling_factors);
   
   #endif
