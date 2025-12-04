
.. _program_listing_file_include_operations_SetMatrix.h:

Program Listing for File SetMatrix.h
====================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_operations_SetMatrix.h>` (``include/operations/SetMatrix.h``)

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
   
   #ifndef SET_MATRIX_H
   #define SET_MATRIX_H
   
   #include "Simulation.h"
   
   Matrix UO2(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable, SciantixArray<InputVariable> &scaling_factor);
   
   Matrix UO2HBS(SciantixArray<Matrix> &matrices, SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable, SciantixArray<InputVariable> &input_variable);
   
   #endif
