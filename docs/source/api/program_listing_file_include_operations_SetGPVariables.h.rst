
.. _program_listing_file_include_operations_SetGPVariables.h:

Program Listing for File SetGPVariables.h
=========================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_operations_SetGPVariables.h>` (``include/operations/SetGPVariables.h``)

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
   //  Version: 2.0                                                                    //
   //  Year: 2022                                                                      //
   //  Authors: D. Pizzocri, G. Zullo, G. Nicodemo                                     //
   //                                                                                  //
   
   #ifndef SET_GPVARIABLE_H
   #define SET_GPVARIABLE_H
   
   #include <vector>
   
   #include "InputVariable.h"
   #include "SciantixArray.h"
   #include "SetVariablesFunctions.h"
   
   void SetGPVariables(int                              Sciantix_options[],
                       double                           Sciantix_history[],
                       double                           Sciantix_variables[],
                       SciantixArray<InputVariable>&    input_variable,
                       SciantixArray<SciantixVariable>& history_variable,
                       SciantixArray<SciantixVariable>& sciantix_variable,
                       SciantixArray<SciantixVariable>& physics_variable);
   
   #endif
