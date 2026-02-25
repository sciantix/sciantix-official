
.. _program_listing_file_include_models_StoichiometryDeviation.h:

Program Listing for File StoichiometryDeviation.h
=================================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_models_StoichiometryDeviation.h>` (``include/models/StoichiometryDeviation.h``)

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
   
   #ifndef STOICHIOMETRY_DEVIATION_H
   #define STOICHIOMETRY_DEVIATION_H
   
   #include "Simulation.h"
   
   double BlackburnThermochemicalModel(double                           stoichiometry_deviation,
                                       double                           temperature,
                                       SciantixArray<SciantixVariable>& sciantix_variable);
   
   #endif  // STOICHIOMETRY_DEVIATION_H
