
.. _program_listing_file_include_Sciantix.h:

Program Listing for File Sciantix.h
===================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_Sciantix.h>` (``include/Sciantix.h``)

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
   
   #ifndef SCIANTIX_H
   #define SCIANTIX_H
   
   #include "Simulation.h"
   
   void Sciantix(int    Sciantix_options[],
                 double Sciantix_history[],
                 double Sciantix_variables[],
                 double Sciantix_scaling_factors[],
                 double Sciantix_diffusion_modes[]);
   
   #endif  // SCIANTIX_H
