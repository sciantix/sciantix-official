
.. _program_listing_file_include_models_GasDiffusion.h:

Program Listing for File GasDiffusion.h
=======================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_models_GasDiffusion.h>` (``include/models/GasDiffusion.h``)

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
   
   #ifndef GAS_DIFFUSION_H
   #define GAS_DIFFUSION_H
   
   #include "Simulation.h"
   
   void defineSpectralDiffusion1Equation(SciantixArray<System> &sciantix_system, SciantixArray<Model> &model, int n_modes);
   
   void defineSpectralDiffusion2Equations(SciantixArray<System> &sciantix_system, SciantixArray<Model> &model, int n_modes);
   
   void defineSpectralDiffusion3Equations(SciantixArray<System> &sciantix_system, SciantixArray<Model> &model, 
       SciantixArray<SciantixVariable> sciantix_variable, SciantixArray<SciantixVariable> physics_variable, int n_modes);
   
   void errorHandling(SciantixArray<InputVariable> input_variable);
   
   #endif // GAS_DIFFUSION_H
