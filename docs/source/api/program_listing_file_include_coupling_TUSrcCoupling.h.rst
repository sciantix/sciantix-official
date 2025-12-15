
.. _program_listing_file_include_coupling_TUSrcCoupling.h:

Program Listing for File TUSrcCoupling.h
========================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_coupling_TUSrcCoupling.h>` (``include/coupling/TUSrcCoupling.h``)

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
   
   #ifndef TUSRCCOUPLING_H
   #define TUSRCCOUPLING_H
   
   #ifdef __cplusplus
   extern "C"
   {
   #endif
       void getSciantixOptions(int Sciantix_options[], double Sciantix_scaling_factors[]);
       void callSciantix(int Sciantix_options[], double Sciantix_history[],
                         double Sciantix_variables[], double Sciantix_scaling_factors[],
                         double Sciantix_diffusion_modes[]);
   
   #ifdef __cplusplus
   }
   #endif
   
   #endif  // TUSRCCOUPLING_H
