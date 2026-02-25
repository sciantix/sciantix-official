
.. _program_listing_file_include_file_manager_Initialization.h:

Program Listing for File Initialization.h
=========================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_file_manager_Initialization.h>` (``include/file_manager/Initialization.h``)

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
   
   #ifndef INITIALIZATION_H
   #define INITIALIZATION_H
   
   #include <cmath>
   #include <iostream>
   #include <vector>
   
   void Initialization(double              Sciantix_history[],
                       double              Sciantix_variables[],
                       double              Sciantix_diffusion_modes[],
                       std::vector<double> Temperature_input,
                       std::vector<double> Fissionrate_input,
                       std::vector<double> Hydrostaticstress_input,
                       std::vector<double> Steampressure_input);
   
   #endif  // INITIALIZATION_H
