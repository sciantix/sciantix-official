
.. _program_listing_file_include_file_manager_InputReading.h:

Program Listing for File InputReading.h
=======================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_file_manager_InputReading.h>` (``include/file_manager/InputReading.h``)

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
   
   #ifndef INPUT_READING_H
   #define INPUT_READING_H
   
   #include "ErrorMessages.h"
   #include <string>
   #include <sstream>
   #include <vector>
   #include <numeric>
   
   void InputReading(
       int Sciantix_options[],
       double Sciantix_variables[],
       double Sciantix_scaling_factors[],
       int &Input_history_points,
       std::vector<double> &Time_input,
       std::vector<double> &Temperature_input,
       std::vector<double> &Fissionrate_input,
       std::vector<double> &Hydrostaticstress_input,
       std::vector<double> &Steampressure_input,
       double &Time_end_h,
       double &Time_end_s
       );
   
   unsigned short int ReadOneSetting(std::string variable_name, std::ifstream& input_file, std::ofstream& output_file);
   double ReadOneParameter(std::string variable_name, std::ifstream& input_file, std::ofstream& output_file);
   
   #endif // INPUT_READING_H
