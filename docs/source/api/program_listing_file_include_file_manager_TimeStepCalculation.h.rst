
.. _program_listing_file_include_file_manager_TimeStepCalculation.h:

Program Listing for File TimeStepCalculation.h
==============================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_file_manager_TimeStepCalculation.h>` (``include/file_manager/TimeStepCalculation.h``)

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
   
   #ifndef TIME_STEP_CALCULATION_H
   #define TIME_STEP_CALCULATION_H
   
   #include <vector>
   
   double TimeStepCalculation(int                 Input_history_points,
                              double              Time_h,
                              std::vector<double> Time_input,
                              double              Number_of_time_steps_per_interval);
   
   #endif  // TIME_STEP_CALCULATION_H
