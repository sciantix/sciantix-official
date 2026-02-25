
.. _program_listing_file_include_operations_SetVariablesFunctions.h:

Program Listing for File SetVariablesFunctions.h
================================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_operations_SetVariablesFunctions.h>` (``include/operations/SetVariablesFunctions.h``)

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
   
   #include "SciantixVariable.h"
   #include <vector>
   
   std::vector<std::string> getInputVariableNames();
   
   std::vector<SciantixVariable>
   initializeHistoryVariable(double Sciantix_history[], double Sciantix_scaling_factors[], bool toOutput);
   
   std::vector<SciantixVariable> initializeSciantixVariable(double Sciantix_variables[],
                                                            bool   toOutputRadioactiveFG,
                                                            bool   toOutputVenting,
                                                            bool   toOutputHelium,
                                                            bool   toOutputCracking,
                                                            bool   toOutputGrainBoundary,
                                                            bool   toOutputHighBurnupStructure,
                                                            bool   toOutputStoichiometryDeviation,
                                                            bool   toOutputChromiumContent);
   
   std::vector<std::string> getScalingFactorsNames();
