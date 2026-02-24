
.. _program_listing_file_include_namespaces_ErrorMessages.h:

Program Listing for File ErrorMessages.h
========================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_namespaces_ErrorMessages.h>` (``include/namespaces/ErrorMessages.h``)

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
   
   #ifndef ERROR_MESSAGES_H
   #define ERROR_MESSAGES_H
   
   #include "MainVariables.h"
   #include <cstdlib>
   #include <fstream>
   #include <iomanip>
   #include <iostream>
   #include <string>
   
   namespace ErrorMessages
   {
       void MissingInputFile(const char* missing_file);
   
       void Switch(std::string routine, std::string variable_name, int variable);
   
       void writeErrorLog();
   };  // namespace ErrorMessages
   
   #endif
