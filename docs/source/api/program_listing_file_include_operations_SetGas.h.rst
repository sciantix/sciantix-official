
.. _program_listing_file_include_operations_SetGas.h:

Program Listing for File SetGas.h
=================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_operations_SetGas.h>` (``include/operations/SetGas.h``)

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
   
   #ifndef SET_GAS_H
   #define SET_GAS_H
   
   #include <vector>
   #include "Gas.h"
   #include "SciantixArray.h"
   
   void xenon(SciantixArray<Gas> &gas);
   void krypton(SciantixArray<Gas> &gas);
   void helium(SciantixArray<Gas> &gas);
   
   #endif // SET_GAS_H
