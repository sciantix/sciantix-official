
.. _program_listing_file_include_classes_Variable.h:

Program Listing for File Variable.h
===================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_Variable.h>` (``include/classes/Variable.h``)

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
   
   #ifndef VARIABLE_H
   #define VARIABLE_H
   
   #include <iostream>
   #include <string>
   #include <vector>
   
   class Variable
   {
     protected:
       std::string name;
   
     public:
       Variable()
       {
       }
       ~Variable()
       {
       }
   
       void setName(std::string n)
       {
           name = n;
       }
   
       std::string getName()
       {
           return name;
       }
   };
   
   #endif
