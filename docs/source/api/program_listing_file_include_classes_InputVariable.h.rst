
.. _program_listing_file_include_classes_InputVariable.h:

Program Listing for File InputVariable.h
========================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_InputVariable.h>` (``include/classes/InputVariable.h``)

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
   
   #ifndef INPUT_VARIABLE_H
   #define INPUT_VARIABLE_H
   
   #include "Variable.h"
   
   class InputVariable : virtual public Variable
   {
   public:
   
       InputVariable(std::string name, double value)
       {
           this->name = name;
           this->value = value;
       }
   
       void setValue(double v)
       {
           value = v;
       }
   
       double getValue()
       {
           return value;
       }
   
       InputVariable() {}
   
       ~InputVariable() {}
   
   protected:
       double value;
   };
   
   #endif //INPUT_VARIABLE_H
