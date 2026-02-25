
.. _program_listing_file_include_classes_SciantixVariable.h:

Program Listing for File SciantixVariable.h
===========================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_SciantixVariable.h>` (``include/classes/SciantixVariable.h``)

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
   
   #ifndef SCIANTIX_VARIABLE_H
   #define SCIANTIX_VARIABLE_H
   
   #include "Variable.h"
   
   class SciantixVariable : virtual public Variable
   {
     protected:
       std::string uom;
       double      final_value;
       double      initial_value;
       bool        to_output;
   
     public:
       SciantixVariable(std::string name, std::string uom, double initial_value, double final_value, bool output)
       {
           this->name          = name;
           this->uom           = uom;
           this->initial_value = initial_value;
           this->final_value   = final_value;
           this->to_output     = output;
       }
   
       void rescaleInitialValue(const double factor);
   
       void rescaleFinalValue(const double factor);
   
       void addValue(const double v);
   
       void setUOM(std::string s);
   
       std::string getUOM();
   
       void setConstant();
   
       void resetValue();
   
       void setFinalValue(double FinalValue);
   
       void setInitialValue(double InitialValue);
   
       double getFinalValue();
   
       double getInitialValue();
   
       double getIncrement();
   
       void setOutput(bool io);
   
       bool getOutput();
   
       SciantixVariable()
       {
       }
   
       ~SciantixVariable()
       {
       }
   };
   
   #endif
