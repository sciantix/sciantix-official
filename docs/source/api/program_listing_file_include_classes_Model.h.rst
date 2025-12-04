
.. _program_listing_file_include_classes_Model.h:

Program Listing for File Model.h
================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_Model.h>` (``include/classes/Model.h``)

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
   
   #ifndef MODEL_H
   #define MODEL_H
   
   #include <vector>
   #include <string>
   #include <iterator>
   #include <map>
   #include <string>
   #include "InputVariable.h"
   #include "Matrix.h"
   #include "Gas.h"
   #include "System.h"
   #include "Material.h"
   
   class Model: public Material
   {
   protected:
       std::string overview;
       std::vector<double> parameter;
   
   public:
       void setParameter(std::vector<double> p)
       {
           parameter = p;
       }
   
       std::vector<double> getParameter()
       {
           return parameter;
       }
   
       Model() {}
   
       ~Model() {}
   };
   
   #endif // MODEL_H
