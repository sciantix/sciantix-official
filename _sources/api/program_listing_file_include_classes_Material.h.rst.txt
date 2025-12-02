
.. _program_listing_file_include_classes_Material.h:

Program Listing for File Material.h
===================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_Material.h>` (``include/classes/Material.h``)

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
   
   #ifndef MATERIAL_H
   #define MATERIAL_H
   
   #include <string>
   
   class Material
   {
   public:
       Material() {}
   
       ~Material() {}
   
       void setName(std::string n)
       {
           name = n;
       }
   
       std::string getName()
       {
           return name;
       }
   
       void setRef(std::string n)
       {
           reference = n;
       }
   
       std::string getRef()
       {
           return reference;
       }
   
   protected:
       std::string name;
       std::string reference;
   };
   
   #endif // MATERIAL_H
