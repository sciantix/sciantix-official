
.. _program_listing_file_include_classes_Gas.h:

Program Listing for File Gas.h
==============================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_Gas.h>` (``include/classes/Gas.h``)

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
   
   #ifndef GAS_H
   #define GAS_H
   
   #include "Material.h"
   
   class Gas : virtual public Material
   {
   protected:
       int atomic_number;
       double mass_number;
       double van_der_waals_volume;
       double decay_rate;
       double precursor_factor;
   
   public:
       void setAtomicNumber(int y)
       {
           atomic_number = y;
       }
   
       int getAtomicNumber()
       {
           return atomic_number;
       }
   
       void setMassNumber(double y)
       {
           mass_number = y;
       }
   
       double getMassNumber()
       {
           return mass_number;
       }
   
       void setVanDerWaalsVolume(double y)
       {
           van_der_waals_volume = y;
       }
   
       double getVanDerWaalsVolume()
       {
           return van_der_waals_volume;
       }
   
       void setDecayRate(double l)
       {
           decay_rate = l;
       }
   
       double getDecayRate()
       {
           return decay_rate;
       }
   
       void setPrecursorFactor(double h)
       {
           precursor_factor = h;
       }
   
       double getPrecursorFactor()
       {
           return precursor_factor;
       }
   
       Gas() {}
   
       ~Gas() {}
   };
   
   #endif // GAS_H
