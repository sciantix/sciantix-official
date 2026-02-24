
.. _program_listing_file_include_classes_Simulation.h:

Program Listing for File Simulation.h
=====================================

|exhale_lsh| :ref:`Return to documentation for file <file_include_classes_Simulation.h>` (``include/classes/Simulation.h``)

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
   
   #ifndef SIMULATION_H
   #define SIMULATION_H
   
   #include "Matrix.h"
   #include "Model.h"
   #include "SciantixArray.h"
   #include "SciantixVariable.h"
   #include "Solver.h"
   #include "System.h"
   #include <cmath>
   #include <vector>
   
   class Simulation
   {
     private:
       SciantixArray<SciantixVariable> sciantix_variable;
       SciantixArray<SciantixVariable> history_variable;
       SciantixArray<SciantixVariable> physics_variable;
   
       SciantixArray<Model>  model;
       SciantixArray<System> sciantix_system;
       SciantixArray<Matrix> matrices;
       SciantixArray<Gas>    gas;
   
       SciantixArray<InputVariable> input_variable;
       SciantixArray<InputVariable> scaling_factors;
   
       int                 n_modes;
       std::vector<double> modes_initial_conditions;
   
       Solver solver;
   
       static Simulation* instance;
   
       Simulation()
       {
           n_modes = 40;
           modes_initial_conditions.resize(720);
       }
   
     public:
       ~Simulation() {}
   
       static Simulation* getInstance();
   
       void setVariables(int Sciantix_options[], double Sciantix_history[],
                         double Sciantix_variables[], double Sciantix_scaling_factors[],
                         double Sciantix_diffusion_modes[]);
   
       void setGas();
       void setMatrix();
       void setSystem();
   
       void setGPVariables(int Sciantix_options[], double Sciantix_history[],
                           double Sciantix_variables[]);
   
       void initialize(int Sciantix_options[], double Sciantix_history[], double Sciantix_variables[],
                       double Sciantix_scaling_factors[], double Sciantix_diffusion_modes[]);
   
       void execute();
   
       void update(double Sciantix_variables[], double Sciantix_diffusion_modes[]);
   
       void output();
   
       void Burnup();
   
       void EffectiveBurnup();
   
       void GasProduction();
   
       void GasDecay();
   
       void GasRelease();
   
       void GasDiffusion();
   
       void GrainGrowth();
   
       void IntraGranularBubbleBehavior();
   
       void InterGranularBubbleBehavior();
   
       void GrainBoundarySweeping();
   
       void GrainBoundaryMicroCracking();
   
       void Densification();
   
       void GrainBoundaryVenting();
   
       double openPorosity(double fabrication_porosity);
   
       void HighBurnupStructureFormation();
   
       void HighBurnupStructurePorosity();
   
       void StoichiometryDeviation();
   
       void GapPartialPressure();
   
       void UO2Thermochemistry();
   
       void ChromiumSolubility();
   
       void Microstructure();
   
       double* getDiffusionModes(std::string gas_name)
       {
           if (gas_name == "Xe")
               return &modes_initial_conditions[0];
           else if (gas_name == "Kr")
               return &modes_initial_conditions[3 * 40];
           else if (gas_name == "He")
               return &modes_initial_conditions[6 * 40];
           else if (gas_name == "Xe133")
               return &modes_initial_conditions[9 * 40];
   
           else if (gas_name == "Kr85m")
               return &modes_initial_conditions[12 * 40];
   
           else if (gas_name == "Xe in HBS")
               return &modes_initial_conditions[15 * 40];
   
           else
           {
               std::cerr << "Error: Invalid gas name \"" << gas_name
                         << "\" in Simulation::getDiffusionModes." << std::endl;
               return nullptr;
           }
       }
   
       double* getDiffusionModesSolution(std::string gas_name)
       {
           if (gas_name == "Xe")
               return &modes_initial_conditions[1 * 40];
   
           else if (gas_name == "Kr")
               return &modes_initial_conditions[4 * 40];
   
           else if (gas_name == "He")
               return &modes_initial_conditions[7 * 40];
   
           else if (gas_name == "Xe133")
               return &modes_initial_conditions[10 * 40];
   
           else if (gas_name == "Kr85m")
               return &modes_initial_conditions[13 * 40];
   
           else if (gas_name == "Xe in HBS")
               return &modes_initial_conditions[16 * 40];
           else
           {
               std::cerr << "Error: Invalid gas name \"" << gas_name
                         << "\" in Simulation::getDiffusionModesSolution." << std::endl;
               return nullptr;
           }
       }
   
       double* getDiffusionModesBubbles(std::string gas_name)
       {
           if (gas_name == "Xe")
               return &modes_initial_conditions[2 * 40];
   
           else if (gas_name == "Kr")
               return &modes_initial_conditions[5 * 40];
   
           else if (gas_name == "He")
               return &modes_initial_conditions[8 * 40];
   
           else if (gas_name == "Xe133")
               return &modes_initial_conditions[11 * 40];
   
           else if (gas_name == "Kr85m")
               return &modes_initial_conditions[14 * 40];
   
           else
           {
               std::cerr << "Error: Invalid gas name \"" << gas_name
                         << "\" in Simulation::getDiffusionModesBubbles." << std::endl;
               return nullptr;
           }
       }
   };
   
   #endif
