//////////////////////////////////////////////////////////////////////////////////////
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
//////////////////////////////////////////////////////////////////////////////////////

#include "SetGas.h"
#include "Simulation.h"

void Simulation::setGas()
{
    xenon(gas);
    krypton(gas);
    helium(gas);
    caesium(gas);
    iodine(gas);
    //caesiumiodine(gas);
}

void xenon(SciantixArray<Gas> &gas)
{
    Gas gas_;    
    gas_.setName("Xe");
    gas_.setAtomicNumber(54);
    gas_.setMassNumber(135);
    gas_.setVanDerWaalsVolume(8.48e-29);
    gas_.setDecayRate(0.0);
    gas_.setChemicallyActive(0.0);
    gas_.setPrecursorFactor(1.00);
    gas.push(gas_);

    gas_.setName("Xe133");
    gas_.setMassNumber(133);
    gas_.setDecayRate(1.53e-6);
    gas_.setChemicallyActive(0.0);
    gas_.setPrecursorFactor(1.25);
    gas.push(gas_);
}

void krypton(SciantixArray<Gas> &gas)
{
    Gas gas_;
    gas_.setName("Kr");
    gas_.setAtomicNumber(36);
    gas_.setVanDerWaalsVolume(6.61e-29);
    gas_.setDecayRate(0.0);
    gas_.setChemicallyActive(0.0);
    gas_.setPrecursorFactor(1.00);
    gas.push(gas_);

    gas_.setName("Kr85m");
    gas_.setMassNumber(85);
    gas_.setDecayRate(4.3e-5);
    gas_.setChemicallyActive(0.0);
    gas_.setPrecursorFactor(1.31);
    gas.push(gas_);
}

void helium(SciantixArray<Gas> &gas)
{
    Gas gas_;
    gas_.setName("He");
    gas_.setAtomicNumber(2);
    gas_.setVanDerWaalsVolume(9.97e-30);
    gas_.setDecayRate(0.0);
    gas_.setChemicallyActive(0.0);
    gas_.setPrecursorFactor(1.00);
    gas.push(gas_);
}

// Data from JEFF-3.3 if not specified, to be verified the volumes

void caesium(SciantixArray<Gas> &gas)
{
    Gas gas_;
    gas_.setName("Cs");
    gas_.setAtomicNumber(55);
    gas_.setVanDerWaalsVolume(1.69e-28);
    gas_.setDecayRate(0.0); //stable
    gas_.setChemicallyActive(1.0);
    gas_.setPrecursorFactor(1.00);
    gas.push(gas_);
    //system_.setYield(0.066534);
}

void iodine(SciantixArray<Gas> &gas) 
{
    Gas gas_;
    gas_.setName("I");
    gas_.setMassNumber(127);
    gas_.setAtomicNumber(53);
    gas_.setVanDerWaalsVolume(9.33e-29);
    gas_.setDecayRate(0.0); // stable
    gas_.setChemicallyActive(1.0);
    gas_.setPrecursorFactor(1.00);
    gas.push(gas_);
    //system_.setYield(0.001241);
}

// void caesiumiodine(SciantixArray<Gas> &gas) 
// {
//     Gas gas_;
//     gas_.setName("CsI");
//     gas_.setVanDerWaalsVolume(9.33e-29 + 1.69e-28);
//     gas_.setDecayRate(0.0); // stable
//     gas_.setChemicallyActive(1.0);
//     gas_.setPrecursorFactor(1.00);
//     gas.push(gas_);
//     //system_.setYield(0.001241);
// }

// // Data from JEFF-3.3 if not specified, to be verified the volumes

// void caesium(SciantixArray<Gas> &gas) //Cs 134, 137
// {
//     Gas gas_;
//     gas_.setName("Cs133");
//     gas_.setAtomicNumber(55);
//     gas_.setVanDerWaalsVolume(1.69e-28);
//     gas_.setDecayRate(0); //stable
//     gas_.setPrecursorFactor(1.00);
//     gas.push(gas_);
//     //system_.setYield(0.066534);

//     Gas gas_;
//     gas_.setName("Cs135");
//     gas_.setAtomicNumber(55);
//     gas_.setVanDerWaalsVolume(1.69e-28);
//     gas_.setDecayRate(0); //2,3e6 years
//     gas_.setPrecursorFactor(1.00); 
//     gas.push(gas_);
//     //system_.setYield(0.063135);

//     Gas gas_;
//     gas_.setName("Cs137");
//     gas_.setAtomicNumber(55);
//     gas_.setVanDerWaalsVolume(1.69e-28);
//     gas_.setDecayRate(7.31e-10); //30,05001 years 
//     gas_.setPrecursorFactor(1.00);
//     gas.push(gas_);
//     //system_.setYield(0.060897);
// }

// void iodine(SciantixArray<Gas> &gas) // I129, 131, 133,135
// {
//     Gas gas_;
//     gas_.setName("I127");
//     gas_.setMassNumber(127);
//     gas_.setAtomicNumber(53);
//     gas_.setVanDerWaalsVolume(9.33e-29);
//     gas_.setDecayRate(0); // stable
//     gas_.setPrecursorFactor(1.00);
//     gas.push(gas_);
//     //system_.setYield(0.001241);
    
//     Gas gas_;
//     gas_.setName("I129");
//     gas_.setMassNumber(129);
//     gas_.setAtomicNumber(53);
//     gas_.setVanDerWaalsVolume(9.33e-29);
//     gas_.setDecayRate(0); // 1,61e7 years  100% branching
//     gas_.setPrecursorFactor(1.00);
//     gas.push(gas_);
//     //system_.setYield(0.008137);

//     Gas gas_;
//     gas_.setName("I131");
//     gas_.setMassNumber(131);
//     gas_.setAtomicNumber(53);
//     gas_.setVanDerWaalsVolume(9.33e-29);
//     gas_.setDecayRate(9.98e-7); // J.A. Turnbull, C.E. Beyer, in: Background and Derivation of ANS-5.4 Standard Fission Product Release Model, United States Nuclear Regulatory Commision, 2010, p. 11. URL, http://www.nrc.gov/reading-rm.html.
//     gas_.setPrecursorFactor(1.00);
//     gas.push(gas_);
//     //system_.setYield(0.02921);
// }