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
    tellurium(gas);
    molybdenum(gas);
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
    gas_.setVanDerWaalsVolume(1.69e-28); // 4/3 * PI * pow(343e-12, 3) from the van der waals radius
    gas_.setDecayRate(0.0); //stable
    gas_.setMassNumber(133);
    gas_.setChemicallyActive(1.0);
    gas_.setPrecursorFactor(1.00);
    gas.push(gas_);
}

void iodine(SciantixArray<Gas> &gas) 
{
    Gas gas_;
    gas_.setName("I");
    gas_.setAtomicNumber(53);
    gas_.setVanDerWaalsVolume(3.25e-29); // 4/3 * PI * pow(198e-12, 3) from the van der waals radius
    gas_.setDecayRate(0.0); // stable
    gas_.setMassNumber(127);
    gas_.setChemicallyActive(1.0);
    gas_.setPrecursorFactor(1.00);
    gas.push(gas_);
}

void tellurium(SciantixArray<Gas> &gas) 
{
    Gas gas_;
    gas_.setName("Te");
    gas_.setAtomicNumber(52);
    gas_.setVanDerWaalsVolume(3.66e-29); // 4/3 * PI * pow(206e-12, 3) from the van der waals radius
    gas_.setDecayRate(0.0); // stable
    gas_.setMassNumber(128);
    gas_.setChemicallyActive(1.0);
    gas_.setPrecursorFactor(1.00);
    gas.push(gas_);
}
void molybdenum(SciantixArray<Gas> &gas) 
{
    Gas gas_;
    gas_.setName("Mo");
    gas_.setAtomicNumber(42);
    gas_.setVanDerWaalsVolume(3.66e-29); // to be modified 
    gas_.setDecayRate(0.0); // stable
    gas_.setMassNumber(96);// to be modified
    gas_.setChemicallyActive(1.0);
    gas_.setPrecursorFactor(1.00);
    gas.push(gas_);
}

