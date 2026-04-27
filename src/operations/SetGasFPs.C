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
//  Version: under development                                                       //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari                                   //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "SetFissionProducts.h"

static void xenon(SciantixArray<FissionProducts>& gas_fp);
static void krypton(SciantixArray<FissionProducts>& gas_fp);
static void helium(SciantixArray<FissionProducts>& gas_fp);

void SetGasFPs(SciantixArray<FissionProducts>& gas_fp)
{
    xenon(gas_fp);
    krypton(gas_fp);
    helium(gas_fp);
}

static void xenon(SciantixArray<FissionProducts>& gas_fp)
{
    FissionProducts gas_;
    gas_.setName("Xe");
    gas_.setAtomicNumber(54);
    gas_.setMassNumber(135);
    gas_.setVanDerWaalsVolume(8.48e-29);
    gas_.setDecayRate(0.0);
    gas_.setChemicallyActive(0.0);
    gas_.setPrecursorFactor(1.00);
    gas_fp.push(gas_);

    gas_.setName("Xe133");
    gas_.setMassNumber(133);
    gas_.setDecayRate(1.53e-6);
    gas_.setChemicallyActive(0.0);
    gas_.setPrecursorFactor(1.25);
    gas_fp.push(gas_);
}

static void krypton(SciantixArray<FissionProducts>& gas_fp)
{
    FissionProducts gas_;
    gas_.setName("Kr");
    gas_.setAtomicNumber(36);
    gas_.setVanDerWaalsVolume(6.61e-29);
    gas_.setDecayRate(0.0);
    gas_.setChemicallyActive(0.0);
    gas_.setPrecursorFactor(1.00);
    gas_fp.push(gas_);

    gas_.setName("Kr85m");
    gas_.setMassNumber(85);
    gas_.setDecayRate(4.3e-5);
    gas_.setChemicallyActive(0.0);
    gas_.setPrecursorFactor(1.31);
    gas_fp.push(gas_);
}

static void helium(SciantixArray<FissionProducts>& gas_fp)
{
    FissionProducts gas_;
    gas_.setName("He");
    gas_.setAtomicNumber(2);
    gas_.setVanDerWaalsVolume(9.97e-30);
    gas_.setDecayRate(0.0);
    gas_.setChemicallyActive(0.0);
    gas_.setPrecursorFactor(1.00);
    gas_fp.push(gas_);
}
