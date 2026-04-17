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
//  Version: under development                                                                   //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari                                   //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "SetFissionProducts.h"

static void molybdenum(SciantixArray<FissionProducts>& metallic_fp);

void SetMetallicFPs(SciantixArray<FissionProducts>& metallic_fp)
{
    molybdenum(metallic_fp);
}

static void molybdenum(SciantixArray<FissionProducts>& metallic_fp)
{
    FissionProducts metallic_;
    metallic_.setName("Mo");
    metallic_.setAtomicNumber(42);
    metallic_.setVanDerWaalsVolume(3.66e-29); // to be checked
    metallic_.setDecayRate(0.0); // stable
    metallic_.setMassNumber(96); // to be checked
    metallic_.setChemicallyActive(1.0);
    metallic_.setPrecursorFactor(1.00);
    metallic_fp.push(metallic_);
}
