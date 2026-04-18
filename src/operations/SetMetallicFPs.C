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

static void metallicPlaceholder(SciantixArray<FissionProducts>& metallic_fp);

void SetMetallicFPs(SciantixArray<FissionProducts>& metallic_fp)
{
    metallicPlaceholder(metallic_fp);
}

static void metallicPlaceholder(SciantixArray<FissionProducts>& metallic_fp)
{
    FissionProducts metallic_;
    // Placeholder metallic FP used only to validate metallic class plumbing.
    metallic_.setName("M_placeholder");
    metallic_.setAtomicNumber(0);
    metallic_.setVanDerWaalsVolume(0.0);
    metallic_.setDecayRate(0.0);
    metallic_.setMassNumber(0.0);
    metallic_.setChemicallyActive(0.0);
    metallic_.setPrecursorFactor(1.00);
    metallic_fp.push(metallic_);
}
