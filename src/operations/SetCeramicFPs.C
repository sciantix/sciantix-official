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
//  Version: 2.5                                                                    //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari                                   //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "SetFissionProducts.h"

static void barium(SciantixArray<FissionProducts>& ceramic_fp);

void SetCeramicFPs(SciantixArray<FissionProducts>& ceramic_fp)
{
    barium(ceramic_fp);
}

static void barium(SciantixArray<FissionProducts>& ceramic_fp)
{
    FissionProducts ceramic_;
    ceramic_.setName("Ba");
    ceramic_.setAtomicNumber(56);
    ceramic_.setMassNumber(138);
    ceramic_.setDecayRate(0.0);
    ceramic_.setChemicallyActive(1.00);
    ceramic_.setPrecursorFactor(1.00);
    ceramic_fp.push(ceramic_);
}
