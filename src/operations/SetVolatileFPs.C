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
//  Authors: D. Pizzocri, G. Zullo, E. Cappellari.                                  //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "SetFissionProducts.h"

static void caesium(SciantixArray<FissionProducts>& volatile_fp);

void SetVolatileFPs(SciantixArray<FissionProducts>& volatile_fp)
{
    caesium(volatile_fp);
}

static void caesium(SciantixArray<FissionProducts>& volatile_fp)
{
    FissionProducts volatile_;
    volatile_.setName("Cs");
    volatile_.setAtomicNumber(55);
    volatile_.setVanDerWaalsVolume(1.69e-28);  // 4/3 * PI * pow(343e-12, 3) from the van der waals radius
    volatile_.setDecayRate(0.0);               // stable
    volatile_.setMassNumber(133);
    volatile_.setChemicallyActive(1.0);
    volatile_.setPrecursorFactor(1.00);
    volatile_fp.push(volatile_);
}