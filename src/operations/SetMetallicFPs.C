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
static void ruthenium(SciantixArray<FissionProducts>& metallic_fp);
static void rhodium(SciantixArray<FissionProducts>& metallic_fp);
static void technetium(SciantixArray<FissionProducts>& metallic_fp);
static void palladium(SciantixArray<FissionProducts>& metallic_fp);

void SetMetallicFPs(SciantixArray<FissionProducts>& metallic_fp)
{
    molybdenum(metallic_fp);
    ruthenium(metallic_fp);
    rhodium(metallic_fp);
    technetium(metallic_fp);
    palladium(metallic_fp);
}

static void molybdenum(SciantixArray<FissionProducts>& metallic_fp)
{
    FissionProducts metallic_;
    metallic_.setName("Mo");
    metallic_.setAtomicNumber(42);
    metallic_.setMassNumber(96); // Stable/long-lived 95, 96, 97, 98, 100
    metallic_.setDecayRate(0.0);
    metallic_.setChemicallyActive(1.00);
    metallic_.setPrecursorFactor(1.00);
    metallic_fp.push(metallic_);
}

static void ruthenium(SciantixArray<FissionProducts>& metallic_fp)
{
    FissionProducts metallic_;
    metallic_.setName("Ru");
    metallic_.setAtomicNumber(44);
    metallic_.setMassNumber(102); // Stable/long-lived 100, 101, 102, 104
    metallic_.setDecayRate(0.0);
    metallic_.setChemicallyActive(1.00);
    metallic_.setPrecursorFactor(1.00);
    metallic_fp.push(metallic_);
}

static void rhodium(SciantixArray<FissionProducts>& metallic_fp)
{
    FissionProducts metallic_;
    metallic_.setName("Rh");
    metallic_.setAtomicNumber(45);
    metallic_.setMassNumber(103); // Stable/long-lived 103
    metallic_.setDecayRate(0.0);
    metallic_.setChemicallyActive(1.00);
    metallic_.setPrecursorFactor(1.00);
    metallic_fp.push(metallic_);
}

static void technetium(SciantixArray<FissionProducts>& metallic_fp)
{
    FissionProducts metallic_;
    metallic_.setName("Tc");
    metallic_.setAtomicNumber(43);
    metallic_.setMassNumber(99); // Stable/long-lived 99
    metallic_.setDecayRate(0.0);
    metallic_.setChemicallyActive(1.00);
    metallic_.setPrecursorFactor(1.00);
    metallic_fp.push(metallic_);
}

static void palladium(SciantixArray<FissionProducts>& metallic_fp)
{
    FissionProducts metallic_;
    metallic_.setName("Pd");
    metallic_.setAtomicNumber(46);
    metallic_.setMassNumber(105); // Stable/long-lived 104, 105, 106, 107, 108
    metallic_.setDecayRate(0.0);
    metallic_.setChemicallyActive(1.00);
    metallic_.setPrecursorFactor(1.00);
    metallic_fp.push(metallic_);
}
