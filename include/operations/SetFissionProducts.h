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
//  Version: under development                                                      //
//  Year: 2026                                                                      //
//  Authors: D. Pizzocri, G. Zullo, E.Cappellari                                    //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SET_FISSION_PRODUCTS_H
#define SET_FISSION_PRODUCTS_H

#include "FissionProducts.h"
#include "SciantixArray.h"

void SetGasFPs(SciantixArray<FissionProducts>& gas_fp);
void SetVolatileFPs(SciantixArray<FissionProducts>& volatile_fp);
void SetMetallicFPs(SciantixArray<FissionProducts>& metallic_fp);

#endif  // SET_FISSION_PRODUCTS_H
