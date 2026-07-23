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

#ifndef SETSYSTEM_H
#define SETSYSTEM_H

#include "InputVariable.h"
#include "Matrix.h"
#include "SciantixArray.h"
#include "System.h"

/**
 * @brief Sets up the system properties in the simulation.
 *
 * @author G. Zullo
 * @author F. Bastien
 *
 */

System Xe_in_UO2(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System Xe_in_UO2HBS(SciantixArray<Matrix>&           matrices,
                    SciantixArray<FissionProducts>&  gas_fp,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors);

System Kr_in_UO2(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System He_in_UO2(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System Xe133_in_UO2(SciantixArray<Matrix>&           matrices,
                    SciantixArray<FissionProducts>&  gas_fp,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors);

System Kr85m_in_UO2(SciantixArray<Matrix>&           matrices,
                    SciantixArray<FissionProducts>&  gas_fp,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors);

System Xe_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System Kr_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System He_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  gas_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System Xe133_in_MOX(SciantixArray<Matrix>&           matrices,
                    SciantixArray<FissionProducts>&  gas_fp,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors);

System Kr85m_in_MOX(SciantixArray<Matrix>&           matrices,
                    SciantixArray<FissionProducts>&  gas_fp,
                    SciantixArray<InputVariable>&    input_variable,
                    SciantixArray<SciantixVariable>& sciantix_variable,
                    SciantixArray<SciantixVariable>& history_variable,
                    SciantixArray<InputVariable>&    scaling_factors);

System Cs_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  volatile_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System Mo_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  metallic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System Ba_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  ceramic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System Tc_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  metallic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System Rh_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  metallic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System Ru_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  metallic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

System Pd_in_MOX(SciantixArray<Matrix>&           matrices,
                 SciantixArray<FissionProducts>&  metallic_fp,
                 SciantixArray<InputVariable>&    input_variable,
                 SciantixArray<SciantixVariable>& sciantix_variable,
                 SciantixArray<SciantixVariable>& history_variable,
                 SciantixArray<InputVariable>&    scaling_factors);

#endif  // SETSYSTEM_H
