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
//  Version: 2.2.1                                                                    //
//  Year: 2025                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef UNMODEL_H
#define UNMODEL_H

#include <algorithm>
#include <cmath>

// UN AD URANIUMNITRIDE
namespace un_model
{
    constexpr double rho_fab = 3.0e13;  // Rizk nominal constant rho_d = 3.0e13;
                                        // used/calibrated dynamic rho_d floor = 3.0e13.
    constexpr double rho_amp = 7.5e14;  // Rizk nominal dynamic amplitude = 0.0; used/calibrated = 7.5e14.
    constexpr double rho_scale = 0.10;  // Rizk nominal dynamic scale = 0.0; used/calibrated = 0.10.
    constexpr double rho_fc_percent =
        3.0;  // Rizk nominal burnup scale = not specified; used/calibrated = 3.0 FIMA percent.
    constexpr double rho_t_half =
        1550.0;  // Rizk nominal temperature half-point = not specified; used/calibrated = 1550 K.
    constexpr double rho_width =
        120.0;  // Rizk nominal temperature width = not specified; used/calibrated = 120 K.
    constexpr double rho_f_min = 0.08;  // Rizk nominal high-temperature floor factor = not specified; used/calibrated = 0.08.
    constexpr double rho_cap = 4.0e15;  // Rizk nominal dynamic cap = not specified; used/calibrated = 4.0e15.

    inline double dynamic_dislocation_density(const double temperature,
                                              const double burnup_percent,
                                              const int    option,
                                              const double constant_rho)
    {
        if (option == 0)
            return std::max(constant_rho, 1.0e10);

        const double burnup_part = 1.0 - std::exp(-std::max(burnup_percent, 0.0) / rho_fc_percent);
        const double ft =
            rho_f_min + (1.0 - rho_f_min) / (1.0 + std::exp((temperature - rho_t_half) / rho_width));
        const double rho = rho_fab + rho_scale * rho_amp * burnup_part * ft;
        return std::min(std::max(rho, 1.0e10), rho_cap);
    }
}  // namespace un_model
// END UN AD URANIUMNITRIDE

#endif  // UNMODEL_H
