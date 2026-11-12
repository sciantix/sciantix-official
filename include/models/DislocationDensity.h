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
//  Version: 2.2.1                                                                  //
//  Year: 2026                                                                      //
//  Authors: G. Nicodemo, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#pragma once

#include <algorithm>
#include <cmath>

// Dislocation density rho_d(B, T) of irradiated UO2, used by the intragranular bubble model
// (iIntraGranularBubbleBehavior = 5): sites for bubble nucleation along dislocations and line trapping.
// Power-law/sigmoid fit of Veshchunov & Shestak, J. Nucl. Mater. 384 (2009) 12-18, Fig. 5 (MFPR calculations,
// modified parameter set, 900-1500 K), curves extracted from the PDF vector paths (context/dd_fit.py,
// VS2009_fig5_grid.csv), within -30%/+36%. The previous Fig. 4 fit used mislabelled digitized points.
// No calculations above 1500 K: T is held at 1500 K above it.
namespace dislocation_density
{
// SCIANTIX burnup per oxide mass (MWd/kgUO2) to heavy-metal mass (GWd/tHM)
constexpr double oxide_to_heavy_metal = (238.02891 + 2 * 15.9994) / 238.02891;
// constant reference density (Barani et al., 2020), m^-2
constexpr double initial_density = 4e13;
constexpr double fit_A = 3.437e13, fit_n = 0.8572, fit_inf = 0.03347, fit_Tc = 1019.0, fit_dT = 255.6, fit_Tmax = 1500.0;

// rho_d (m^-2) at heavy-metal burnup B (GWd/tHM) and temperature T (K)
inline double correlation(double B, double T)
{
    const double x       = (std::min(T, fit_Tmax) - fit_Tc) / fit_dT;
    const double sigmoid = x >= 0 ? std::exp(-x) / (1 + std::exp(-x)) : 1 / (1 + std::exp(x));
    return B > 0 ? fit_A * std::pow(B, fit_n) * (fit_inf + (1 - fit_inf) * sigmoid) : 0.;
}
}  // namespace dislocation_density
