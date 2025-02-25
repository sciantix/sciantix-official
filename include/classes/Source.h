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
//  Version: 2.1                                                                    //
//  Year: 2024                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SOURCE_H
#define SOURCE_H
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <limits>
#include "MainVariables.h"




/**
 * 
 * The Source class stores the data related to a non-uniform source, which includes:
 * - The time at which the source is defined.
 * - The normalized domain (ND) which is a set of radial positions (scaled).
 * - The slopes of the source function (used for each region).
 * - The intercepts of the source function (used for each region).
 * 
 * Each part of the source is defined with a time and associated values for ND, slopes, and intercepts.
 *
 * @author A. Zayat
 */

class Source
{
public:
    double time;                          ///< Time at which we have this non-uniform source
    std::vector<double> NormalizedDomain; ///< NormalizedDomain = r/a [0, edge1/a, edge2/a, edge3/a, ..., 1]
    std::vector<double> Slopes;           ///< Source function slopes for each region
    std::vector<double> Intercepts;       ///< Source function intercepts for each region

    
    /**
     * @brief Constructor
     */
    Source() {}

    /**
     * @brief Destructor
     */
    ~Source() {}
};
#endif // SOURCE_H
