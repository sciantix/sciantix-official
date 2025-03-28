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
#include <algorithm>
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
     * @brief Gives the spatial averaged value of the fission rate (Fdot)
     * @param GrainRadius 
     * @param source source shape
     */
    double Source_Volume_Average(double GrainRadius, Source source)
    {
        double NumberofRegions = source.Slopes.size(); // Obtains the number of regions
        double VA;
        std::vector<double> domain;

        std::transform(source.NormalizedDomain.begin(), source.NormalizedDomain.end(), domain.begin(),
                   [GrainRadius](double x) { return x * GrainRadius; });
        

        for (size_t j = 0; j < NumberofRegions; ++j)
        {
            VA += ((3/4)*source.Slopes[j]*pow(domain[j+1],4)/pow(GrainRadius,3)) + source.Intercepts[j]*pow(domain[j+1]/GrainRadius,3)
            - ((3/4)*source.Slopes[j]*pow(domain[j],4)/pow(GrainRadius,3)) + source.Intercepts[j]*pow(domain[j]/GrainRadius,3)
            ;
        }
        return VA;
    }

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
