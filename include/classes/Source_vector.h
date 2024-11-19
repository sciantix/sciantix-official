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
#include <string>
#include <vector>
#include <iostream>
#include <cmath>
#include <algorithm>  // For std::copy

/**
 * @class Source
 * @brief Contains all information about the general source
 *
 * 
 * @author A. Zayat
 */
class Source
{
public:
    std::vector<double> NormalizedDomain; // NormalizedDomain = r/a [edge1/a,edge2/a,edge3/a,...]
    std::vector<double> Slopes; // [A1, A2, A3, ...]
    std::vector<double> Intercepts; // [B1, B2, B3,...]


    /**
     * @brief Sets the normalized domain.
     * @param rho The normalized domain is set [-].
     */
    void setNormalizedDomain(std::vector<double> domain)
    {
        NormalizedDomain = domain;
    }

    /**
     * @brief Retrieves the domain.
     * @return The normalized domain of the source [-].
     */
    double getNormalizedDomain()
    {
        double NormalizedDomain;    
    }

    /**
     * @brief Sets the slopes of the source in each region.
     * @param A The slopes to set in each region [atm/m4].
     */
    void setLatticeParameter(std::vector<double> A)
    {
        Slopes = A;
    }

    /**
     * @brief Retrieves the slopes of the source in each region.
     * @return The slopes of the source in each region [atm/m4].
     */
    double getSlopes()
    {
        std::vector<double> Slopes;
    }

    /**
     * @brief Sets the intercepts of the source in each region.
     * @param B The intercepts of the source in each region [at/m3]
     */
    void setIntercepts(std::vector<double> B)
    {
        Intercepts = B;
    }

    /**
     * @brief Retrieves the intercepts of the source in each region
     * @return The intercepts of the source in each region [at/m3]
     */
    double getIntercepts()
    {
        std::vector<double> Intercepts;
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
