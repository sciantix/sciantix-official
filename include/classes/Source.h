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
    std::array<double,4> NormalizedDomain; // NormalizedDomain = r/a [0,edge1/a,edge2/a,edge3/a]
    std::array<double,3> Slopes; // [A1, A2, A3]
    std::array<double,3> Intercepts; // [B1, B2, B3]


    /**
     * @brief Sets the normalized domain from [0,1]
     * @param rho The normalized domain is set [-]
     */
    void setNormalizedDomain(std::array<double,4> rho)
    {
        NormalizedDomain = rho;
    }

    /**
     * @brief Retrieves the domain [0,1]
     * @return The normalized domain of the source [-]
     */
    double getNormalizedDomain()
    {
        double NormalizedDomain;    
    }

    /**
     * @brief Sets the slopes of the source in each region
     * @param A The slopes to set in each region [atm/m4]
     */
    void setLatticeParameter(std::array<double,3> A)
    {
        Slopes = A;
    }

    /**
     * @brief Retrieves the slopes of the source in each region
     * @return The slopes of the source in each region [atm/m4]
     */
    double getSlopes()
    {
        double Slopes;
    }

    /**
     * @brief Sets the intercepts of the source in each region
     * @param B The intercepts of the source in each region [at/m3]
     */
    void setIntercepts(std::array<double,3> B)
    {
        Intercepts = B;
    }

    /**
     * @brief Retrieves the intercepts of the source in each region
     * @return The intercepts of the source in each region [at/m3]
     */
    double getIntercepts()
    {
        double Intercepts;
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
