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



/**
 * @class Source
 * @brief Contains all information about the general source, the normalized domain, the slopes and intercepts
 * Each region: [r_i , r_i+1] has the corresponding source function S_i(r) = A_i * r + B_i 
 * 
 * @author A. Zayat
 */
class Source
{
public:
    std::vector<double> NormalizedDomain; // NormalizedDomain = r/a [0,edge1/a,edge2/a,edge3/a,...,1]
    std::vector<double> Slopes; // [A1, A2, A3,...,Af]
    std::vector<double> Intercepts; // [B1, B2, B3,...,Bf]
    
    // As a trial to see  if the code will work, we'll fix the ND, Slopes and Intercepts and then figure out
    // a way to make it more general
    std::vector<double> domain = {0,0.2,0.5,1};
    std::vector<double> slopes = {1e10, 0, -1e10};
    std::vector<double> intercepts = {1e19, (1e19+1e4), (1e19+25000)};

    /**
     * @brief Sets the normalized domain from [0,1]
     * @param domian The normalized domain vector
     */
    void setNormalizedDomain()
    {
        NormalizedDomain = domain;
    }

    /**
     * @brief Retrieves the domain [0,1]
     * @return The normalized domain of the source [-]
     */
    
    std::vector<double> getNormalizedDomain()
    {
        return NormalizedDomain;
    }

    /**
     * @brief Sets the slopes of the source in each region
     * @param slopes The slopes to set in each region [atm/m4]
     */
    void setSlopes()
    {
        Slopes = slopes;
    }

    /**
     * @brief Retrieves the slopes of the source in each region
     * @return The slopes of the source in each region [atm/m4]
     */
    std::vector<double> getSlopes()
    {
        return Slopes;
    }

    /**
     * @brief Sets the intercepts of the source in each region
     * @param intercepts the intercepts of the source in each region [at/m3]
     */
    void setIntercepts()
    {
        Intercepts = intercepts;
    }


    /**
     * @brief Retrieves the intercepts of the source in each region
     * @return The intercepts of the source in each region [at/m3]
     */
    std::vector<double> getIntercepts()
    {
        return Intercepts;
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
