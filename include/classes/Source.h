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
 * @brief Contains all information about the general source
 *
 * 
 * @author A. Zayat
 */
class Source //For now we'll stick with 3 regions and then extend it
{
public:
    std::vector<double> NormalizedDomain; // NormalizedDomain = r/a [0,edge1/a,edge2/a,edge3/a,...]
    std::vector<double> Slopes; // [A1, A2, A3,...]
    std::vector<double> Intercepts; // [B1, B2, B3,...]


    // /**
    //  * @brief Sets the normalized domain from [0,1]
    //  * @param domian The normalized domain vector
    //  */
    // void setNormalizedDomain(std::vector<double> domain);

    // /**
    //  * @brief Retrieves the domain [0,1]
    //  * @return The normalized domain of the source [-]
    //  */
    
    // std::vector<double> getNormalizedDomain();

    // /**
    //  * @brief Sets the slopes of the source in each region
    //  * @param slopes The slopes to set in each region [atm/m4]
    //  */
    // void setSlopes(std::vector<double> slopes);

    // /**
    //  * @brief Retrieves the slopes of the source in each region
    //  * @return The slopes of the source in each region [atm/m4]
    //  */
    // std::vector<double> getSlopes();

    // /**
    //  * @brief Sets the intercepts of the source in each region
    //  * @param intercepts the intercepts of the source in each region [at/m3]
    //  */
    // void setIntercepts(std::vector<double> intercepts);

    // /**
    //  * @brief Retrieves the intercepts of the source in each region
    //  * @return The intercepts of the source in each region [at/m3]
    //  */
    // std::vector<double> getIntercepts();

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
