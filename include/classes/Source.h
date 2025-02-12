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
 * @brief Contains all information about the non uniform source: the normalized domain, the slopes and intercepts.
 * Each region: [r_i , r_i+1] has the corresponding source function S_i(r) = A_i * r + B_i.
 * The Slopes and Intercepts are of size N while the Normalized Domain is of size N+1.
 * example:  Normalized Domain = [0,rho1,rho2,1] | Slopes = [A1, A2, A3] | Intercepts = [B1, B2, B3].
 * @author A. Zayat
 */

 class Source
{
public:
    std::vector<double> NormalizedDomain; // NormalizedDomain = r/a [0,edge1/a,edge2/a,edge3/a,...,1]
    std::vector<double> Slopes; // [A1, A2, A3,...,Af]
    std::vector<double> Intercepts; // [B1, B2, B3,...,Bf]
    
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
