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
//  Version: 2.0                                                                    //
//  Year: 2022                                                                      //
//  Authors: D. Pizzocri, T. Barani.                                                //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include <vector>

/**
 * \brief Interpolates the value of an input variable at a given point using linear interpolation.
 * This function assumes that the input values (xx for positions and yy for values) are sorted and corresponds one-to-one.
 * The interpolation formula used is based on the straight-line equation between two points.
 *
 * @param x The point at which we want to interpolate.
 * @param xx Vector of input values representing the x-coordinates at which yy values are defined.
 * @param yy Vector of output values corresponding to each xx value.
 * @param n The total number of points in xx and yy.
 * @return The interpolated value at point x.
 */
double InputInterpolation(double x, std::vector<double> xx, std::vector<double> yy, unsigned short int n);
