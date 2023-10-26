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

/// InputInterpolator
/// This function performs the interpolation of the input values,
/// calculating the current value of an input variable (e.g., temperature)
/// This algorithm is taken from Numerical recipes

#include "InputInterpolation.h"

double InputInterpolation(double x, std::vector<double> xx, std::vector<double> yy, unsigned short int n)
{
	double y;
	signed short int interval_low, interval_upp, interval_med, i;
	double a, b, c;

	n--;

	if (n == 0)
	{
		y = yy[0];
		return y;
	}

	// find the "right" interval
	interval_low = -1;
	interval_upp = n + 1;
	while (interval_upp - interval_low > 1)
	{
		interval_med = (interval_low + interval_upp) / 2;

		if (x < xx[interval_med])
			interval_upp = interval_med;
		else
			interval_low = interval_med;
	}

	if (x == xx[0])
		i = 0;
	else if (x == xx[n])
		i = n - 1;
	else
		i = interval_low;

	if (i == -1)
	{
		y = yy[0];
		return y;
	}
	else if (i == n)
	{
		y = yy[n];
		return y;
	}

	c = xx[i + 1] - xx[i];
	a = (xx[i + 1] * yy[i] - xx[i] * yy[i + 1]) / c;
	b = (yy[i + 1] - yy[i]) / c;

	y = a + b * x;
	return y;
}
