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
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SOLVER_h
#define SOLVER_h

#include <vector>
#include <string>
#include <cmath>
#include "InputVariable.h"
#include "ConstantNumbers.h"

/// Derived class for the SCIANTIX solvers. They are communicated with models within the Simulation class.

class Solver : public InputVariable
{
public:

	double Integrator(double initial_value, double parameter, double increment)
	{
		/// Solver for the ODE [y' = + S]
		// parameter = source term
		return initial_value + parameter * increment;
	}

	double LimitedGrowth(double initial_value, std::vector<double> parameter, double increment)
	{
		/// Solver for the ODE [y' = k / y + S]
		// parameter[0] = growth rate
		// parameter[1] = source term
		return 0.5 * ((initial_value + parameter[1] * increment) + sqrt(pow(initial_value + parameter[1] * increment, 2) + 4.0 * parameter[0] * increment));
	}

	//double Decay(double initial_condition, std::vector<double> parameter, double increment)
	double Decay(double initial_condition, double decay_rate, double source_term, double increment)
	{
		/// Solver for the ODE [y' = - L y + S]
		/// 1nd parameter = decay rate
		/// 2st parameter = source
		return (initial_condition + source_term * increment) / (1.0 + decay_rate * increment);
	}

	double BinaryInteraction(double initial_condition, double interaction_coefficient, double increment)
		/// Solver for the ODE [y' = -k y**2]
	{
		/*
		if(increment == 0.0)
			return initial_condition;
		else
			return initial_condition = 0.25 * (sqrt(1 + 8 * initial_condition * increment) - 1) / increment;
		*/
		return initial_condition / (1.0 + interaction_coefficient * initial_condition * increment);
	}

	double SpectralDiffusion(double* initial_condition, std::vector<double> parameter, double increment)
	{
		/// Solver for the spatially averaged solution of the PDE [dy/dt = D div grad y + S - L y]
		/// We apply a spectral approach in space, projecting the equation on the eigenfunctions of the laplacian operator.
		/// We use the first order backward Euler solver in time.
		/// The number of terms in the expansion, N, is fixed a priori.

		// Parameters
		// 0) N_modes
		// 1) D
		// 2) r
		// 3) production
		// 4) loss rate

		unsigned short int n(0);
		unsigned short int np1(1);

		double diffusion_rate_coeff(0.0);
		double diffusion_rate(0.0);
		double source_rate_coeff(0.0);
		double source_rate(0.0);
		double projection_coeff(0.0);
		double solution(0.0);
		const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

		diffusion_rate_coeff = pow(pi, 2) * parameter.at(1) / pow(parameter.at(2), 2);
		projection_coeff = -2.0 * sqrt(2.0 / pi);
		source_rate_coeff = projection_coeff * parameter.at(3);

		for (n = 0; n < parameter.at(0); n++)
		{
			np1 = n + 1;
			const double n_coeff = pow(-1.0, np1) / np1;

			diffusion_rate = diffusion_rate_coeff * pow(np1, 2) + parameter.at(4);
			source_rate = source_rate_coeff * n_coeff;

			initial_condition[n] = Solver::Decay(initial_condition[n], diffusion_rate, source_rate, increment);

			solution += projection_coeff * n_coeff * initial_condition[n] / ((4. / 3.) * pi);
		}

		return solution;
	}

	double dotProduct1D(std::vector<double> u, double v[], int n)
	{
		/// Function to compute the dot product between two arrays (v and u) of size n
		double result = 0.0;
		for (int i = 0; i < n; ++i)
			result += u[i] * v[i];
		return result;
	}

	void dotProduct2D(double A[], double v[], int n_rows, const int n_col, double result[])
	{
		/// Function to compute the dot product between a matrix and an array
		std::vector<double> a(n_col);
		for (int i = 0; i < n_rows; ++i)
		{
			// create a vector from the i-th row of A
			for (int j = 0; j < n_col; ++j)
			{
				a[j] = A[i * n_col + j];
			}
			result[i] = dotProduct1D(a, v, n_col);
		}
	}

	void SpectralDiffusionNonEquilibrium(double& gas_solution, double& gas_bubble, double* initial_condition_gas_solution, double* initial_condition_gas_bubble, std::vector<double> parameter, double increment)
	{
		/// SpectralDiffusionNonEquilibrium
		/// Solver for the spatially averaged solution of the systems of PDEs:
		/// |dy1/dt = D div grad y1 - gy1 + by2 + S1 - Ly1
		/// |dy2/dt = Db div grad y2 + gy1 - by2 + S2 - Ly2

		///           ((D div grad - g - L)                       b )
		///           (         g             (Db div grad -b - L)   )

		/// |dy1/dt = D  div grad y1 - gy1 + by2 + S1 - Ly1
		/// |dy2/dt = Db div grad y2 + gy1 - by2 + S2 - Ly2

		///           ((D div grad - g - L)                       b )
		///           (         g             (Db div grad -b - L)   )

		/// We apply a spectral approach in space, projecting the equation on the eigenfunctions of the laplacian operator.
		/// We use the first order backward Euler solver in time.
		/// The number of terms in the expansion, N, is fixed a priori.
		// ------------------------------------
		// parameter --> number of modes
		//               diffusion_coefficient (D)
		//               resolution_rate (b)
		//               trapping_rate (g)
		//               decay rate (L)
		//               domain_radius 
		//               source_term 
		//               source_term_bubbles

		unsigned short int n(0);
		unsigned short int np1(1);

		double bubble_diffusion_rate(0.0);
		double diffusion_rate_coeff(0.0);
		double bubble_diffusion_rate_coeff(0.0);
		double diffusion_rate(0.0);
		double source_rate_coeff_solution(0.0);
		double source_rate_coeff_bubbles(0.0);
		double source_rate_solution(0.0);
		double source_rate_bubble(0.0);
		double projection_coeff(0.0);
		double gas_solution_solution(0.);
		double gas_bubble_solution(0.);
		double coeff_matrix[4];
		double initial_conditions[2];
		const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

		diffusion_rate_coeff = pow(pi, 2) * parameter.at(1) / pow(parameter.at(5), 2); // pi^2 * D / a^2
		bubble_diffusion_rate_coeff =  pow(pi, 2) * parameter.at(8) / pow(parameter.at(5), 2); // pi^2 * Db / a^2
		projection_coeff = -2.0 * sqrt(2.0 / pi);
		source_rate_coeff_solution = projection_coeff * parameter.at(6); // - 2 sqrt(2/pi) * S1
		source_rate_coeff_bubbles = projection_coeff * parameter.at(7); // - 2 sqrt(2/pi) * S2

		for (n = 0; n < parameter.at(0); n++)
		{
			np1 = n + 1;
			const double n_coeff = pow(-1.0, np1) / np1;

			diffusion_rate = diffusion_rate_coeff * pow(np1, 2); // pi^2 * D * n^2 / a^2
			bubble_diffusion_rate = bubble_diffusion_rate_coeff * pow(np1, 2); // pi^2 * Db * n^2 / a^2
			source_rate_solution = source_rate_coeff_solution * n_coeff; // - 2 sqrt(2/pi) * S * (-1)^n/n
			source_rate_bubble = source_rate_coeff_bubbles * n_coeff;

			coeff_matrix[0] = 1.0 + (diffusion_rate + parameter.at(3) + parameter.at(4)) * increment; // 1 + (pi^2 * D * n^2 / a^2 + g + L) dt
			coeff_matrix[1] = -parameter.at(2) * increment; // - b
			coeff_matrix[2] = -parameter.at(3) * increment; // - g
			coeff_matrix[3] = 1.0 + (bubble_diffusion_rate + parameter.at(2) + parameter.at(4)) * increment; // 1 + (pi^2 * Db * n^2 / a^2 + b + L) dt
			initial_conditions[0] = initial_condition_gas_solution[n] + source_rate_solution * increment;
			initial_conditions[1] = initial_condition_gas_bubble[n] + source_rate_bubble * increment;

			Solver::Laplace2x2(coeff_matrix, initial_conditions);

			initial_condition_gas_solution[n] = initial_conditions[0];
			initial_condition_gas_bubble[n] = initial_conditions[1];

			gas_solution_solution += projection_coeff * n_coeff * initial_conditions[0] / ((4. / 3.) * pi);
			gas_bubble_solution += projection_coeff * n_coeff * initial_conditions[1] / ((4. / 3.) * pi);
		}
		gas_solution = gas_solution_solution;
		gas_bubble = gas_bubble_solution;
	}

	void Laplace2x2(double A[], double b[])
	{
		/// Laplace 2x2
		/// The function solve a system of two linear equations according to Cramer method.
		double detX(0.0), detY(0.0);
		double detA = A[0] * A[3] - A[1] * A[2];

		if (detA != 0.0)
		{
			detX = b[0] * A[3] - b[1] * A[1];
			detY = b[1] * A[0] - b[0] * A[2];
			b[0] = detX / detA;
			b[1] = detY / detA;
		}
	}


  double QuarticEquation(std::vector<double> parameter)
  {
		/**
		 * @brief Solver for the quartic equation ax^4 + bx^3 +cx^2 +dx + e = 0
		 * with the iterative Newton's method.
		 * 
		 * @param parameter.at(0) initial conditions
		 * @param parameter.at(1) coefficient of x^4
		 * @param parameter.at(2) coefficient of x^3
		 * @param parameter.at(3) coefficient of x^2
		 * @param parameter.at(4) coefficient of x^1
		 * @param parameter.at(5) coefficient of x^0
		 * @return x1 solution
		 */

    double function(0.0);
    double derivative(0.0);
    double y1(0.0);
    unsigned short int iter(0);
    const double tol(1.0e-3);
    const unsigned short int max_iter(5);

		double y0 = parameter.at(0);
		double a = parameter.at(1);
		double b = parameter.at(2);
		double c = parameter.at(3);
		double d = parameter.at(4);
		double e = parameter.at(5);

    while (iter < max_iter)
    {
      function = a*pow(y0, 4) + b*pow(y0, 3) + c*pow(y0, 2) + d*y0 + e;
      derivative = 4.0*a*pow(y0, 3) + 3.0*b*pow(y0, 2) + 2.0*c*y0 + d;

      y1 = y0 - function/derivative;
      y0 = y1;

      if(function < tol) return y1;

      iter++;
    }
    return y1;
  }

	void modeInitialization(int n_modes, double mode_initial_condition, double* diffusion_modes)
	{
		const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

		// projection on diffusion modes of the initial conditions
		double initial_condition(0.0);
		double projection_remainder(0.0);
		double reconstructed_solution(0.0);
		int iteration(0), iteration_max(20), n(0), np1(1);
		double projection_coeff(0.0);
		projection_coeff = -sqrt(8.0 / pi);

		initial_condition = mode_initial_condition;

		projection_remainder = initial_condition;
		for (iteration = 0; iteration < iteration_max; ++iteration)
		{
			reconstructed_solution = 0.0;
			for (n = 0; n < n_modes; ++n)
			{
				np1 = n + 1;
				const double n_coeff = pow(-1.0, np1) / np1;
				diffusion_modes[n] += projection_coeff * n_coeff * projection_remainder;
				reconstructed_solution += projection_coeff * n_coeff * diffusion_modes[n] * 3.0 / (4.0 * pi);
			}
			projection_remainder = initial_condition - reconstructed_solution;
		}
	}

  double NewtonBlackburn(std::vector<double> parameter)
  {
		/**
		 * @brief Solver for the non-linear equation (Blackburn's thermochemical urania model) log(PO2(x)) = 2.0*log(x*(x+2.0)/(1.0-x)) + 108.0*pow(x,2.0) - 32700.0/T + 9.92
		 * with the iterative Newton's method.
		 * 
		 */

    double fun(0.0);
    double deriv(0.0);
    double x1(0.0);
    unsigned short int iter(0);
    const double tol(1.0e-3);
    const unsigned short int max_iter(50);
    
	  double a = parameter.at(0);
	  double b = parameter.at(1);
	  double c = log(parameter.at(2));

    if(parameter.at(2)==0)
			std::cout << "Warning: check NewtonBlackburn solver!" << std::endl;
    
		if(a == 0.0)
    {
      a = 1.0e-7;
    } 

    while (iter < max_iter)
    {
      fun =  2.0*log(a*(a+2.0)/(1.0-a)) + 108.0*pow(a,2.0) - 32700.0/b + 9.92 - c;

      deriv = 216.0*a + 2.0*(pow(a,2.0)-2.0*a-2.0)/((a-1.0)*a*(2.0+a));

      x1 = a - fun/deriv;
      a = x1;

      if(abs(fun)<tol) return x1;

      iter++;
    }
    return x1;
  }

	double NewtonLangmuirBasedModel(double initial_value, std::vector<double> parameter, double increment)
	{
		/// Solver for the ODE [y' = K(1-beta*exp(alpha*x)))]
		/// @param parameter[0] = K
		/// @param parameter[1] = beta
		/// @param parameter[2] = alpha

		double K = parameter.at(0);
		double beta = parameter.at(1);
		double alpha = parameter.at(2);
		double x0 = initial_value;
		double x00 = initial_value;

    double fun(0.0);
    double deriv(0.0);
    double x1(0.0);
    unsigned short int iter(0);
    const double tol(1.0e-3);
    const unsigned short int max_iter(50);

    while (iter < max_iter)
    {
      fun = x0 - x00 - K * increment + K * beta * exp(alpha * x0) * increment;

      deriv = 1.0 + K * beta * alpha * exp(alpha * x0) * increment;

      x1 = x0 - fun/deriv;
      x0 = x1;

      if(abs(fun)<tol) return x1;

      iter++;
    }
    return x1;
	}


	Solver() {}
	~Solver() {}
};

#endif
