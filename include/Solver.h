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

/**
 * @brief Class providing solver methods for the SCIANTIX simulation framework.
 *
 * The Solver class contains various numerical methods to solve differential equations
 * and other mathematical problems encountered in the simulation. These solvers are
 * used in conjunction with models within the Simulation class.
 */
class Solver : public InputVariable
{
public:

	/**
	 * @brief Integrates the ODE y' = + S.
	 *
	 * @param initial_value The initial value of the dependent variable.
	 * @param parameter The source term.
	 * @param increment The time increment.
	 * @return The updated value after integration.
	 */
	double Integrator(double initial_value, double parameter, double increment)
	{
		return initial_value + parameter * increment;
	}


	/**
	 * @brief Solves the ODE y' = k / y + S using a limited growth model.
	 *
	 * @param initial_value The initial value of the dependent variable.
	 * @param parameter A vector containing the growth rate [0] and source term [1].
	 * @param increment The time increment.
	 * @return The updated value after solving the ODE.
	 */
	double LimitedGrowth(double initial_value, std::vector<double> parameter, double increment)
	{
		return 0.5 * ((initial_value + parameter[1] * increment) + sqrt(pow(initial_value + parameter[1] * increment, 2) + 4.0 * parameter[0] * increment));
	}


	/**
	 * @brief Solves the ODE y' = - L y + S using a decay model.
	 *
	 * @param initial_condition The initial value of the dependent variable.
	 * @param decay_rate The decay rate.
	 * @param source_term The source.
	 * @param increment The time increment.
	 * @return The updated value after solving the ODE.
	 */
	double Decay(double initial_condition, double decay_rate, double source_term, double increment)
	{
		return (initial_condition + source_term * increment) / (1.0 + decay_rate * increment);
	}


	/**
	 * @brief Solves the ODE y' = -k y**2 using a binary interaction model.
	 *
	 * @param initial_condition The initial value of the dependent variable.
	 * @param interaction_coefficient The interaction coefficient.
	 * @param increment The time increment.
	 * @return The updated value after solving the ODE.
	 */
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

	/**
	 * @brief Solves the ODE y' = -k y**2 using a binary interaction model.
	 *
	 * @param initial_condition The initial value of the dependent variable.
	 * @param interaction_coefficient The interaction coefficient.
	 * @param increment The time increment.
	 * @return The updated value after solving the ODE.
	 */
	double BinaryInteractionVerification(double initial_condition, double interaction_coefficient, double increment,int mode)
	/// Solver for the ODE [y' = -k y**2]
	{
		if(mode == 0){
			if(increment == 0.0)
				return initial_condition;
			else
				return initial_condition = 0.25 * (sqrt(1 + 8 * initial_condition * increment) - 1) / increment;
			
		}
		if(mode == 1 ) {
			return initial_condition / (1.0 + interaction_coefficient * initial_condition * increment);
		}
		std::cout << "no correct mode used" ;
	}


	/**
	 * @brief Solves the spatially averaged PDE dy/dt = D div grad y + S - L y using a spectral approach.
	 * We apply a spectral approach in space, projecting the equation on the eigenfunctions of the laplacian operator.
	 * We use the first order backward Euler solver in time.
	 * The number of terms in the expansion, N, is fixed a priori.
	 *
	 * @param initial_condition The initial conditions for the diffusion modes.
	 * @param parameter A vector containing the parameters for the diffusion equation.
	 * @param increment The time increment.
	 * @return The updated value after solving the PDE.
	 *
	 *
	 *  Parameters :
	 * 0 : N_modes
	 * 1 : D
	 * 2 : r
	 * 3 :production
	 * 4 :loss rate
	 */
	double SpectralDiffusion(double *initial_condition, std::vector<double> parameter, double increment)
	{
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


	/**
	 * @brief Function to compute the dot product between two arrays (v and u) of size n
	 *
	 * @param u The vector.
	 * @param v The array.
	 * @param n The size of the vector and array.
	 * @return The dot product result.
	 */
	double dotProduct1D(std::vector<double> u, double v[], int n)
	{
		double result = 0.0;
		for (int i = 0; i < n; ++i)
			result += u[i] * v[i];
		return result;
	}


	/**
	 * @brief Computes the dot product of a 2D matrix and a 1D array.
	 *
	 * @param A The matrix.
	 * @param v The array.
	 * @param n_rows The number of rows in the matrix.
	 * @param n_col The number of columns in the matrix.
	 * @param result The result array.
	 */
	void dotProduct2D(double A[], double v[], int n_rows, const int n_col, double result[])
	{
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

	/**
	 * @brief Solves two coupled diffusion equations using a spectral approach.
	 *
	 * @param gas_1 The first gas variable.
	 * @param gas_2 The second gas variable.
	 * @param initial_condition_gas_1 Initial conditions for the first gas.
	 * @param initial_condition_gas_2 Initial conditions for the second gas.
	 * @param parameter A vector containing the parameters for the diffusion equations.
	 * @param increment The time increment.
	 */
	void SpectralDiffusion2equations(double &gas_1, double &gas_2, double *initial_condition_gas_1, double *initial_condition_gas_2, std::vector<double> parameter, double increment)
	{
		unsigned short int n(0);
		unsigned short int np1(1);

		double diffusion_rate1(0.0);
		double diffusion_rate2(0.0);

		double diffusion_rate_coeff1(0.0);
		double diffusion_rate_coeff2(0.0);

		double source_rate1(0.0);
		double source_rate2(0.0);

		double source_rate_coeff_1(0.0);
		double source_rate_coeff_2(0.0);

		double projection_coeff(0.0);

		double gas_1_solution(0.);
		double gas_2_solution(0.);

		double coeff_matrix[4];
		double initial_conditions[2];

		const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

		diffusion_rate_coeff1 = pow(pi, 2) * parameter.at(1) / pow(parameter.at(3), 2); // pi^2 * D1 / a^2
		diffusion_rate_coeff2 = pow(pi, 2) * parameter.at(2) / pow(parameter.at(3), 2); // pi^2 * D2 / a^2

		projection_coeff = -2.0 * sqrt(2.0 / pi);

		source_rate_coeff_1 = projection_coeff * parameter.at(4); // - 2 sqrt(2/pi) * S1
		source_rate_coeff_2 = projection_coeff * parameter.at(5); // - 2 sqrt(2/pi) * S2

		for (n = 0; n < parameter.at(0); n++)
		{
			np1 = n + 1;
			const double n_coeff = pow(-1.0, np1) / np1;

			diffusion_rate1 = diffusion_rate_coeff1 * pow(np1, 2); // pi^2 * D1 * n^2 / a^2
			diffusion_rate2 = diffusion_rate_coeff2 * pow(np1, 2); // pi^2 * D2 * n^2 / a^2

			source_rate1 = source_rate_coeff_1 * n_coeff; // - 2 sqrt(2/pi) * S1 * (-1)^n/n
			source_rate2 = source_rate_coeff_2 * n_coeff; // - 2 sqrt(2/pi) * S2 * (-1)^n/n

			coeff_matrix[0] = 1.0 + (diffusion_rate1 + parameter.at(7) + parameter.at(8)) * increment;
			coeff_matrix[1] = -parameter.at(6) * increment;
			coeff_matrix[2] = -parameter.at(7) * increment;
			coeff_matrix[3] = 1.0 + (diffusion_rate2 + parameter.at(6) + parameter.at(8)) * increment;

			initial_conditions[0] = initial_condition_gas_1[n] + source_rate1 * increment;
			initial_conditions[1] = initial_condition_gas_2[n] + source_rate2 * increment;

			Solver::Laplace2x2(coeff_matrix, initial_conditions);

			initial_condition_gas_1[n] = initial_conditions[0];
			initial_condition_gas_2[n] = initial_conditions[1];

			gas_1_solution += projection_coeff * n_coeff * initial_conditions[0] / ((4. / 3.) * pi);
			gas_2_solution += projection_coeff * n_coeff * initial_conditions[1] / ((4. / 3.) * pi);
		}
		gas_1 = gas_1_solution;
		gas_2 = gas_2_solution;
	}


	/**
	 * @brief Solves three coupled diffusion equations using a spectral approach.
	 *
	 * @param gas_1 The first gas variable.
	 * @param gas_2 The second gas variable.
	 * @param gas_3 The third gas variable.
	 * @param initial_condition_gas_1 Initial conditions for the first gas.
	 * @param initial_condition_gas_2 Initial conditions for the second gas.
	 * @param initial_condition_gas_3 Initial conditions for the third gas.
	 * @param parameter A vector containing the parameters for the diffusion equations.
	 * @param increment The time increment.
	 */
	void SpectralDiffusion3equations(double &gas_1, double &gas_2, double &gas_3, double *initial_condition_gas_1, double *initial_condition_gas_2, double *initial_condition_gas_3, std::vector<double> parameter, double increment)
	{
		unsigned short int n(0);
		unsigned short int np1(1);

		double diffusion_rate1(0.0);
		double diffusion_rate2(0.0);
		double diffusion_rate3(0.0);

		double diffusion_rate_coeff1(0.0);
		double diffusion_rate_coeff2(0.0);
		double diffusion_rate_coeff3(0.0);

		double source_rate1(0.0);
		double source_rate2(0.0);
		double source_rate3(0.0);

		double source_rate_coeff_1(0.0);
		double source_rate_coeff_2(0.0);
		double source_rate_coeff_3(0.0);

		double projection_coeff(0.0);

		double gas_1_solution(0.);
		double gas_2_solution(0.);
		double gas_3_solution(0.);

		double coeff_matrix[9];
		double initial_conditions[3];

		const double pi = CONSTANT_NUMBERS_H::MathConstants::pi;

		diffusion_rate_coeff1 = pow(pi, 2) * parameter.at(1) / pow(parameter.at(4), 2); // pi^2 * D1 / a^2
		diffusion_rate_coeff2 = pow(pi, 2) * parameter.at(2) / pow(parameter.at(4), 2); // pi^2 * D2 / a^2
		diffusion_rate_coeff3 = pow(pi, 2) * parameter.at(3) / pow(parameter.at(4), 2); // pi^2 * D3 / a^2

		projection_coeff = -2.0 * sqrt(2.0 / pi);

		source_rate_coeff_1 = projection_coeff * parameter.at(5); // - 2 sqrt(2/pi) * S1
		source_rate_coeff_2 = projection_coeff * parameter.at(6); // - 2 sqrt(2/pi) * S2
		source_rate_coeff_3 = projection_coeff * parameter.at(7); // - 2 sqrt(2/pi) * S3

		for (n = 0; n < parameter.at(0); n++)
		{
			np1 = n + 1;
			const double n_coeff = pow(-1.0, np1) / np1;

			diffusion_rate1 = diffusion_rate_coeff1 * pow(np1, 2); // pi^2 * D1 * n^2 / a^2
			diffusion_rate2 = diffusion_rate_coeff2 * pow(np1, 2); // pi^2 * D2 * n^2 / a^2
			diffusion_rate3 = diffusion_rate_coeff3 * pow(np1, 2); // pi^2 * D3 * n^2 / a^2

			source_rate1 = source_rate_coeff_1 * n_coeff; // - 2 sqrt(2/pi) * S * (-1)^n/n
			source_rate2 = source_rate_coeff_2 * n_coeff;
			source_rate3 = source_rate_coeff_3 * n_coeff;

			coeff_matrix[0] = 1.0 + (diffusion_rate1 + parameter.at(9) + parameter.at(10) + parameter.at(11)) * increment;
			coeff_matrix[1] = -parameter.at(8) * increment;
			coeff_matrix[2] = 0.0;

			coeff_matrix[3] = -parameter.at(9) * increment;
			coeff_matrix[4] = 1.0 + (diffusion_rate2 + parameter.at(8) + parameter.at(10) + parameter.at(11)) * increment;
			coeff_matrix[5] = 0.0;

			coeff_matrix[6] = -parameter.at(11) * increment;
			coeff_matrix[7] = -parameter.at(11) * increment;
			coeff_matrix[8] = 1.0 + (diffusion_rate3 + parameter.at(10)) * increment;

			initial_conditions[0] = initial_condition_gas_1[n] + source_rate1 * increment;
			initial_conditions[1] = initial_condition_gas_2[n] + source_rate2 * increment;
			initial_conditions[2] = initial_condition_gas_3[n] + source_rate3 * increment;

			Solver::Laplace3x3(coeff_matrix, initial_conditions);

			initial_condition_gas_1[n] = initial_conditions[0];
			initial_condition_gas_2[n] = initial_conditions[1];
			initial_condition_gas_3[n] = initial_conditions[2];

			gas_1_solution += projection_coeff * n_coeff * initial_conditions[0] / ((4. / 3.) * pi);
			gas_2_solution += projection_coeff * n_coeff * initial_conditions[1] / ((4. / 3.) * pi);
			gas_3_solution += projection_coeff * n_coeff * initial_conditions[2] / ((4. / 3.) * pi);
		}
		gas_1 = gas_1_solution;
		gas_2 = gas_2_solution;
		gas_3 = gas_3_solution;
	}

	/**
	 * @brief Solves a system of two linear equations using Cramer's method.
	 *
	 * @param A The coefficient matrix.
	 * @param b The constant terms vector.
	 */
	void Laplace2x2(double A[], double b[])
	{
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


	/**
	 * @brief Solves a system of three linear equations according to Cramer's method.
	 *
	 * @param A The coefficient matrix.
	 * @param b The constant terms vector.
	 */
	void Laplace3x3(double A[], double b[])
	{
		double detX(0.0), detY(0.0), detZ(0.0);
		double detA = A[0] * (A[4] * A[8] - A[5] * A[7]) - A[1] * (A[3] * A[8] - A[5] * A[6]) + A[2] * (A[3] * A[7] - A[4] * A[6]);

		if (detA != 0.0)
		{
			detX = b[0] * (A[4] * A[8] - A[5] * A[7]) - A[1] * (b[1] * A[8] - A[5] * b[2]) + A[2] * (b[1] * A[7] - A[4] * b[2]);
			detY = A[0] * (b[1] * A[8] - A[5] * b[2]) - b[0] * (A[3] * A[8] - A[5] * A[6]) + A[2] * (A[3] * b[2] - b[1] * A[6]);
			detZ = A[0] * (A[4] * b[2] - b[1] * A[7]) - A[1] * (A[3] * b[2] - b[1] * A[6]) + b[0] * (A[3] * A[7] - A[4] * A[6]);
			b[0] = detX / detA;
			b[1] = detY / detA;
			b[2] = detZ / detA;
		}
	}


	/**
	 * @brief Computes the determinant of a NxN matrix according to Cramer's method.
	 *
	 * @param N The size of the matrix.
	 * @param A The matrix.
	 * @return The determinant of the matrix.
	 */
	double det(int N, double A[])
	{
		int dim = N * N;
		// int Nm1 = N-1;
		double C[(N - 1) * (N - 1)];
		double sum = 0.0;

		if (N == 2)
		{
			return A[0] * A[3] - A[1] * A[2];
		}
		else
		{
			for (int i = 0; i < N; i++)
			{
				int j = 0;
				for (int z = N; z < dim; z++)
				{
					if ((dim - z + i) % N != 0)
					{
						C[j] = A[z];
						j++;
					}
				}
				int r = i / N;
				int c = i % N;
				sum += ((r + c) % 2 == 0 ? +1 : -1) * A[i] * det(N - 1, C);
			}
			return sum;
		}
	}

	/**
	 * @brief Solves a system of linear equations using the Laplace method.
	 *
	 * @param N The size of the matrix.
	 * @param A The coefficient matrix.
	 * @param b The constant terms vector.
	 */
	void Laplace(int N, double A[], double b[])
	{
		int dim = N * N;
		double detA = det(N, A);
		double M[dim];
		double detX = 0.0;
		double x[N];

		for (int i = 0; i < N; i++)
		{
			for (int j = 0; j < dim; j++)
			{
				if (j % N == i)
				{
					M[j] = b[j / N];
				}
				else
				{
					M[j] = A[j];
				}
			}
			detX = det(N, M);
			x[i] = detX / detA;
		}
		for (int i = 0; i < N; i++)
		{
			b[i] = x[i];
		}
	}

	/**
	 * @brief Solver for the quartic equation ax^4 + bx^3 +cx^2 +dx + e = 0
	 * with the iterative Newton's method.
	 *
	 * @param parameter A vector containing the coefficients of the equation.
	 * parameter.at(0) initial conditions
	 * parameter.at(1) coefficient of x^4
	 * parameter.at(2) coefficient of x^3
	 * parameter.at(3) coefficient of x^2
	 * parameter.at(4) coefficient of x^1
	 * parameter.at(5) coefficient of x^0
	 * @return x1, the solution to the equation.
	 */
	double QuarticEquation(std::vector<double> parameter)
	{
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
			function = a * pow(y0, 4) + b * pow(y0, 3) + c * pow(y0, 2) + d * y0 + e;
			derivative = 4.0 * a * pow(y0, 3) + 3.0 * b * pow(y0, 2) + 2.0 * c * y0 + d;

			y1 = y0 - function / derivative;
			y0 = y1;

			if (function < tol)
				return y1;

			iter++;
		}
		return y1;
	}

	/**
	 * @brief Initializes the diffusion modes.
	 *
	 * @param n_modes The number of diffusion modes.
	 * @param mode_initial_condition The initial condition for the modes.
	 * @param diffusion_modes The diffusion modes array.
	 */
	void modeInitialization(int n_modes, double mode_initial_condition, double *diffusion_modes)
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

	/**
	 * @brief Solver for the non-linear equation (Blackburn's thermochemical urania model) log(PO2(x)) = 2.0*log(x*(x+2.0)/(1.0-x)) + 108.0*pow(x,2.0) - 32700.0/T + 9.92
	 * with the iterative Newton's method.
	 * 
	 * @param parameter A vector containing the parameters of the equation.
	 * @return The solution to the equation.
	 */
	double NewtonBlackburn(std::vector<double> parameter)
	{
		double fun(0.0);
		double deriv(0.0);
		double x1(0.0);
		unsigned short int iter(0);
		const double tol(1.0e-3);
		const unsigned short int max_iter(50);

		double a = parameter.at(0);
		double b = parameter.at(1);
		double c = log(parameter.at(2));

		if (parameter.at(2) == 0)
			std::cout << "Warning: check NewtonBlackburn solver!" << std::endl;

		if (a == 0.0)
			a = 1.0e-7;

		while (iter < max_iter)
		{
			fun = 2.0 * log(a * (a + 2.0) / (1.0 - a)) + 108.0 * pow(a, 2.0) - 32700.0 / b + 9.92 - c;

			deriv = 216.0 * a + 2.0 * (pow(a, 2.0) - 2.0 * a - 2.0) / ((a - 1.0) * a * (2.0 + a));

			x1 = a - fun / deriv;
			a = x1;

			if (abs(fun) < tol)
				return x1;

			iter++;
		}
		return x1;
	}

	/**
	 * @brief Solver for the ODE [y' = K(1-beta*exp(alpha*y)))]
	 *
	 * @param initial_value The initial value of the dependent variable.
	 * @param parameter A vector containing the parameters of the ODE.
	 * @param increment The time increment.
	 * 
	 * parameter[0] = K
	 * parameter[1] = beta
	 * parameter[2] = alpha
	 * 
	 * @return The solution to the ODE.
	 */
	double NewtonLangmuirBasedModel(double initial_value, std::vector<double> parameter, double increment)
	{
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

			x1 = x0 - fun / deriv;
			x0 = x1;

			if (abs(fun) < tol)
				return x1;

			iter++;
		}
		return x1;
	}

	/**
	 * @brief Default constructor for the Solver class.
	 */
	Solver() {}
	/**
	 * @brief Destructor for the Solver class.
	 */
	~Solver() {}
};

#endif // SOLVER_H