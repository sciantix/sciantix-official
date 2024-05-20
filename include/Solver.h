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

	void SpectralDiffusion2equations(double& gas_1, double& gas_2, double* initial_condition_gas_1, double* initial_condition_gas_2, std::vector<double> parameter, double increment)
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

		projection_coeff = - 2.0 * sqrt(2.0 / pi);

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
			coeff_matrix[1] = - parameter.at(6) * increment;
			coeff_matrix[2] = - parameter.at(7) * increment;
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

	void SpectralDiffusion3equations(double& gas_1, double& gas_2, double& gas_3, double* initial_condition_gas_1, double* initial_condition_gas_2, double* initial_condition_gas_3, std::vector<double> parameter, double increment)
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

		projection_coeff = - 2.0 * sqrt(2.0 / pi);

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
			coeff_matrix[1] = - parameter.at(8) * increment;
			coeff_matrix[2] = 0.0;

			coeff_matrix[3] = - parameter.at(9) * increment;
			coeff_matrix[4] = 1.0 + (diffusion_rate2 + parameter.at(8) + parameter.at(10) + parameter.at(11)) * increment;
			coeff_matrix[5] = 0.0;
		
			coeff_matrix[6] = - parameter.at(11) * increment;
			coeff_matrix[7] = - parameter.at(11) * increment;
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

	/// The function solve a system of two linear equations according to Cramer method.
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

	//The function solve a system of three linear equations according to Cramer method.
	void Laplace3x3(double A[], double b[])
	{
		double detX(0.0), detY(0.0), detZ(0.0);
		double detA = A[0]*(A[4]*A[8]-A[5]*A[7]) - A[1]*(A[3]*A[8]-A[5]*A[6]) + A[2]*(A[3]*A[7]-A[4]*A[6]);

		if (detA != 0.0)
		{
			detX = b[0]*(A[4]*A[8]-A[5]*A[7]) - A[1]*(b[1]*A[8]-A[5]*b[2]) + A[2]*(b[1]*A[7]-A[4]*b[2]);
			detY = A[0]*(b[1]*A[8]-A[5]*b[2]) - b[0]*(A[3]*A[8]-A[5]*A[6]) + A[2]*(A[3]*b[2]-b[1]*A[6]);
			detZ = A[0]*(A[4]*b[2]-b[1]*A[7]) - A[1]*(A[3]*b[2]-b[1]*A[6]) + b[0]*(A[3]*A[7]-A[4]*A[6]);
			b[0] = detX/detA;
			b[1] = detY/detA;
			b[2] = detZ/detA;
		}
	}

	//The function compute the determinant of a NxN matrix according to Cramer method
	double det(int N, double A[])
	{
		int dim = N*N;
		// int Nm1 = N-1;
		double C[(N-1)*(N-1)];
		double sum = 0.0;

		if (N == 2) {
			return A[0]*A[3] - A[1]*A[2];
		}
		else {
			for (int i=0; i < N; i++) {
			int j = 0;
			for (int z = N; z < dim; z++) {
				if ((dim-z+i) % N != 0) {
				C[j] = A[z];
				j++;
				}
			}
				int r = i/N;
				int c = i%N;
				sum += ((r+c)%2==0 ? +1:-1)*A[i]*det(N-1, C);
			}
			return sum;
		}
	}

	void Laplace(int N, double A[], double b[])
	{
		int dim = N*N;
		double detA = det(N, A);
		double M[dim];
		double detX = 0.0;
		double x[N];

		for (int i = 0; i < N; i++) {
			for (int j = 0; j < dim; j++) {
			if (j%N == i) {
				M[j] = b[j/N];
			}
			else {
				M[j] = A[j];
			}
			}
			detX = det(N, M);
			x[i] = detX/detA;
		}
		for (int i = 0; i < N; i++) {
			b[i] = x[i];
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
			a = 1.0e-7;

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
		/// Solver for the ODE [y' = K(1-beta*exp(alpha*y)))]
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
    

	void LUGauss(const int N, double * A, double * b)
    {
		/**
		 * @brief LU decomposition, Doolittle algorithm, according to @ref Dahlquist, Germund, Ake Bjorck & Anderson, Ned, Numerical Methods, Prentice Hall, 1974.
		 * The function performs firstly the LU decomposition of the input matrix (A) storing in it the lower and upper matrixes.
		 * Then, the system solution is accomplished using the factorized matrix and a backward substitution method.
		 * The solution is stored and returned in the RHS vector b.
		 * NB: This function can be used also to perform only the decomposition itslef (i.e., for preconditioning)
		 * passing a dummy rhs.
		 * 
		 * @param[in] N dimension
		 * @param[in] A matrix of the linear system (in **1d-array form**)
		 * @param[in] b RHS vector
		 * 
		 * @param[out] b solution
		 * 
		 */

        int i, j, k, l;
        double sum = 0.0;
		std::vector<double> y(N);

        // Decomposition
        for(k=0;k<N;++k)
		{
    	for(j=k;j<N;++j)
			{
      	sum=0.;
        for(l=0;l<k;++l)
					sum += A[k*N+l] * A[l*N+j];
        A[k*N+j] = A[k*N+j] - sum;
        }
        for(i=k+1;i<N;++i)
			{
      	sum = 0.;
        for(l=0; l<k; ++l)
				sum += A[i*N+l] * A[l*N+k];
          A[i*N+k] = (A[i*N+k]-sum) / A[k*N+k];
        }
       }

		// System solution
		for(i=0;i<N;++i)
			{
			sum=0.;
			for(k=0;k<i;++k)
					sum += A[i*N+k] * y[k];
		y[i] = b[i] - sum;
		b[i] = 0.; // to cast solution in the input array b, first set this = 0
		}
		for(i=N-1; i>=0; --i)
			{
		sum=0.;
		for(k=i+1;k<N;++k)
					sum += A[i*N+k] * b[k];
		b[i] = (y[i]-sum) / A[i*N+i];
        }
    } 



	double ROM_Sphere5(double* initial_condition, std::vector<double> parameter, double increment)
    { 
		/**
		 * @brief Solver for the spatially averaged solution of the PDE [dy/dt = D div grad y + S]
		 *        We apply a spectral approach in space, projecting the equation on the spatial modes 
		 *        constructed by means the Proper Orthogonal Decomposition technique (G. Berkooz et al. (1993) 
		 *        https://doi.org/10.1146/annurev.fl.25.010193.002543). 
		 *	  We use the first order backward Euler solver in time. 
		 *    The number of modes adopted is fixed a priori at 5.
		 *	  From Pizzocri et al. (2023) https://doi.org/10.1016/j.net.2023.07.013 
		 * @param[in] parameter.at(0) dimension (not used)
		 * @param[in] parameter.at(1) diffusivity
		 * @param[in] parameter.at(2) radius of the sphere
		 * @param[in] parameter.at(3) source term
		 * @param[in] parameter.at(4) decay rate (not used)
		 * 
		 */
		std::cout << "Solver ROM 5" << std::endl; 

		const int dim = 5;

		// System of ODEs for temporal coefficients
		// dx/dt = D/R^2 matrix_A x + S matrix_B
   		
	  /// @param matrix_A matrix of coefficients
		double matrix_A[dim][dim] = 
		{
			{-10.5567873290090,-5.3064732472182,-3.3280716526395,2.3501142736907,1.6659233785950},
			{-5.3581471945348,-50.4583397658830,-33.5616140896220,22.8112511126060,16.0836928271950},
			{-3.5400454660848,-33.9187607249370,-142.9831205876900,110.6987594435100,74.1025248004660},
			{2.7859250612434,24.1168451798870,112.4203111575800,-321.7483158597200,-259.0064087538400},
			{2.4480172149492,18.6934083659180,79.4927395489150,-265.1402399821900,-578.9572380978200}
		};

		/// @param matrix_B vector of coefficients
		double matrix_B[dim] =
		{
			1.7195243191226	,
			0.8155431162763	,
			0.5158246426884	,
			-0.3643492476870,
			-0.2582971509203
		};

		/// @param bases_average_volume weighted volume average of the set of bases considered
		double bases_average_volume[dim] = 
		{
			0.41129530866285,
			0.19507084256177,
			0.12338078227278,
			-0.087149181097342,
			-0.061782439034486
		};
	
		// identity matrix
		double I[dim][dim];
		for (int i=0; i<dim; ++i)
		{
			for (int j=0; j<dim; ++j)
			{
				if (i==j) I[i][j]=1.0;
				else I[i][j]=0.0;
			}
		}
		
		// Implicit solution of the system of ODEs for temporal coefficients
		// (I - D/R^2 matrix_A delta_t) * x_1 = (x_0 + S matrix_B delta_t)

		const double diffusion_rate = parameter.at(1) / (pow(parameter.at(2),2));

		double lhs[dim*dim];
		double rhs[dim];

		// lhs=(I - D/R^2 matrix_A delta_t)
		int k=0;
		for (int i = 0; i < dim; ++i)
		{
			for (int j = 0; j < dim; ++j)
			{
				lhs[k] = I[i][j] - diffusion_rate * increment * matrix_A[i][j];
				++k;
			}
		}

		// rhs = (x_0 + S matrix_B delta_t)
		for (int i = 0; i < dim; ++i)
		{
			rhs[i] = initial_condition[i] + parameter.at(3) * increment * matrix_B[i];
		}

		Solver::LUGauss(dim, lhs, rhs);

		for (int i = 0; i < dim; ++i)
		{
			initial_condition[i] = rhs[i];
		}
		
		double solution(0.);
		for (int i = 0; i < dim; ++i)
		{ 
			solution += bases_average_volume[i] * rhs[i];
		}
		
		return solution;
	}	

    double ROM_Sphere10(double* initial_condition, std::vector<double> parameter, double increment)
    {
		
		/**
		 * @brief Solver for the spatially averaged solution of the PDE [dy/dt = D div grad y + S]
		 *        We apply a spectral approach in space, projecting the equation on the spatial modes 
		 *        constructed by means the Proper Orthogonal Decomposition technique (G. Berkooz et al. (1993) 
		 *        https://doi.org/10.1146/annurev.fl.25.010193.002543). 
		 *	  We use the first order backward Euler solver in time. 
		 *        The number of modes adopted is fixed a priori at 10.
		 *	  From Di Gennaro, "Multi-physics development and application of a reduced order model 
		 * 	  of fission gas diffusion in fuel performance codes." Master thesis, Politecnico di Milano, 2021. 
		 * @param[in] parameter.at(0) dimension (not used)
		 * @param[in] parameter.at(1) diffusivity
		 * @param[in] parameter.at(2) radius of the sphere
		 * @param[in] parameter.at(3) source term
		 * @param[in] parameter.at(4) decay rate (not used)
		 * 
		 */
		
		const int dim = 10;
		std::cout << "Solver ROM 10" << std::endl; 

		// System of ODEs for temporal coefficients
		// dx/dt = D/R^2 matrix_A x + S matrix_B
   		
	  /// @param matrix_A matrix of coefficients
		double matrix_A[dim][dim] = 
		{
			{-10.556787329009, -5.3064732472182, -3.3280716526395, 2.3501142736907, 1.665923378595, 1.0633360697322, -0.70785261370886, 0.5970297112339, 0.48066418427119, -0.45286971881565},
            {-5.3581471945348, -50.458339765883, -33.561614089622, 22.811251112606, 16.083692827195, 10.246297462826, -6.810193552188, 5.7548896447294, 4.5994046993301, -4.3707063962865},
            {-3.5400454660848, -33.918760724937, -142.98312058769, 110.69875944351, 74.102524800466, 46.57538806633, -30.860162065751, 25.823859681001, 20.879249625902, -19.471144431084},
            {2.7859250612434, 24.116845179887, 112.42031115758, -321.74831585972, -259.00640875384, -156.62693397849, 102.48624612451, -85.864010477117, -67.960456636593, 65.04332685305},
			{2.4480172149492, 18.693408365918, 79.492739548915, -265.14023998219, -578.95723809782, -432.30465259752, 280.52935468313, -227.03909194509, -187.24770089165, 174.14192137412},
			{2.2235536293717, 14.080140361898, 55.187927974064, -170.43458286824, -444.21383634827, -707.70686712461, 553.58941107847, -485.5700640659, -367.98721712359, 385.33613342372},
			{-1.9890147613692, -11.1459326551, -40.552081845857, 119.47288082985, 300.27451131434, 565.88986629718, -593.12292272217, 554.4479246217, 531.98441396064, -487.28122589426},
			{1.7449534155617, 9.8173868984077, 35.176703128563, -102.55506183267, -248.53760487628, -500.63708139, 556.91703966938, -1146.0279850325, -469.50563557407, 1140.1585170103},
			{2.3182041679351, 10.391887148701, 33.74551901412, -91.09250099891, -219.32676402787, -398.44257276116, 548.13720107213, -487.26029091955, -1040.4371477725, 704.69372703568},
			{-2.497553503587, -11.18157298981, -34.895950470614, 93.767635634499, 218.53175999434, 437.57297629369, -527.72963696754, 1181.4074735961, 735.06025995406, -2465.69731935}
		};

		/// @param matrix_B vector of coefficients
		double matrix_B[dim] =
		{
			1.7195243191226, 
			0.81554311627625, 
			0.51582464268838,
			-0.364349247687, 
			-0.25829715092032,
			-0.16488577033011,
			0.10974653963097,
			-0.09268408851984,
			-0.074425043402282,
			0.070374207181827 
		};

		/// @param bases_average_volume weighted volume average of the set of bases considered
		double bases_average_volume[dim] = 
		{ 
			0.41129530866285,
			0.19507084256177, 
			0.12338078227278, 
			-0.087149181097342, 
			-0.061782439034486, 
			-0.039439246684594, 
			0.026250420764756, 
			-0.022169230392364, 
			-0.01780182510824, 
			0.016832900205518
		};
	
		// identity matrix
		double I[dim][dim];
		for (int i=0; i<dim; ++i)
		{
			for (int j=0; j<dim; ++j)
			{
				if (i==j) I[i][j]=1.0;
				else I[i][j]=0.0;
			}
		}
		
		// Implicit solution of the system of ODEs for temporal coefficients
		// (I - D/R^2 matrix_A delta_t) * x_1 = (x_0 + S matrix_B delta_t)

		const double diffusion_rate = parameter.at(1) / (pow(parameter.at(2),2));

		double lhs[dim*dim];
		double rhs[dim];

		// lhs=(I - D/R^2 matrix_A delta_t)
		int k=0;
		for (int i = 0; i < dim; ++i)
		{
			for (int j = 0; j < dim; ++j)
			{
				lhs[k] = I[i][j] - diffusion_rate * increment * matrix_A[i][j];
				++k;
			}
		}

		// rhs = (x_0 + S matrix_B delta_t)
		for (int i = 0; i < dim; ++i)
		{
			rhs[i] = initial_condition[i] + parameter.at(3) * increment * matrix_B[i];
		}

		Solver::LUGauss(dim, lhs, rhs);

		for (int i = 0; i < dim; ++i)
		{
			initial_condition[i] = rhs[i];
		}
		
		double solution(0.);
		for (int i = 0; i < dim; ++i)
		{ 
			solution += bases_average_volume[i] * rhs[i];
		}
		
		return solution;
	}





	Solver() {}
	~Solver() {}
};

#endif
