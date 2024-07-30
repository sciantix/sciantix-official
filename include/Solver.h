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
    
	double ROM_Cylinder(double* initial_condition, std::vector<double> parameter, double increment)
  	{
		/**
		 @brief Solver for the spatially averaged solution of the systems of PDEs: 
		 *  	  | dy1/dt = D(y2) div grad y1 + S
		 * 		  | dy2/dt = alpha div grad y2 + Q
		 *        We apply a spectral approach in space, projecting the equation on the spatial modes 
		 *        constructed by means the Proper Orthogonal Decomposition technique (G. Berkooz et al. (1993) 
		 *        https://doi.org/10.1146/annurev.fl.25.010193.002543). 
		 *	      We use the first order backward Euler solver in time. 
		 *        The number of modes adopted is fixed a priori at 5.
		 *	  	  From Di Gennaro, "Multi-physics development and application of a reduced order model 
		 * 	      of fission gas diffusion in fuel performance codes." Master thesis, Politecnico di Milano, 2021. 
		 * @param[in] parameter.at(0) number of modes (not used)
		 * @param[in] parameter.at(1) diffusivity 
		 * @param[in] parameter.at(2) radius of the cylinder
		 * @param[in] parameter.at(3) length of the cylinder
		 * @param[in] parameter.at(4) gas source term
		 * @param[in] parameter.at(5) thermal diffusivity
		 * @param[in] parameter.at(6) heat source term
		 * @param[in] parameter.at(7) temperature Tbc
		 * @param[in] parameter.at(8) alphaD
		 * @param[in] parameter.at(9) T0
		 */
		std::cout<<"ROM"<<std::endl;

  		std::cout<<"T0 " <<parameter.at(9)<<std::endl;
		//const double alphaD=2.8708e-19; 
		//const double D0=3.7691e-17;
		const double tau=1;  
		//const double D0=parameter.at(1); //Sbagliato, deve essere il coefficiente di diffusione valutato in T0
		//const double alphaD_verifica=((2.675e-5 * exp(-35200/parameter.at(9)))/(pow(parameter.at(9),2)))+((9.1858e-21*pow(3e19,0.5)*exp(-13834/parameter.at(9)))/(pow(parameter.at(9),2)));	

		const int dimC = 5;	
		const int dimT = 2;

		// System of ODEs for temporal coefficients
		// dx/dt = D/R^2 matrix_A x + S matrix_B
   		
	  	/// @param matrix_H matrix of coefficients
		double matrix_H[dimT][dimT] = 
		{
			{-0.20304529410438, -1.794194167602e-08},
    		{-1.7929870999053e-06, -3.0489219756252e-12}
		};
		/// @param matrix_J matrix of coefficients
		double matrix_J[dimT][dimT] = 
		{
			{0.86825783440324, 7.6256023299567e-08},
    		{7.6256023299567e-08, 6.6972975757376e-15}
		};
		/// @param matrix_K matrix of coefficients
		double matrix_K[dimT] = 
		{
			1.6481835446569, 
			1.4475414767746e-07
		};
		/// @param matrix_L matrix of coefficients
		double matrix_L[dimT] =
		{
			1.7679984122541,
			1.5612486870281e-07
		};
		/// @param matrix_X matrix of coefficients
		double matrix_X[dimC][dimC] =
		{
			{-6.058719664885, -0.55429841879809, -1.1754548439162, -2.0410958324088, -0.98343912117635},
			{-0.55429841879812, -7.1078459977489, -2.7823021602085, -5.0548352870777, -2.653584549214},
			{-1.1754548439162, -2.7823021602085, -12.290613439702, -12.702887263948, -8.0886618067476},
			{-2.0410958324088, -5.0548352870777, -12.702887263948, -33.729028729911, -21.789917775035},
			{-0.98343912117639, -2.653584549214, -8.0886618067475, -21.789917775034, -29.425141601677}
		};
		/// @param matrix_M matrix of coefficients
		double matrix_M1[dimT][dimC] =
		{
			{-3.3167101513421, -0.39850493973846, -0.65182466064754, -1.1222420423319, -0.55366283495148},
    		{-2.9307440834931e-07, -3.5535919570262e-08, -5.5987819326174e-08, -1.0116273749943e-07, -4.6328212742628e-08}
		};
		double matrix_M2[dimT][dimC] =
		{
			{-0.3985611417337, -3.9380323623978, -1.6115630433382, -2.7799745334155, -1.4912224707165},
    		{-3.5544209249796e-08, -3.4642504113336e-07, -1.445232940823e-07, -2.4408323183224e-07, -1.3337395181765e-07}
		};
		double matrix_M3[dimT][dimC] =
		{
			{-0.65174302280484, -1.6116576193174, -6.8076834247023, -7.1193214575871, -4.5027775683261},
    		{-5.5972426217128e-08, -1.4455322960431e-07, -5.9998315278438e-07, -6.2965526325548e-07, -3.9690279687705e-07}
		};
		double matrix_M4[dimT][dimC] =
		{
			{-1.1223060338343, -2.7798877039665, -7.1193950203681, -18.831524866587, -12.2698543299},
    		{-1.0120230332024e-07, -2.4407872646593e-07, -6.2969983736467e-07, -1.6629343258041e-06, -1.0845633073539e-06}
		};
		double matrix_M5[dimT][dimC] =
		{
			{-0.55359365772032, -1.4913275143428, -4.5026623866635, -12.269923156162, -16.442875824631},
    		{-4.6293149395284e-08, -1.3342830154277e-07, -3.9689351139282e-07, -1.0846151646332e-06, -1.4511876233436e-06}
		};
		/// @param matrix_N matrix of coefficients
		double matrix_N1[dimT][dimC] =
		{
			{3.4015975737584e-05, -6.5318871137904e-05, 7.9888352752989e-05, -6.148735777728e-05, 6.8239592556045e-05},
    		{-1.8392671600485e-10, 2.3356095633567e-10, -2.5349867630452e-10, 1.8121640915479e-10, -2.0738347467606e-10}
		};
		double matrix_N2[dimT][dimC] =
		{
			{-9.117032807446e-06, 5.7243333378549e-05, -9.7984971443193e-05, 8.3981065320342e-05, -9.8802748843154e-05},
    		{2.4743943005571e-10, -3.475158687674e-10, 3.1861661928719e-10, -2.6762904050654e-10, 2.9747303612563e-10}
		};

		double matrix_N3[dimT][dimC] =
		{
			{-1.7488873623134e-06, -3.4095773083916e-06, 6.0183572405547e-05, -7.6339529240589e-05, 0.00010397213006715},
    		{-2.8068676231522e-10, 3.5444411763358e-10, -3.7278299418121e-10, 2.5020144317844e-10, -2.9717264875267e-10}	
		};

		double matrix_N4[dimT][dimC] =
		{
			{2.5028477172353e-06, -2.8486911934598e-06, -2.7786900308149e-06, 3.9684693061977e-05, -6.9436717580297e-05},
    		{2.2971621382409e-10, -2.8332830649735e-10, 2.9507288798914e-10, -2.2405167954849e-10, 2.0127967514603e-10}
		};



		/// @param matrix_P matrix of coefficients
		double matrix_P[dimC] =
		{
			1.3561215930268,
			0.76462117999943,
			0.432354947383,
			0.45302040100184,
			0.28893362630681
		};


		/* double basesT_average_volume[dimT] = 
		{
			0.56509235729245,
			4.9901045994017e-08
		};*/ 
		double basesC_average_volume[dimC] = 
		{
			0.43344719229789,
    		0.24439025626201,
			0.1381904388094,
			0.14479558608727,
			0.092349734512828
		};
		
		// identity matrix
		double I_C[dimC][dimC], I_T[dimT][dimT];
  		for (int i=0;i<dimC;++i){
  	    	  for (int j=0;j<dimC;++j){
  	          if (i==j) I_C[i][j]=1.0;
  	     	  else I_C[i][j]=0.0;
  		}}
  		for (int i=0;i<dimT;++i){
  	    	  for (int j=0;j<dimT;++j){
  	     	  if (i==j) I_T[i][j]=1.0;
  	     	  else I_T[i][j]=0.0;
  		}}
		
		//Coefficienti Temperatura
       		const double C1=parameter.at(5)/pow(parameter.at(3),2);                        //Moltiplica H
       		const double C2=parameter.at(6);                             				   //Moltiplica L
       		const double C6=(tau*parameter.at(7))/pow(parameter.at(3),2);				   //Moltiplica K
       		const double C7=tau/pow(parameter.at(3),2);                  				   // Moltiplica J
       		//Coefficienti Concentrazione
			const double C3=(parameter.at(1)-parameter.at(8)*parameter.at(9))/pow(parameter.at(2),2);  //Moltiplica X
       		const double C4=parameter.at(8)/pow(parameter.at(2),2);                       //Moltiplica M e N
       		const double C5=parameter.at(4);                                              //Moltiplica P

		double lhs_T[dimT*dimT];
      	double rhs_T[dimT];

		// Implicit solution of the system of ODEs for temporal coefficients
		// (I - alpha/L^2 delta_t matrix_H - tau_T/L^2 delta_t matrix_J) * x_1 = (x_0 + Q matrix_L delta_t - tau_T T_BC/L^2 delta_t matrix_K)
		
		// lhs = (I - alpha/L^2 delta_t matrix_H - tau_T/L^2 delta_t matrix_J)
		int k=0;
      		for (int i = 0; i < dimT; ++i) {
        	  for (int j = 0; j < dimT; ++j) {
          	  //lhs_T[k]=I_T[i][j]-C1*increment*matrix_H[i][j]-C7*increment*matrix_J[i][j];
			  lhs_T[k]=I_T[i][j]-C1*increment*matrix_H[i][j]; 

          	  ++k;
      		}}
      		// rhs = (x_0 + Q matrix_L delta_t - tau_T T_BC/L^2 delta_t matrix_K)
      		for (int i = 0; i < dimT; ++i) {
          	  //rhs_T[i]=initial_condition[i]+C2*increment*matrix_L[i]-C6*increment*matrix_K[i];
          	  rhs_T[i]=initial_condition[i]+C2*increment*matrix_L[i];
			  
      		}
      
      	Solver::LUGauss(dimT,lhs_T,rhs_T);


		// Implicit solution of the system of ODEs for temporal coefficients
		// (I - 1/R^2 (D0-alphaD T0)matrix_X delta_t - alphaD/R^2 delta_t x' matrix_M - alphaD/R^2 delta_t x' matrix_N) * y_1 	= (y_0 + S matrix_P delta_t)

		double lhs_C[dimC*dimC];
      	double rhs_C[dimC];

		// lhs = (I - 1/R^2 (D0-alphaD T0)matrix_X delta_t - alphaD/R^2 delta_t x' matrix_M - alphaD/R^2 delta_t x' matrix_N)
		int z=0;
      		for (int i = 0; i < dimC; ++i) {
        	  for (int j = 0; j < dimC; ++j) {
            	  lhs_C[z]=I_C[i][j]-C3*increment*matrix_X[i][j]; 
            	    for (int k = 0; k < dimT; ++k)
             	    {
                	if(i==0) {lhs_C[z] += -C4*increment*rhs_T[k]*matrix_M1[k][j]-C4*increment*rhs_T[k]*matrix_N1[k][j];}
                	else if(i==1) {lhs_C[z] += -C4*increment*rhs_T[k]*matrix_M2[k][j]-C4*increment*rhs_T[k]*matrix_N2[k][j];}
                	else if(i==2) {lhs_C[z] += -C4*increment*rhs_T[k]*matrix_M3[k][j]-C4*increment*rhs_T[k]*matrix_N3[k][j];}
                	else if(i==3) {lhs_C[z] += -C4*increment*rhs_T[k]*matrix_M4[k][j]-C4*increment*rhs_T[k]*matrix_N4[k][j];}
                	else if(i==4) {lhs_C[z] += -C4*increment*rhs_T[k]*matrix_M5[k][j];}
             	    }
             	    ++z;
      	        }}

		// rhs = (y_0 + S matrix_P delta_t)
		int zz=0;
      		for (int i = dimT; i < dimT+dimC; ++i) {
          	  rhs_C[zz]=initial_condition[i]+C5*increment*matrix_P[zz];
          	  ++zz;
      		}
		
		Solver::LUGauss(dimC,lhs_C,rhs_C);
		
		for (int i = 0; i < dimT; ++i) {
        	  initial_condition[i] = rhs_T[i];
     		}
      
      		int zzz=0;
      		for (int i = dimT; i < dimT+dimC; ++i) {
        	  initial_condition[i] = rhs_C[zzz];
        	  ++zzz;
      		}

		double solution(0.);
		for (int i = 0; i < dimC; ++i)
		{ 
			solution += basesC_average_volume[i] * rhs_C[i];
		}
		
		return solution;

	}




	Solver() {}
	~Solver() {}
};

#endif
