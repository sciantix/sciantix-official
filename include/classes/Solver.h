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
//  Year: 2023                                                                      //
//  Authors: D. Pizzocri, G. Zullo.                                                 //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#ifndef SOLVER_h
#define SOLVER_h

#include <vector>
#include <string>
#include <cmath>
#include "InputVariable.h"

/**
 * @brief Class providing solver methods for the SCIANTIX simulation framework.
 *
 * The Solver class contains various numerical methods to solve differential equations
 * and other mathematical problems encountered in the simulation. These solvers are
 * used in conjunction with models within the Simulation class.
 * 
 * @author D. Pizzocri
 * @author T. Barani
 * @author G. Zullo
 * 
 * @ref <a href="https://www.sciencedirect.com/science/article/pii/S0022311517315039" target="_blank">Pizzocri D. et al (2018). Journal of Nuclear Materials, 502, 323-330.</a>
 * @ref <a href="https://www.sciencedirect.com/science/article/pii/S1738573321006148" target="_blank">Zullo G. et al (2022). Nuclear Engineering and Technology, 54, 1195-1205.</a>
 * 
 */
class Solver : virtual public InputVariable
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
	double Integrator(double initial_value, double parameter, double increment);


	/**
	 * @brief Solves the ODE y' = k / y + S using a limited growth model.
	 *
	 * @param initial_value The initial value of the dependent variable.
	 * @param parameter A vector containing the growth rate and source term.
	 * @param increment The time increment.
	 * @return The updated value after solving the ODE.
	 */
	double LimitedGrowth(double initial_value, std::vector<double> parameter, double increment);


	/**
	 * @brief Solves the ODE y' = - L y + S using a decay model.
	 *
	 * @param initial_condition The initial value of the dependent variable.
	 * @param decay_rate The decay rate.
	 * @param source_term The source.
	 * @param increment The time increment.
	 * @return The updated value after solving the ODE.
	 */
	double Decay(double initial_condition, double decay_rate, double source_term, double increment);


	/**
	 * @brief Solves the ODE y' = -k y**2 using a binary interaction model.
	 *
	 * @param initial_condition The initial value of the dependent variable.
	 * @param interaction_coefficient The interaction coefficient.
	 * @param increment The time increment.
	 * @return The updated value after solving the ODE.
	 */
	double BinaryInteraction(double initial_condition, double interaction_coefficient, double increment);


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
	double SpectralDiffusion(double *initial_condition, std::vector<double> parameter, double increment);


	/**
	 * @brief Function to compute the dot product between two arrays (v and u) of size n
	 *
	 * @param u The vector.
	 * @param v The array.
	 * @param n The size of the vector and array.
	 * @return The dot product result.
	 */
	double dotProduct1D(std::vector<double> u, double v[], int n);


	/**
	 * @brief Computes the dot product of a 2D matrix and a 1D array.
	 *
	 * @param A The matrix.
	 * @param v The array.
	 * @param n_rows The number of rows in the matrix.
	 * @param n_col The number of columns in the matrix.
	 * @param result The result array.
	 */
	void dotProduct2D(double A[], double v[], int n_rows, const int n_col, double result[]);

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
	void SpectralDiffusion2equations(double &gas_1, double &gas_2, double *initial_condition_gas_1, double *initial_condition_gas_2, std::vector<double> parameter, double increment);


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
	void SpectralDiffusion3equations(double &gas_1, double &gas_2, double &gas_3, double *initial_condition_gas_1, double *initial_condition_gas_2, double *initial_condition_gas_3, std::vector<double> parameter, double increment);

	/**
	 * @brief Solves a system of two linear equations using Cramer's method.
	 *
	 * @param A The coefficient matrix.
	 * @param b The constant terms vector.
	 */
	void Laplace2x2(double A[], double b[]);


	/**
	 * @brief Solves a system of three linear equations according to Cramer's method.
	 *
	 * @param A The coefficient matrix.
	 * @param b The constant terms vector.
	 */
	void Laplace3x3(double A[], double b[]);


	/**
	 * @brief Computes the determinant of a NxN matrix according to Cramer's method.
	 *
	 * @param N The size of the matrix.
	 * @param A The matrix.
	 * @return The determinant of the matrix.
	 */
	double det(int N, double A[]);

	/**
	 * @brief Solves a system of linear equations using the Laplace method.
	 *
	 * @param N The size of the matrix.
	 * @param A The coefficient matrix.
	 * @param b The constant terms vector.
	 */
	void Laplace(int N, double A[], double b[]);

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
	double QuarticEquation(std::vector<double> parameter);

	/**
	 * @brief Initializes the diffusion modes.
	 *
	 * @param n_modes The number of diffusion modes.
	 * @param mode_initial_condition The initial condition for the modes.
	 * @param diffusion_modes The diffusion modes array.
	 */
	void modeInitialization(int n_modes, double mode_initial_condition, double *diffusion_modes);

	/**
	 * @brief Solver for the non-linear equation (Blackburn's thermochemical urania model) log(PO2(x)) = 2.0*log(x*(x+2.0)/(1.0-x)) + 108.0*pow(x,2.0) - 32700.0/T + 9.92
	 * with the iterative Newton's method.
	 * 
	 * @param parameter A vector containing the parameters of the equation.
	 * @return The solution to the equation.
	 */
	double NewtonBlackburn(std::vector<double> parameter);
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
	double NewtonLangmuirBasedModel(double initial_value, std::vector<double> parameter, double increment);

	/**
	 * @brief Constructor
	 */
	Solver() {}
	/**
	 * @brief Destructor
	 */
	~Solver() {}
};

#endif // SOLVER_H
