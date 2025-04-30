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

#include "Solver.h"

double Solver::Integrator(double initial_value, double parameter, double increment)
{
    return initial_value + parameter * increment;
}

double Solver::LimitedGrowth(double initial_value, std::vector<double> parameter, double increment)
{
    return 0.5 * ((initial_value + parameter[1] * increment) + sqrt(pow(initial_value + parameter[1] * increment, 2) + 4.0 * parameter[0] * increment));
}

double Solver::Decay(double initial_condition, double decay_rate, double source_term, double increment)
{
    return (initial_condition + source_term * increment) / (1.0 + decay_rate * increment);
}

double Solver::BinaryInteraction(double initial_condition, double interaction_coefficient, double increment)
{
    return initial_condition / (1.0 + interaction_coefficient * initial_condition * increment);
}

double Solver::SpectralDiffusion(double *initial_condition, std::vector<double> parameter, double increment)
{
    size_t n;
    unsigned short int np1(1);

    double diffusion_rate_coeff(0.0);
    double diffusion_rate(0.0);
    double source_rate_coeff(0.0);
    double source_rate(0.0);
    double projection_coeff(0.0);
    double solution(0.0);

    diffusion_rate_coeff = pow(M_PI, 2) * parameter.at(1) / pow(parameter.at(2), 2);
    projection_coeff = -2.0 * sqrt(2.0 / M_PI);
    source_rate_coeff = projection_coeff * parameter.at(3);

    for (n = 0; n < parameter.at(0); n++)
    {
        np1 = n + 1;
        const double n_coeff = pow(-1.0, np1) / np1;

        diffusion_rate = diffusion_rate_coeff * pow(np1, 2) + parameter.at(4);
        source_rate = source_rate_coeff * n_coeff;
        
        initial_condition[n] = Solver::Decay(initial_condition[n], 1.0, 0.0, parameter.at(6));

        initial_condition[n] = Solver::Decay(initial_condition[n], diffusion_rate, source_rate, increment);

        initial_condition[n] = Solver::Integrator(initial_condition[n], n_coeff*projection_coeff*parameter.at(5), 1);

        solution += projection_coeff * n_coeff * initial_condition[n] / ((4. / 3.) * M_PI);
    }

    return solution;
}

double Solver::dotProduct1D(std::vector<double> u, double v[], int n)
{
    double result = 0.0;
    for (int i = 0; i < n; ++i)
        result += u[i] * v[i];
    return result;
}

void Solver::dotProduct2D(double A[], double v[], int n_rows, const int n_col, double result[])
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

void Solver::SpectralDiffusion2equations(double &gas_1, double &gas_2, double *initial_condition_gas_1, double *initial_condition_gas_2, std::vector<double> parameter, double increment)
{
    size_t n;
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

    diffusion_rate_coeff1 = pow(M_PI, 2) * parameter.at(1) / pow(parameter.at(3), 2); // pi^2 * D1 / a^2
    diffusion_rate_coeff2 = pow(M_PI, 2) * parameter.at(2) / pow(parameter.at(3), 2); // pi^2 * D2 / a^2

    projection_coeff = -2.0 * sqrt(2.0 / M_PI);

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

        gas_1_solution += projection_coeff * n_coeff * initial_conditions[0] / ((4. / 3.) * M_PI);
        gas_2_solution += projection_coeff * n_coeff * initial_conditions[1] / ((4. / 3.) * M_PI);
    }
    gas_1 = gas_1_solution;
    gas_2 = gas_2_solution;
}

void Solver::SpectralDiffusion3equations(double &gas_1, double &gas_2, double &gas_3, double *initial_condition_gas_1, double *initial_condition_gas_2, double *initial_condition_gas_3, std::vector<double> parameter, double increment)
{
    size_t n;
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

    diffusion_rate_coeff1 = pow(M_PI, 2) * parameter.at(1) / pow(parameter.at(4), 2); // pi^2 * D1 / a^2
    diffusion_rate_coeff2 = pow(M_PI, 2) * parameter.at(2) / pow(parameter.at(4), 2); // pi^2 * D2 / a^2
    diffusion_rate_coeff3 = pow(M_PI, 2) * parameter.at(3) / pow(parameter.at(4), 2); // pi^2 * D3 / a^2

    projection_coeff = -2.0 * sqrt(2.0 / M_PI);

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

        gas_1_solution += projection_coeff * n_coeff * initial_conditions[0] / ((4. / 3.) * M_PI);
        gas_2_solution += projection_coeff * n_coeff * initial_conditions[1] / ((4. / 3.) * M_PI);
        gas_3_solution += projection_coeff * n_coeff * initial_conditions[2] / ((4. / 3.) * M_PI);
    }
    gas_1 = gas_1_solution;
    gas_2 = gas_2_solution;
    gas_3 = gas_3_solution;
}

void Solver::SpectralDiffusionNonEquilibriumCylinder(double& gas_solution, double& gas_bubble, double* initial_condition_gas_solution, double* initial_condition_gas_bubble, int N, double diffusion_coefficient, double resolution_rate, double trapping_rate, double domain_radius, double source_term, double source_term_bubbles, double time_step)
{
    unsigned short int n(0);

    double diffusion_rate_coeff(0.0);
    double diffusion_rate(0.0);
    double source_rate_coeff_solution(0.0);
    double source_rate_coeff_bubbles(0.0);
    double source_rate_solution(0.0);
    double source_rate_bubble(0.0);
    double projection_coeff(0.0);
    double gas_solution_solution(0.0);
    double gas_bubble_solution(0.0);
    double coeff_matrix[4];
    double initial_conditions[2];

    const double zJ0[40]  = {2.4048,5.5201,8.6537,11.7915,14.9309,18.0711,21.2116,24.3525,27.4935,30.6346,33.7758,36.9171,40.0584,43.1998,46.3412,49.4826,52.6241,55.7655,58.9070,62.0485,65.1899648,68.33146933,71.4729816,74.61450064,77.75602563,80.89755587,84.03909078,87.18062984,90.32217264,93.46371878,96.60526795,99.74681986,102.8883743,106.0299309,109.1714896,112.3130503,115.4546127,118.5961766,121.7377421,124.8793089};
    const double zJ02[40] = {5.7832,30.4713,74.8870,139.0403,222.9323,326.5634,449.9335,593.0429,755.8914,938.4791,1140.8060,1362.8722,1604.6775,1866.2220,2147.5057,2448.5287,2769.2908,3109.7922,3470.0328,3850.0125,4249.73151065221,4669.18970077716,5108.38709930765,5567.32370630898,6045.99952183356,6544.41454592383,7062.56877861446,7600.46221993398,8158.09486990606,8735.46672855046,9332.57779588379,9949.42807192008,10586.0175566713,11242.3462501475,11918.4141523576,12614.2212633090,13329.7675830083,14065.0531114611,14820.0778486725,15594.8417946467};

    diffusion_rate_coeff = 14.0 * diffusion_coefficient / (4.0 * pow(domain_radius, 2));
    projection_coeff = 2.0;
    source_rate_coeff_solution = projection_coeff * source_term;
    source_rate_coeff_bubbles  = projection_coeff * source_term_bubbles;

    for (n = 0; n < N; n++)
    {
        diffusion_rate = diffusion_rate_coeff * zJ02[n];
        source_rate_solution = source_rate_coeff_solution / zJ0[n];
        source_rate_bubble   = source_rate_coeff_bubbles / zJ0[n];

        coeff_matrix[0] =  1.0 + ( diffusion_rate + trapping_rate ) * time_step;
        coeff_matrix[1] =  - resolution_rate * time_step;
        coeff_matrix[2] =  - trapping_rate * time_step;
        coeff_matrix[3] =  1.0 +  resolution_rate  * time_step;
        initial_conditions[0]  =  initial_condition_gas_solution[n] + source_rate_solution * time_step;
        initial_conditions[1]  =  initial_condition_gas_bubble[n]   + source_rate_bubble   * time_step;

        Solver::Laplace2x2(coeff_matrix, initial_conditions);
        initial_condition_gas_solution[n] = initial_conditions[0];
        initial_condition_gas_bubble[n] = initial_conditions[1];

        gas_solution_solution  += initial_conditions[0]  * (projection_coeff / zJ0[n]);
        gas_bubble_solution    += initial_conditions[1] * (projection_coeff / zJ0[n]);
    }
    gas_solution = gas_solution_solution;
    gas_bubble   = gas_bubble_solution;
};

void Solver::Laplace2x2(double A[], double b[])
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

void Solver::Laplace3x3(double A[], double b[])
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

double Solver::det(int N, double A[])
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

void Solver::Laplace(int N, double A[], double b[])
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

double Solver::QuarticEquation(std::vector<double> parameter)
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

void Solver::modeInitialization(int n_modes, double mode_initial_condition, double *diffusion_modes)
{
    // projection on diffusion modes of the initial conditions
    double initial_condition(0.0);
    double projection_remainder(0.0);
    double reconstructed_solution(0.0);
    int iteration(0), iteration_max(20), n(0), np1(1);
    double projection_coeff(0.0);
    projection_coeff = -sqrt(8.0 / M_PI);

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
            reconstructed_solution += projection_coeff * n_coeff * diffusion_modes[n] * 3.0 / (4.0 * M_PI);
        }
        projection_remainder = initial_condition - reconstructed_solution;
    }
}

double Solver::NewtonBlackburn(std::vector<double> parameter)
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

double Solver::NewtonLangmuirBasedModel(double initial_value, std::vector<double> parameter, double increment)
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