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
    std::cout << "initial condition:" << *initial_condition << std::endl; //MDG 
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

        initial_condition[n] = Solver::Decay(initial_condition[n], diffusion_rate, source_rate, increment);

        solution += projection_coeff * n_coeff * initial_condition[n] / ((4. / 3.) * M_PI);
    }
    std::cout << "initial condition:" << *initial_condition << std::endl; //MDG aggiorna la condizione iniziale in modo che corrisponde a quella passata al time step successivo. 

    std::cout << "result:" << solution << std::endl; //MDG
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

using namespace H5;
using namespace std;

double Solver::ROM_cylinder(double *initial_condition, std::vector<double> parameter, double increment)
{

    hsize_t rows = 0;
    hsize_t cols = 0; // Variabili per le dimensioni della matrice

    try {
        //////////////////////////////   MATRICI  //////////////////////////////
        // Apriamo il file HDF5 in modalità lettura
        H5File file("/Users/martina/Library/CloudStorage/OneDrive-PolitecnicodiMilano/PhD/Git/rom-cylinder_DEIM-POD/offline-online stages/fit White/2. DEIM-POD/matrici_RB.h5", H5F_ACC_RDONLY);

        // Accediamo ai dataset
        DataSet dataset_MM_RB = file.openDataSet("MM_RB"); //N_epsilonxN_epsilon
        DataSet dataset_FF_RB = file.openDataSet("FF_RB"); //N_epsilonx1
        DataSet dataset_KK_RB = file.openDataSet("KK_RB"); // N_D+N_DxN_epsilonxN_epsilon
        DataSet dataset_AA_RB = file.openDataSet("AA_RB"); // N_epsilon
        DataSet dataset_ZZ_CO = file.openDataSet("ZZ_CO"); // N_D
        DataSet dataset_II_CO = file.openDataSet("II_CO"); // N_DxN_D


        // Funzione per ottenere e stampare le dimensioni di un dataset
        auto printDimensions = [](DataSet &dataset, const std::string &name) {
            DataSpace dataspace = dataset.getSpace();
            hsize_t dims[3]; // Supporta fino a 3 dimensioni
            int ndims = dataspace.getSimpleExtentNdims();
            
            if (ndims > 0) {
                dataspace.getSimpleExtentDims(dims, NULL); // Ottieni le dimensioni
                std::cout << "Dimensioni di " << name << ": ";
                for (int i = 0; i < ndims; ++i) {
                    std::cout << dims[i]; // Stampa ciascuna dimensione
                    if (i < ndims - 1) std::cout << " x "; // Aggiungi "x" tra le dimensioni
                }
                std::cout << std::endl;
            } else {
                std::cout << "Il dataset " << name << " non ha dimensioni." << std::endl;
            }
        };

        // Stampa le dimensioni di ciascun dataset
        /*cout << "Dimensioni dei dataset:" << endl;
        printDimensions(dataset_MM_RB, "MM_RB"); //N_epsilonxN_epsilon
        printDimensions(dataset_FF_RB, "FF_RB"); //N_epsilonx1
        printDimensions(dataset_KK_RB, "KK_RB"); // N_D+N_D xN_epsilonxN_epsilon
        printDimensions(dataset_AA_RB, "AA_RB"); // N_epsilon
        printDimensions(dataset_ZZ_CO, "ZZ_CO"); // N_D
        printDimensions(dataset_II_CO, "II_CO"); // N_DxN_D
        */

        // MM_RB - N_epsilonxN_epsilon
        DataSpace dataspace_MM_RB = dataset_MM_RB.getSpace();
        hsize_t dims_MM_RB[2];
        dataspace_MM_RB.getSimpleExtentDims(dims_MM_RB, NULL);
        Eigen::MatrixXd MM_RB(dims_MM_RB[0], dims_MM_RB[1]);
        dataset_MM_RB.read(MM_RB.data(), PredType::NATIVE_DOUBLE); 
        MM_RB.transposeInPlace();  //L'ho dovuta trasporre per averla come in python
        //std::cout << "MM_RB (0,1): " << MM_RB(0,1) << std::endl;
        //std::cout << "MM_RB (1,0): " << MM_RB(1,0) << std::endl;
        //std::cout << "MM_RB (1,1): " << MM_RB(1,1) << std::endl;

        // FF_RB - N_epsilon
        DataSpace dataspace_FF_RB = dataset_FF_RB.getSpace();
        hsize_t dims_FF_RB[2];
        dataspace_FF_RB.getSimpleExtentDims(dims_FF_RB, NULL);
        Eigen::MatrixXd FF_RB(dims_FF_RB[0], dims_FF_RB[1]);
        dataset_FF_RB.read(FF_RB.data(), PredType::NATIVE_DOUBLE);
        //std::cout << "FF_RB (0): " << FF_RB(0) << std::endl;
        //std::cout << "FF_RB (1): " << FF_RB(1) << std::endl;
        //std::cout << "FF_RB (2): " << FF_RB(2) << std::endl;
        //std::cout << "FF_RB (3): " << FF_RB(3) << std::endl;

        // KK_RB - N_D+N_D x N_epsilon x N_epsilon
        DataSpace dataspace_KK_RB = dataset_KK_RB.getSpace();
        hsize_t dims_KK_RB[3];
        dataspace_KK_RB.getSimpleExtentDims(dims_KK_RB, NULL);
        std::vector<double> buffer(dims_KK_RB[0] * dims_KK_RB[1] * dims_KK_RB[2]);
        dataset_KK_RB.read(buffer.data(), PredType::NATIVE_DOUBLE);
        Eigen::Tensor<double, 3> KK_RB(static_cast<Eigen::Index>(dims_KK_RB[0]),
                                static_cast<Eigen::Index>(dims_KK_RB[1]),
                                static_cast<Eigen::Index>(dims_KK_RB[2]));

        for (int i = 0; i < dims_KK_RB[0]; ++i) {
            for (int j = 0; j < dims_KK_RB[1]; ++j) {
                for (int k = 0; k < dims_KK_RB[2]; ++k) {
                    KK_RB(i, j, k) = buffer[i * dims_KK_RB[1] * dims_KK_RB[2] + j * dims_KK_RB[2] + k];
                }
            }
        }
        /*for (int i = 0; i < dims_KK_RB[0]; ++i) {
        for (int j = 0; j < dims_KK_RB[1]; ++j) {
            for (int k = 0; k < dims_KK_RB[2]; ++k) {
                std::cout << "KK_RB (" << i << ", " << j << ", " << k << "): " << KK_RB(i, j, k) << std::endl;
            }
        }
        }*/ // -> Corretto



        // AA_RB - N_epsilon
        DataSpace dataspace_AA_RB = dataset_AA_RB.getSpace();
        hsize_t dims_AA_RB[1];
        dataspace_AA_RB.getSimpleExtentDims(dims_AA_RB, NULL);
        Eigen::VectorXd AA_RB(dims_AA_RB[0]); 
        dataset_AA_RB.read(AA_RB.data(), PredType::NATIVE_DOUBLE);
        /*for (int i = 0; i < dims_AA_RB[0]; ++i) {
        std::cout << "AA_RB (" << i << "): " << AA_RB(i) << std::endl;
        }*/

        //ZZ_CO - N_D 
        DataSpace dataspace_ZZ_CO = dataset_ZZ_CO.getSpace();
        hsize_t dims_ZZ_CO[1];
        dataspace_ZZ_CO.getSimpleExtentDims(dims_ZZ_CO, NULL);
        Eigen::VectorXd ZZ_CO(dims_ZZ_CO[0]);
        dataset_ZZ_CO.read(ZZ_CO.data(), PredType::NATIVE_DOUBLE);
        /*for (int i = 0; i < dims_ZZ_CO[0]; ++i) {
        std::cout << "ZZ_CO (" << i << "): " << ZZ_CO(i) << std::endl;
        }*/ //Corretto

        //II_CO - N_DxN_D
        DataSpace dataspace_II_CO = dataset_II_CO.getSpace();
        hsize_t dims_II_CO[2];
        dataspace_II_CO.getSimpleExtentDims(dims_II_CO, NULL);
        Eigen::MatrixXd II_CO(dims_II_CO[0], dims_II_CO[1]);
        dataset_II_CO.read(II_CO.data(), PredType::NATIVE_DOUBLE);
        II_CO.transposeInPlace();
        /*for (int i = 0; i < II_CO.rows(); ++i) {
        for (int j = 0; j < II_CO.cols(); ++j) {
            std::cout << "II_CO (" << i << ", " << j << "): " << II_CO(i, j) << std::endl;
        }
        }*/ //Corretto


        // Visualizza le dimensioni delle matrici e dei vettori
        /*cout << "Dimensioni delle matrici create:" << endl;
        cout << "Dimensioni di MM_RB: " << MM_RB.rows() << " x " << MM_RB.cols() << endl;
        cout << "Dimensioni di FF_RB: " << FF_RB.rows() << " x " << FF_RB.cols() << endl;
        cout << "Dimensioni di KK_RB: " 
             << KK_RB.dimension(0) << " x " 
             << KK_RB.dimension(1) << " x " 
             << KK_RB.dimension(2) << endl;
        cout << "Dimensioni di AA_RB: " << AA_RB.size() << endl; 
        cout << "Dimensioni di ZZ_CO: " << ZZ_CO.size() << endl;  
        cout << "Dimensioni di II_CO: " << II_CO.rows() << " x " << II_CO.cols() << endl;*/
        

        // Chiudiamo i dataset e il file
        dataset_MM_RB.close();
        dataset_FF_RB.close();
        dataset_KK_RB.close();
        dataset_AA_RB.close();
        dataset_ZZ_CO.close();
        dataset_II_CO.close();
        file.close();

        
        //////////////////////////////   PARAMETRI  //////////////////////////////
        int N_D = ZZ_CO.size(); 
        int N_epsilon = AA_RB.size(); 
        double RADIUS = parameter.at(2); 
        double LENGTH = parameter.at(4);
        double SOURCE_C = parameter.at(3);
        double ALPHA_T =  parameter.at(5); 
        double SOURCE_T= parameter.at(6);
        double Tbc = parameter.at(7);
        double fission_rate = parameter.at(8);
        Eigen::VectorXd TT(ZZ_CO.size()); 
        double boltzmann_constant = 1.380651e-23; 
        TT = Tbc + (SOURCE_T * LENGTH * LENGTH / ALPHA_T) * (1 - ZZ_CO.array().square()) / 2; 
        /*for (int i = 0; i < TT.rows(); ++i) {
            std::cout << "TT (" << i << "): " << TT(i) << std::endl;
        }*/ // -> Corretto

        // Diffusion coefficient
        Eigen::VectorXd WW(N_D); // N_Dx1 vector
        //double gasDiffusivity = system.getFissionGasDiffusivity(); 

        for (int i = 0; i < TT.size(); ++i) {
            double temperature = TT(i);

            // Diffusivity Turnbull
            //double d1 = 7.6e-10 * exp(-4.86e-19 / (boltzmann_constant * temperature));
            //double d2 = 4.0 * 1.41e-25 * sqrt(fission_rate) * exp(-1.91e-19 / (boltzmann_constant * temperature));
            //double d3 = 8.0e-40 * fission_rate;
            //double diffusivity = d1 + d2 + d3;

            //Diffusivity fit White
            double diffusivity = 2.949513e-13 * exp(-20487.36244 / temperature); //fit White. 

            // Assegna il valore di diffusivity a WW
            WW(i) = diffusivity;
        }
        //std::cout << "WW: " << WW.transpose() << std::endl; //-> Corretto

        //WW << 1.03483679e-16, 2.13899491e-17, 5.64282052e-17, 3.34541840e-17, 8.48893704e-17, 2.54543157e-17;
        
        // CALCOLO CC_CO: 
        Eigen::MatrixXd temp1 = (II_CO * WW) / (RADIUS * RADIUS);
        Eigen::MatrixXd temp2 = (II_CO * WW) / (LENGTH * LENGTH); 
        Eigen::MatrixXd CC_CO = Eigen::MatrixXd::Zero(temp1.rows() + temp2.rows(), temp1.cols());
        CC_CO << temp1, temp2;
        //std::cout << "Dimensions of CC_CO: " << CC_CO.rows() << "x" << CC_CO.cols() << std::endl;
        //std::cout << "CC_CO:" << std::endl;
        /*for (int i = 0; i < CC_CO.rows(); ++i) {
            std::cout << "CC_CO (" << i << "): " << CC_CO(i) << std::endl;
        }*/ // -> Corretto

        //CALCOLO SS_RB: 
        //  Costruisco per CC_CO un tensore 2D
        Eigen::Tensor<double, 2> CC_CO_tensor(CC_CO.rows(), CC_CO.cols());
        for (int i = 0; i < CC_CO.rows(); ++i) {
            for (int j = 0; j < CC_CO.cols(); ++j) {
                CC_CO_tensor(i, j) = CC_CO(i, j);
            }
        }
        //std::cout << "CC_CO_tensor values:" << std::endl;
        /*for (int i = 0; i < CC_CO_tensor.dimension(0); ++i) {
            for (int j = 0; j < CC_CO_tensor.dimension(1); ++j) {
                std::cout << "CC_CO_tensor(" << i << ", " << j << "): " 
                        << CC_CO_tensor(i, j) << std::endl;
            }
        }*/ // -> Corretto
        //  Costruisco il tensore 3D per SS_RB: 
        Eigen::Tensor<double, 3> SS_RB = CC_CO_tensor.contract(KK_RB, Eigen::array<Eigen::IndexPair<int>, 1>{{Eigen::IndexPair<int>(0, 0)}});
        //std::cout << "Dimensions of SS_RB: " << SS_RB.dimension(0) << "x" << SS_RB.dimension(1) << "x" << SS_RB.dimension(2) << std::endl;
        /*for (int i = 0; i < SS_RB.dimension(0); ++i) {
        for (int j = 0; j < SS_RB.dimension(1); ++j) {
            for (int k = 0; k < SS_RB.dimension(2); ++k) {
                std::cout << "SS_RB(" << i << ", " << j << ", " << k << "): " 
                        << SS_RB(i, j, k) << std::endl;
            }
        }
        }*/ // -> Corretto

        //CALCOLO QQ_RB: 
        Eigen::VectorXd QQ_RB = SOURCE_C * FF_RB;
        /*for (int i = 0; i < QQ_RB.rows(); ++i) {
            std::cout << "QQ_RB (" << i << "): " << QQ_RB(i) << std::endl;
        }*/ //Corretto

        //LHS and RHS: 
        Eigen::MatrixXd SS_RB_matrix(SS_RB.dimension(0) * SS_RB.dimension(1), SS_RB.dimension(2));
        for (int i = 0; i < SS_RB.dimension(0); ++i) {
            for (int j = 0; j < SS_RB.dimension(1); ++j) {
                for (int k = 0; k < SS_RB.dimension(2); ++k) {
                    SS_RB_matrix(i * SS_RB.dimension(1) + j, k) = SS_RB(i, j, k);
                }
            }
        }
        /*for (int i = 0; i < SS_RB_matrix.rows(); ++i) {
        for (int j = 0; j < SS_RB_matrix.cols(); ++j) {
            std::cout << "SS_RB_matrix (" << i << ", " << j << "): " << SS_RB_matrix(i, j) << std::endl;
        }
        }*/ // -> Corretto

        Eigen::MatrixXd LHS_RB_static = MM_RB + increment * SS_RB_matrix;
        Eigen::VectorXd RHS_RB_static = increment * QQ_RB;
        //std::cout << "Dimensioni di LHS_RB_static: " << LHS_RB_static.rows() << " x " << LHS_RB_static.cols() << std::endl;
        //std::cout << "Dimensioni di RHS_RB_static: " << RHS_RB_static.size() << std::endl;
        /*for (int i = 0; i < LHS_RB_static.rows(); ++i) {
        for (int j = 0; j < LHS_RB_static.cols(); ++j) {
            std::cout << "LHS_RB_static (" << i << ", " << j << "): " << LHS_RB_static(i, j) << std::endl;
        }
        }*/ // -> Corretto
        /*for (int i = 0; i < RHS_RB_static.size(); ++i) {
            std::cout << "RHS_RB_static (" << i << "): " << RHS_RB_static(i) << std::endl;
        }*/ //-> Corretto

        //////////////////////////////   SOLVER  //////////////////////////////
        /* // Solver che c'è su python 
        Eigen::VectorXd reconstructed_solution(1000); 
        for (int ii = 0; ii < 1000; ii++) {
        // Solve the linear system
        old_sol_RB = LHS_RB_static.colPivHouseholderQr().solve(RHS_RB_static + MM_RB * old_sol_RB);
        double value = (AA_RB.transpose() * old_sol_RB)(0, 0);
        reconstructed_solution(ii) = value;
        std::cout << reconstructed_solution(ii) << std::endl;
        }*/ //Il risultato combacia con python

        int n = LHS_RB_static.rows();
        Eigen::VectorXd initial_condition_vec(n); //Devo per forza creare questo vettore, non posso usare initial_condition[].  
        //  Riempio il vettore
        for (int i = 0; i < n; ++i) {
            initial_condition_vec(i) = initial_condition[i];
        }

        initial_condition_vec = LHS_RB_static.colPivHouseholderQr().solve(RHS_RB_static + MM_RB * initial_condition_vec);
        double value = (AA_RB.transpose() * initial_condition_vec)(0, 0);
        double reconstructed_solution = value;
        std::cout << "reconstructed solution: " << reconstructed_solution << std::endl;
        //  Riempio initial condition per il time step successivo. 
        for (int i = 0; i < n; ++i) {
            initial_condition[i] = initial_condition_vec(i);
        }

        return reconstructed_solution;

    } 

    //Senza questo restituisce errore. 
    catch (Exception &error) {
    error.printErrorStack();
    std::cerr << "Errore durante l'accesso ai dati HDF5!" << std::endl;
    }


}


