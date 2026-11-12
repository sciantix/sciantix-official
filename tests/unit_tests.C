//////////////////////////////////////////////////////////////////////////////////////
//       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
//      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
//     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
//      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
//  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
//  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
//                                                                                  //
//  Unit tests for the Solver integrators and the SciantixArray container.         //
//  Run via CTest: ctest --test-dir build                                           //
//                                                                                  //
//////////////////////////////////////////////////////////////////////////////////////

#include "InputVariable.h"
#include "SciantixArray.h"
#include "Solver.h"

#include <cmath>
#include <iostream>
#include <string>
#include <vector>

static int failures = 0;

static void checkClose(const std::string& name, double value, double expected, double tol)
{
    if (std::isnan(value) || std::fabs(value - expected) > tol)
    {
        std::cerr << "FAIL: " << name << " = " << value << ", expected " << expected << " (tol " << tol << ")"
                  << std::endl;
        failures++;
    }
    else
        std::cout << "ok:   " << name << std::endl;
}

static void checkTrue(const std::string& name, bool condition)
{
    if (!condition)
    {
        std::cerr << "FAIL: " << name << std::endl;
        failures++;
    }
    else
        std::cout << "ok:   " << name << std::endl;
}

static void testSolver()
{
    Solver solver;

    // y' = S  ->  y0 + S dt
    checkClose("Integrator", solver.Integrator(2.0, 3.0, 0.5), 3.5, 1e-12);

    // y' = -L y + S  ->  (y0 + S dt) / (1 + L dt)
    checkClose("Decay", solver.Decay(1.0, 2.0, 4.0, 0.5), 1.5, 1e-12);
    // L = 0 reduces to plain integration
    checkClose("Decay (L=0)", solver.Decay(1.0, 0.0, 4.0, 0.5), 3.0, 1e-12);

    // y' = -k y^2  ->  y0 / (1 + k y0 dt)
    checkClose("BinaryInteraction", solver.BinaryInteraction(10.0, 0.1, 1.0), 5.0, 1e-12);

    // y' = k/y + S, semi-implicit quadratic solution
    {
        double y0 = 1.0, k = 2.0, S = 3.0, dt = 0.5;
        double expected = 0.5 * ((y0 + S * dt) + std::sqrt(std::pow(y0 + S * dt, 2) + 4.0 * k * dt));
        checkClose("LimitedGrowth", solver.LimitedGrowth(y0, {k, S}, dt), expected, 1e-12);
    }

    // Cramer 2x2: [2 1; 1 3] x = [5; 10] -> x = [1; 3]
    {
        double A[4] = {2.0, 1.0, 1.0, 3.0};
        double b[2] = {5.0, 10.0};
        solver.Laplace2x2(A, b);
        checkClose("Laplace2x2 x0", b[0], 1.0, 1e-12);
        checkClose("Laplace2x2 x1", b[1], 3.0, 1e-12);
    }

    // Cramer NxN against the same 2x2 system
    {
        double A[4] = {2.0, 1.0, 1.0, 3.0};
        double b[2] = {5.0, 10.0};
        solver.Laplace(2, A, b);
        checkClose("Laplace(N=2) x0", b[0], 1.0, 1e-12);
        checkClose("Laplace(N=2) x1", b[1], 3.0, 1e-12);
    }

    {
        double A[4] = {2.0, 1.0, 1.0, 3.0};
        checkClose("det 2x2", solver.det(2, A), 5.0, 1e-12);
    }

    {
        std::vector<double> u    = {1.0, 2.0, 3.0};
        double              v[3] = {4.0, 5.0, 6.0};
        checkClose("dotProduct1D", solver.dotProduct1D(u, v, 3), 32.0, 1e-12);
    }

    // x^4 - 16 = 0, Newton from x = 1.5 -> x = 2
    checkClose("QuarticEquation", solver.QuarticEquation({1.5, 1.0, 0.0, 0.0, 0.0, -16.0}), 2.0, 1e-3);

    // Spectral diffusion: no production, zero initial modes -> identically zero
    {
        const int           n_modes        = 40;
        double              modes[n_modes] = {0.0};
        std::vector<double> parameter      = {static_cast<double>(n_modes), 1e-19, 5e-6, 0.0, 0.0};
        checkClose("SpectralDiffusion (S=0)", solver.SpectralDiffusion(modes, parameter, 3600.0), 0.0, 1e-25);
    }

    // Spectral diffusion: with production the spatially averaged concentration is positive
    {
        const int           n_modes        = 40;
        double              modes[n_modes] = {0.0};
        std::vector<double> parameter      = {static_cast<double>(n_modes), 1e-19, 5e-6, 1e18, 0.0};
        checkTrue("SpectralDiffusion (S>0) positive", solver.SpectralDiffusion(modes, parameter, 3600.0) > 0.0);
    }
}

static void testSciantixArray()
{
    SciantixArray<InputVariable> array;

    checkTrue("SciantixArray empty()", array.empty());

    InputVariable a;
    a.setName("alpha");
    a.setValue(1.0);
    array.push(a);

    InputVariable b;
    b.setName("beta");
    b.setValue(2.0);
    array.push(b);

    checkTrue("SciantixArray isElementPresent", array.isElementPresent("alpha"));
    checkTrue("SciantixArray !isElementPresent", !array.isElementPresent("gamma"));
    checkClose("SciantixArray lookup by name", array["beta"].getValue(), 2.0, 1e-12);
    checkClose("SciantixArray lookup by index", array[0].getValue(), 1.0, 1e-12);

    // push() with an existing name replaces the element in place
    InputVariable a2;
    a2.setName("alpha");
    a2.setValue(10.0);
    array.push(a2);
    checkClose("SciantixArray push replaces", array["alpha"].getValue(), 10.0, 1e-12);

    array.clear();
    checkTrue("SciantixArray clear()", array.empty());
}

int main()
{
    testSolver();
    testSciantixArray();

    if (failures > 0)
    {
        std::cerr << failures << " test(s) failed." << std::endl;
        return 1;
    }
    std::cout << "All unit tests passed." << std::endl;
    return 0;
}
