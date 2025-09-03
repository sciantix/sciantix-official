#include <cmath>
#include <iostream>
#include <string>

#include "Matrix.h"

void Matrix::setGrainBoundaryVacancyDiffusivity(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices) {
    // Source: Reynolds and Burton, JNM, 82 (1979) 22-25
    std::string reference = "";
    reference += "Reynolds and Burton, JNM, 82 (1979) 22-25";
    // Units:
    //   A: m²/s
    //   B: J
    //   GrainBoundaryVacancyDiffusivity: m²/s
    //   T: K
    const double A = 6.9e-04;
    const double B = 5.35e-19;

    GrainBoundaryVacancyDiffusivity =A*exp(((-B)/(boltzmann_constant*history_variable["Temperature"].getFinalValue())));
}

