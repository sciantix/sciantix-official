#include <cmath>
#include <iostream>
#include <string>

#include "Matrix.h"

void Matrix::setGrainBoundaryMobility(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices) {
    // Source: Ainscough et al., JNM, 49 (1973) 117-128
    std::string reference = "";
    reference += "Ainscough et al., JNM, 49 (1973) 117-128";
    // Units:
    //   T: K
    const double A = 1.455e-8;
    const double B = 32114.5;

    GrainBoundaryMobility =A*exp(((-B)/history_variable["Temperature"].getFinalValue()));
}

