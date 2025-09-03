#include <cmath>
#include <iostream>
#include <string>

#include "Matrix.h"

void Matrix::setPoreResolutionRate(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices) {
    // Source: Inconnue
    std::string reference = "";
    reference += "Inconnue";
    // Units:
    //   HBS: K
    const double A = 1.0e-9;
    const double B = 2.0e-23;

    PoreResolutionRate =(1-pow((exp((sciantix_variable["HBS pore radius"].getFinalValue()/(9*A)))),3))*B*history_variable["Fission rate"].getFinalValue()*((3*A)/(3*A+sciantix_variable["HBS pore radius"].getFinalValue()))*(A/(A*sciantix_variable["HBS pore radius"].getFinalValue()));
}

