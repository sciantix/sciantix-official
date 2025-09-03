#include <cmath>
#include <iostream>
#include <string>

#include "Matrix.h"

void Matrix::setPoreNucleationRate(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices) {
    // Source: Barani et al., JNM 563 (2022) 153627
    std::string reference = "";
    reference += "Barani et al., JNM 563 (2022) 153627";
    // Units:
    //   T: K
    const double A = 5.0e17;
    const double B = 2.77e-7;
    const double C = 3.54;
    const double D = 2.54;
    const double E = 1.25e-6;

    PoreNucleationRate =(A*B*C*(1-sciantix_variable["Restructured volume fraction"].getFinalValue())*pow(sciantix_variable["Effective burnup"].getFinalValue(),D))*E;
}

