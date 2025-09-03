#include <cmath>
#include <iostream>
#include <string>

#include "Matrix.h"

void Matrix::setPoreTrappingRate(SciantixArray<SciantixVariable> &sciantix_variable, SciantixArray<SciantixVariable> &history_variable,SciantixArray<Matrix> &matrices) {
    // Source: Inconnue
    std::string reference = "";
    reference += "Inconnue";
    // Units:

    PoreTrappingRate =4.0*M_PI*GrainBoundaryVacancyDiffusivity*sciantix_variable["Xe at grain boundary"].getFinalValue()*sciantix_variable["HBS pore radius"].getFinalValue()*(1.0+1.8*pow(sciantix_variable["HBS porosity"].getFinalValue(),1.3));
}

