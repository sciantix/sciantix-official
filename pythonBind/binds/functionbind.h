#ifndef FUNCTIONBIND_H
#define FUNCTIONBIND_H

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/numpy.h>

#include "Sciantix.h"
#include "InputInterpolation.h"
#include "InputReading.h"
#include "Initialization.h"
#include "TimeStepCalculation.h"
#include "MainVariables.h"
#include "Simulation.h"

#include <vector>

namespace py = pybind11;

void init_functions(py::module_ &m);

// --- Function declarations --- //
py::array_t<double> getVariablesInArray_double();
py::array_t<double> getHistoryInArray_double();
py::array_t<double> getDiffusionModesInArray_double();
py::array_t<double> getScalingFactorsInArray_double();
py::array_t<int> getOptionsInArray_int();

#endif 
