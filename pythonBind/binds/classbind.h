#ifndef CLASSBIND_H
#define CLASSBIND_H

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/numpy.h>

#include "SciantixVariable.h"
#include "InputVariable.h"
#include "System.h"
#include "Model.h"
#include "Matrix.h"
#include "Material.h"
#include "Solver.h"
#include "Simulation.h"
#include "SciantixArray.h"

namespace py = pybind11;

/**
 * This function is used to import all the classes used for the pybind11 module
 * @param module
 */
void init_classes(py::module_ &m);

#endif
