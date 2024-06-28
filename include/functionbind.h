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

#include "HistoryVariableDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "SystemDeclaration.h"
#include "PhysicsVariableDeclaration.h"
#include "ModelDeclaration.h"
#include "MaterialDeclaration.h"
#include "GasDeclaration.h"
#include "MatrixDeclaration.h"
#include "Global.h"
#include <vector>

namespace py = pybind11;

void init_functions(py::module_ &m);

#endif 
