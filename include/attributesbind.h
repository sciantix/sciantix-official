#ifndef ATTRIBUTESBIND_H
#define ATTRIBUTESBIND_H

#include <pybind11/pybind11.h>
#include <pybind11/stl.h>
#include <pybind11/numpy.h>

#include "MainVariables.h"
#include "HistoryVariableDeclaration.h"
#include "SciantixVariableDeclaration.h"
#include "SystemDeclaration.h"
#include "PhysicsVariableDeclaration.h"
#include "ModelDeclaration.h"
#include "MaterialDeclaration.h"
#include "GasDeclaration.h"
#include "MatrixDeclaration.h"


namespace py = pybind11;

/**
 * This function is used to import all the attributes used for the pybind11 module
 * @param module
 */
void init_attributes(py::module_ &m);

#endif 
