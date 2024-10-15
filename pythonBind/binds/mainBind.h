#include "attributesbind.h"
#include "classbind.h"
#include "functionbind.h"

#include "InputReading.h"

void bind_get_history_variable(py::module_ &m);
void bind_get_variables(py::module_ &m);
void bind_get_diffusion_modes(py::module_ &m);
void bind_get_scaling_factors(py::module_ &m);
void bind_get_options(py::module_ &m);
