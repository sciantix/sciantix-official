#include <pybind11/pybind11.h>
#include "mainBind.h"

namespace py = pybind11;

/**
 * This is the PYBIND11_MODULE for exporting C/C++ classes as a Python module.
 * @param sciantixModule The name of the module.
 * @param m The module reference.
 * @return This function returns the Python module.
 */
PYBIND11_MODULE(sciantixModule, m) {

    //--- imports all the attributes used --- // 
    init_attributes(m);
    
    //--- imports all the functions used --- //
    init_functions(m);

    //--- imports all the classes used --- //
    init_classes(m);
}
