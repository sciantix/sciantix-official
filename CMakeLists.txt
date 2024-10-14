# This CMakeLists.txt file is useful when compiling SCIANTIX with the cmake tool
#
# output:
#   > build directory with executable in build/sciantix.x
#   > pythonBind directory with sciantixModule (python binding option)
#
# Author: G. Zullo (giovanni.zullo@polimi.it)

cmake_minimum_required(VERSION 3.14)

project(sciantix)

set(pybind11_DIR "/home/giovanni/.local/lib/python3.10/site-packages/pybind11/share/cmake/pybind11")

find_package(pybind11 REQUIRED)

function(add_subdirectory_includes root_dir)
    file(GLOB SUBDIRS RELATIVE ${root_dir} ${root_dir}/*)
    foreach(subdir ${SUBDIRS})
        if(IS_DIRECTORY ${root_dir}/${subdir})
            include_directories(${root_dir}/${subdir})
            add_subdirectory_includes(${root_dir}/${subdir})
        endif()
    endforeach()
endfunction()

include_directories(${PROJECT_SOURCE_DIR}/include)

add_subdirectory_includes(${PROJECT_SOURCE_DIR}/include)

file(GLOB_RECURSE SOURCES ${PROJECT_SOURCE_DIR}/src/*.C)

add_library(sciantix SHARED ${SOURCES})

pybind11_add_module(sciantixModule pythonBind/binds/attributesbind.cpp pythonBind/binds/classbind.cpp pythonBind/binds/functionbind.cpp pythonBind/binds/mainBind.cpp)

target_link_libraries(sciantixModule PRIVATE sciantix)

set_target_properties(sciantixModule PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/python
    OUTPUT_NAME "sciantixModule"
)

set_property(TARGET sciantix PROPERTY CXX_STANDARD 17)

add_executable(sciantix.x ${SOURCES})

target_link_libraries(sciantix.x PRIVATE sciantix)

set_target_properties(sciantix.x PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}
)