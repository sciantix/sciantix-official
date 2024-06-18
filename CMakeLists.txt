#//////////////////////////////////////////////////////////////////////////////////////
#//       _______.  ______  __       ___      .__   __. .___________. __  ___   ___  //
#//      /       | /      ||  |     /   \     |  \ |  | |           ||  | \  \ /  /  //
#//     |   (----`|  ,----'|  |    /  ^  \    |   \|  | `---|  |----`|  |  \  V  /   //
#//      \   \    |  |     |  |   /  /_\  \   |  . `  |     |  |     |  |   >   <    //
#//  .----)   |   |  `----.|  |  /  _____  \  |  |\   |     |  |     |  |  /  .  \   //
#//  |_______/     \______||__| /__/     \__\ |__| \__|     |__|     |__| /__/ \__\  //
#//                                                                                  //
#//  Originally developed by D. Pizzocri & T. Barani                                 //
#//                                                                                  //
#//  Version: 2.0                                                                    //
#//  Year: 2022                                                                      //
#//  Authors:                                                                        //
#//                                                                                  //
#//////////////////////////////////////////////////////////////////////////////////////

# CMake version
cmake_minimum_required(VERSION 3.14)

# name of the project
project(sciantix)

# Ensure that CMake can find Pybind11
find_package(pybind11 REQUIRED)

# Include directories for your project
include_directories(include)

# Add source files
file(GLOB SOURCES "src/*.C")
file(GLOB HEADERS "include/*.h" "include/*.hpp")

# Add the library target for your project
add_library(sciantix SHARED ${SOURCES})

# Add the pybind11 module target
pybind11_add_module(sciantixModule pythonBind/Bindings.cpp)

# Link the main library with the pybind11 module
target_link_libraries(sciantixModule PRIVATE sciantix)

# Set the output directory for the compiled module
set_target_properties(sciantixModule PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/python # Name of the folder
    OUTPUT_NAME "sciantixModule"  # Name of the Outputed Module
)

# Specify C++ standard
set_property(TARGET sciantix PROPERTY CXX_STANDARD 17)
