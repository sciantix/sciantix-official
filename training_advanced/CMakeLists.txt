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
#//  Authors: G. Zullo (giovanni.zullo@polimi.it)                                    //
#//                                                                                  //
#//////////////////////////////////////////////////////////////////////////////////////

# This CMakeLists.txt file is useful when compiling SCIANTIX with the cmake tool
# 
# output:
#   > build directory with executable in build/Debug/sciantix.exe
#
# When using IDE, a CMakeSettings.json could be automatically produced to compile sciantix in a locall out/build/ folder
# by setting this file as startup element.
# More information here: (https://learn.microsoft.com/it-it/cpp/build/configure-cmake-debugging-sessions?view=msvc-170)

cmake_minimum_required(VERSION 3.0.0)

# Name of the project created / existent in visual studio
project(sciantix)

# Enable C++ compiler
enable_language (CXX)

# Specify SCIANTIX directories
set(include "include/*.h")
set(src "src/*.C")

include(CTest)
enable_testing()

include_directories(
    ${CMAKE_SOURCE_DIR}/include/
)

file(GLOB SOURCES ${include} ${src})
add_executable(sciantix ${SOURCES})

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})

include(CPack)