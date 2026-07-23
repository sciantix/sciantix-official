# This CMakeLists.txt file is useful when compiling SCIANTIX with the cmake tool
#
# output:
#   > build directory with executable in build/sciantix.x
#
# When using IDE, a CMakeSettings.json could be automatically produced to compile sciantix in a locall out/build/ folder
# by setting this file as startup element.
# More information here: (https://learn.microsoft.com/it-it/cpp/build/configure-cmake-debugging-sessions?view=msvc-170)
#
# Author: G. Zullo (giovanni.zullo@polimi.it), E. Cappellari (elisa.cappellari@polimi.it)

cmake_minimum_required(VERSION 3.5.0)

if(APPLE)
    message(STATUS "Configuring on macOS, setting custom compilers...")
    set(CMAKE_C_COMPILER /opt/homebrew/bin/gcc-14)
    set(CMAKE_CXX_COMPILER /opt/homebrew/bin/g++-14)
else()
    message(STATUS "Not on macOS, using default compilers.")
endif()

# Name of the project created / existent in visual studio
project(sciantix)

# Function to add all subdirectories of a given path to include directories
function(add_subdirectory_includes root_dir)
    file(GLOB SUBDIRS RELATIVE ${root_dir} ${root_dir}/*) # get all files in directory (which is passed in parameter)
    foreach(subdir ${SUBDIRS})
        if(IS_DIRECTORY ${root_dir}/${subdir}) # if file is a directory
            include_directories(${root_dir}/${subdir})  # include the directory as header directory
            add_subdirectory_includes(${root_dir}/${subdir})    # exectute the function for the subdirectory (to get all subdirectories of it if any)
        endif()
    endforeach()
endfunction()

# Enable C++ compiler
enable_language (CXX)

# Language standard and warnings
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options(-Wall -Wextra)
endif()

# Set option for coupling with TU
option (COUPLING_TU OFF)

# Optional coupling with OpenCalphad:
# auto-detected if a "opencalphad-for-sciantix" directory has been built with Allmake_OC.sh next to this repository, e.g.
#   workspace/
#   |-- sciantix-official/
#   `-- opencalphad-for-sciantix/
# Override with -DCOUPLING_OC=ON/OFF to force it on or off regardless of detection.
if(EXISTS "${CMAKE_SOURCE_DIR}/../opencalphad-for-sciantix/build/liboctq-isoc.a" AND
   EXISTS "${CMAKE_SOURCE_DIR}/../opencalphad-for-sciantix/build/liboctq-isoc-gb.a")
    set(COUPLING_OC_DEFAULT ON)
else()
    set(COUPLING_OC_DEFAULT OFF)
endif()
option(COUPLING_OC "Enable OpenCalphad coupling" ${COUPLING_OC_DEFAULT})
if(COUPLING_OC)
    message(STATUS "OpenCalphad coupling enabled")
else()
    message(STATUS "OpenCalphad coupling disabled (built ../opencalphad-for-sciantix libraries not found and COUPLING_OC not forced ON)")
endif()

# Include CTest
include(CTest)
enable_testing()

# Include header files
include_directories(${PROJECT_SOURCE_DIR}/include)
# Add all subdirectories of the include folder to the include path
add_subdirectory_includes(${PROJECT_SOURCE_DIR}/include)

# Include source files
file(GLOB_RECURSE SOURCES ${PROJECT_SOURCE_DIR}/src/*.C)

# OCUtilsCoupling.C links directly against OpenCalphad's compiled OCASI libraries.
if(NOT COUPLING_OC)
    list(REMOVE_ITEM SOURCES ${PROJECT_SOURCE_DIR}/src/coupling/OCUtilsCoupling.C)
endif()

# Set custom object file directory (to have all object files in the obj directory)
set(CMAKE_OBJECT_PATH_PREFIX ${PROJECT_SOURCE_DIR}/obj/)

# Add executable sciantix.x (or .exe) or create static library
if(COUPLING_TU)
    add_library(sciantix STATIC ${SOURCES})
    set(CMAKE_STATIC_LIBRARY_SUFFIX ".a")
    if(COUPLING_OC)
        target_compile_definitions(sciantix PRIVATE USE_OPENCALPHAD)
    endif()
else()
    add_executable(sciantix ${SOURCES})
    if(COUPLING_OC)
        target_compile_definitions(sciantix PRIVATE USE_OPENCALPHAD)
        target_link_libraries(sciantix
            ${PROJECT_SOURCE_DIR}/../opencalphad-for-sciantix/build/liboctq-isoc.a
            ${PROJECT_SOURCE_DIR}/../opencalphad-for-sciantix/build/liboctq-isoc-gb.a
            gfortran quadmath gomp)
    endif()
    if (UNIX)
        set(CMAKE_EXECUTABLE_SUFFIX ".x")
    endif()
endif()

# Unit tests (run with: ctest --test-dir build)
if(BUILD_TESTING AND NOT COUPLING_TU)
    add_executable(unit_tests
        ${PROJECT_SOURCE_DIR}/tests/unit_tests.C
        ${PROJECT_SOURCE_DIR}/src/classes/Solver.C
        ${PROJECT_SOURCE_DIR}/src/namespaces/ErrorMessages.C
        ${PROJECT_SOURCE_DIR}/src/MainVariables.C
    )
    add_test(NAME unit_tests COMMAND unit_tests)
endif()

# Copy all object files in obj directory by using a custom command
add_custom_command(TARGET sciantix POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_SOURCE_DIR}/obj # make obj directory
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_BINARY_DIR}/CMakeFiles/sciantix.dir/src ${PROJECT_SOURCE_DIR}/obj # copy all object files in it
)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})

include(CPack)