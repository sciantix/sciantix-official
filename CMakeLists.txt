# This CMakeLists.txt file is useful when compiling SCIANTIX with the cmake tool
#
# output:
#   > build directory with executable in build/sciantix.x
#
# When using IDE, a CMakeSettings.json could be automatically produced to compile sciantix in a locall out/build/ folder
# by setting this file as startup element.
# More information here: (https://learn.microsoft.com/it-it/cpp/build/configure-cmake-debugging-sessions?view=msvc-170)
#
# Author: G. Zullo (giovanni.zullo@polimi.it)

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

# Set option for coupling with TU
option (COUPLING_TU OFF)

# Include CTest
include(CTest)
enable_testing()

# Include header files
include_directories(${PROJECT_SOURCE_DIR}/include)
# Add all subdirectories of the include folder to the include path
add_subdirectory_includes(${PROJECT_SOURCE_DIR}/include)

# Include source files
file(GLOB_RECURSE SOURCES ${PROJECT_SOURCE_DIR}/src/*.C)

# Set custom object file directory (to have all object files in the obj directory)
set(CMAKE_OBJECT_PATH_PREFIX ${PROJECTSOUR}/obj/)

# Add executable sciantix.x (or .exe) or create static library
if(COUPLING_TU)
    add_library(sciantix STATIC ${SOURCES})
    set(CMAKE_STATIC_LIBRARY_SUFFIX ".a")
else()
    add_executable(sciantix ${SOURCES})
    if (UNIX)
        set(CMAKE_EXECUTABLE_SUFFIX ".x")
    endif()
endif()

# Copy all object files in obj directory by using a custom command
add_custom_command(TARGET sciantix POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_SOURCE_DIR}/obj # make obj directory
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_BINARY_DIR}/CMakeFiles/sciantix.dir/src ${PROJECT_SOURCE_DIR}/obj # copy all object files in it
)

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})

include(CPack)

# Optional UN L3 numerical/state tests. The historical executable and its
# solver implementation keep their existing configuration and entry points.
option(SCIANTIX_UN_L3_TESTS "Build the UN L3 fixture replay and state tests" OFF)
if(SCIANTIX_UN_L3_TESTS)
    add_library(un_l3_exchange_test_objects OBJECT
        src/classes/Solver.C src/models/UNIntraGranular.C
        src/models/UNTransport.C src/models/UNBubblePhysics.C)
    set_property(TARGET un_l3_exchange_test_objects PROPERTY CXX_STANDARD 11)
    add_executable(un_l3_solver_fixture
        UN_model/phase2a/solver_fixture.cpp $<TARGET_OBJECTS:un_l3_exchange_test_objects>)
    set_property(TARGET un_l3_solver_fixture PROPERTY CXX_STANDARD 11)
    add_executable(un_l3_state_test
        UN_model/phase2a/state_test.cpp $<TARGET_OBJECTS:un_l3_exchange_test_objects>)
    set_property(TARGET un_l3_state_test PROPERTY CXX_STANDARD 11)
    add_test(NAME un_l3_state_contract COMMAND un_l3_state_test)
endif()
