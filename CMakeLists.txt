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

cmake_minimum_required(VERSION 3.4.0)

# Name of the project created / existent in visual studio
project(sciantix)


# Function to add all subdirectories of a given path to include directories
function(add_subdirectory_includes root_dir)
    file(GLOB SUBDIRS RELATIVE ${root_dir} ${root_dir}/*)
    foreach(subdir ${SUBDIRS})
        if(IS_DIRECTORY ${root_dir}/${subdir})
            include_directories(${root_dir}/${subdir})
            add_subdirectory_includes(${root_dir}/${subdir})
        endif()
    endforeach()
endfunction()


# Enable C++ compiler
enable_language (CXX)

include(CTest)
enable_testing()

include_directories(${PROJECT_SOURCE_DIR}/include)

# Add all subdirectories in the include folder to the include path
add_subdirectory_includes(${PROJECT_SOURCE_DIR}/include)

file(GLOB_RECURSE SOURCES ${PROJECT_SOURCE_DIR}/src/*.C)

# Set custom object file directory
set(CMAKE_OBJECT_PATH_PREFIX ${PROJECTSOUR}/obj/)

add_executable(sciantix ${SOURCES})


add_custom_command(TARGET sciantix POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_SOURCE_DIR}/obj
    COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_BINARY_DIR}/CMakeFiles/sciantix.dir/src ${PROJECT_SOURCE_DIR}/obj
)


set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})

include(CPack)
