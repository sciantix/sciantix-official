cmake_minimum_required(VERSION 3.14)

project(sciantix)

set(pybind11_DIR "/home/giovanni/.local/lib/python3.10/site-packages/pybind11/share/cmake/pybind11")

find_package(pybind11 REQUIRED)

function(add_subdirectory_includes root_dir)
    file(GLOB SUBDIRS RELATIVE ${root_dir} ${root_dir}/*) # get all files in directory (which is passed in parameter)
    foreach(subdir ${SUBDIRS})
        if(IS_DIRECTORY ${root_dir}/${subdir}) # if file is a directory
            include_directories(${root_dir}/${subdir})  # include the directory as header directory
            add_subdirectory_includes(${root_dir}/${subdir})    # execute the function for the subdirectory (to get all subdirectories of it if any)
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
    LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/python # Name of the folder
    OUTPUT_NAME "sciantixModule"  # Name of the Outputed Module
)

# Specify C++ standard
set_property(TARGET sciantix PROPERTY CXX_STANDARD 17)

# add also sciantix.x as product