cmake_minimum_required(VERSION 3.23)

project(P_HyLand Fortran)
enable_language(Fortran)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR})


# save the library
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)
# Fortran .mod
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/include)

find_package(OpenMP REQUIRED)


SET(HDF5_USE_STATIC_LIBRARIES ON)

SET(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "/opt/homebrew/Cellar/hdf5/1.14.1/")

find_package(HDF5 MODULE REQUIRED COMPONENTS Fortran HL)


message("fortran compiler is ${CMAKE_fortran_COMPILER}")
# write the HDF5 finding result
message("HDF5_FOUND is : ${HDF5_FOUND}")
message("HDF5_INCLUDE_DIRS is : ${HDF5_INCLUDE_DIRS}")
message("HDF5_VERSION is : ${HDF5_VERSION}")
message("HDF5_LIBRARIES is ${HDF5_LIBRARIES}")
message("HDF5_HL_LIBRARIES is ${HDF5_HL_LIBRARIES}")


# detect the operating system
IF (CMAKE_SYSTEM_NAME MATCHES "Linux")
    MESSAGE(STATUS "current platform: Linux ")
    file(READ Control.Project FILE_CONTENTS)
    string(REPLACE "\\" "/" FILE_CONTENTS "${FILE_CONTENTS}")
    file(WRITE Control.Project "${FILE_CONTENTS}")

ELSEIF (CMAKE_SYSTEM_NAME MATCHES "Windows")
    MESSAGE(STATUS "current platform: Windows")
    file(READ Control.Project FILE_CONTENTS)
    string(REPLACE "/" "\\" FILE_CONTENTS "${FILE_CONTENTS}")
    file(WRITE Control.Project "${FILE_CONTENTS}")

ELSEIF (CMAKE_SYSTEM_NAME MATCHES "Darwin")
    MESSAGE(STATUS "current platform: Mac OS X")
    file(READ Control.Project FILE_CONTENTS)
    string(REPLACE "\\" "/" FILE_CONTENTS "${FILE_CONTENTS}")
    file(WRITE Control.Project "${FILE_CONTENTS}")

ELSE ()
    MESSAGE(STATUS "other platform: ${CMAKE_SYSTEM_NAME}")
ENDIF (CMAKE_SYSTEM_NAME MATCHES "Linux")

if (OPENMP_FOUND)
    message("openMP found")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
endif()


include_directories("${HDF5_INCLUDE_DIRS}")

file(GLOB_RECURSE SRCS RELATIVE ${PROJECT_SOURCE_DIR} src/*)

add_executable(PHyL_v1 ${SRCS})

target_link_libraries(PHyL_v1 ${HDF5_LIBRARIES})
