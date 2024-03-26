# - Try to find libz3
# Once done this will define
#
#  Z3_FOUND - system has libz3
#  Z3_CXX_INCLUDE_DIRS - the z3 C++ include directory
#  Z3_LIBRARIES - Link these to use libbpf

find_path (Z3_CXX_INCLUDE_DIRS z3++.h)

find_library (Z3_LIBRARIES z3)

include (FindPackageHandleStandardArgs)

FIND_PACKAGE_HANDLE_STANDARD_ARGS(Z3 REQUIRED_VARS
  Z3_LIBRARIES
  Z3_CXX_INCLUDE_DIRS)

mark_as_advanced(Z3_CXX_INCLUDE_DIRS Z3_LIBRARIES)
