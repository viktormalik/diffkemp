OPTION(VENDOR_GTEST "Vendor Google Test libraries" OFF)
if (${VENDOR_GTEST})
# Download and unpack googletest at configure time
configure_file(CMakeLists.txt.in googletest-download/CMakeLists.txt)
execute_process(COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" .
                WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/googletest-download")
execute_process(COMMAND "${CMAKE_COMMAND}" --build .
                WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/googletest-download")

# Add googletest directly to our build. This adds the following targets:
# gtest, gtest_main, gmock and gmock_main
add_subdirectory("${CMAKE_CURRENT_BINARY_DIR}/googletest-src"
                 "${CMAKE_CURRENT_BINARY_DIR}/googletest-build"
                 EXCLUDE_FROM_ALL)
 endif(${VENDOR_GTEST})

# Disable RTTI to link with SimpLL correctly.
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti -fvisibility=hidden")

if (${SANITIZE_ADDRESS})
  add_compile_options(-fsanitize=address -fno-omit-frame-pointer)
  add_link_options(-fsanitize=address -fno-omit-frame-pointer)
endif()

enable_testing()

include_directories(${CMAKE_SOURCE_DIR}/diffkemp/simpll)

file(GLOB pass_tests passes/*.cpp)
add_executable(runTests SimpLLTest.cpp DifferentialFunctionComparatorTest.cpp
               FieldAccessUtilsTest.cpp ${pass_tests})

exec_program(llvm-config ARGS --libs irreader passes support OUTPUT_VARIABLE llvm_libs)
exec_program(llvm-config ARGS --system-libs OUTPUT_VARIABLE system_libs)
string(STRIP ${system_libs} system_libs)

target_link_libraries(runTests gtest -lpthread simpll-lib ${llvm_libs})
if (NOT ${system_libs} STREQUAL "")
	target_link_libraries(runTests gtest ${system_libs})
endif()

include(GoogleTest)
gtest_discover_tests(runTests)
add_custom_target(test
  COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
  DEPENDS runTests)
