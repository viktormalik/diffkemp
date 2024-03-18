cmake_policy(SET CMP0048 NEW)
cmake_minimum_required(VERSION 3.5)
project(diffkemp
    VERSION 0.5.0)

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
add_definitions(${LLVM_DEFINITIONS})
include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
link_directories(${LLVM_LIBRARY_DIRS})
set(CMAKE_CXX_STANDARD 17)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_LIST_DIR}/cmake)

include(GNUInstallDirs)

option(SANITIZE_ADDRESS "Enable address sanitizer" OFF)

add_subdirectory(diffkemp/simpll)
add_subdirectory(diffkemp/building)

option(BUILD_LLREVE "Build the LLReve tool for semantic comparison" OFF)
if (${BUILD_LLREVE})
  if (NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/llreve)
    execute_process(COMMAND
      git clone
        -b diffkemp
        https://github.com/viktormalik/llreve.git
        ${CMAKE_CURRENT_SOURCE_DIR}/llreve)
  endif ()
  add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/llreve/reve)
endif ()

execute_process(COMMAND
  ${CMAKE_CURRENT_SOURCE_DIR}/tools/simpll_clean.sh
  ${CMAKE_CURRENT_SOURCE_DIR})

option(BUILD_VIEWER "Install packages for result viewer and create production build" OFF)
if (${BUILD_VIEWER})
  find_program(NPM npm REQUIRED)
  message(STATUS "Found npm ${NPM}")
  message(STATUS "Installing packages for result viewer")
  execute_process(
    COMMAND ${NPM} install
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/view
  )
  message(STATUS "Creating production build of result viewer")
  execute_process(
    COMMAND ${NPM} run build
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/view
  )

  set(VIEW_INSTALL_DIR /var/lib/diffkemp/view)
  install(DIRECTORY view/build/ DESTINATION ${VIEW_INSTALL_DIR}
    # Setting 644 for files.
    FILE_PERMISSIONS OWNER_READ OWNER_WRITE
                     GROUP_READ
                     WORLD_READ
    # Setting 755 for directories to make it possible
    # to access files/folders in the directory. 
    DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                          GROUP_READ GROUP_EXECUTE
                          WORLD_READ WORLD_EXECUTE
    # Do not install directory describing results of comparison.
    PATTERN "data/" EXCLUDE
  ) # install - copy `build` dir
  # Creating `data` directory
  # and setting the permissions to 777 to make able to create there files and remove them.
  install(DIRECTORY DESTINATION ${VIEW_INSTALL_DIR}/data
    DIRECTORY_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE
                          GROUP_READ GROUP_WRITE GROUP_EXECUTE
                          WORLD_READ WORLD_WRITE WORLD_EXECUTE
  ) # install `data` dir
endif () # ${BUILD_VIEWER}

add_subdirectory(tests/unit_tests/simpll)
