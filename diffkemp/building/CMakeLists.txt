set(CC_WRAPPER_SOURCE "${CMAKE_SOURCE_DIR}/diffkemp/building/cc_wrapper.py")
add_custom_target(cc-wrapper-source ALL DEPENDS ${CC_WRAPPER_SOURCE})
install(PROGRAMS ${CC_WRAPPER_SOURCE} RENAME diffkemp-cc-wrapper.py DESTINATION ${CMAKE_INSTALL_BINDIR})

# Try to find RPython as:
# - standalone executable (rpython)
# - Python2 module (python2 -m rpython)
if (NOT DEFINED ENV{WITHOUT_RPYTHON})
  find_program(RPYTHON rpython)
  if (NOT RPYTHON)
    execute_process(COMMAND python2 -m rpython --help
                    RESULT_VARIABLE HAVE_RPYTHON_MODULE
                    OUTPUT_QUIET ERROR_QUIET)
    if (${HAVE_RPYTHON_MODULE} EQUAL 0)
      set(RPYTHON python2 -m rpython)
    endif ()
  endif ()
  if (NOT RPYTHON)
    message(WARNING "RPython not found, not building compiler wrapper")
  else ()
    set(CC_WRAPPER "${CMAKE_BINARY_DIR}/cc_wrapper-c")
    add_custom_target(cc-wrapper ALL DEPENDS ${CC_WRAPPER})
    # Note: Setting PYTHONPATH to be able to use diffkemp modules from wrapper.
    add_custom_command(OUTPUT ${CC_WRAPPER}
                       DEPENDS ${CC_WRAPPER_SOURCE}
                       COMMAND PYTHONPATH=${CMAKE_SOURCE_DIR} ${RPYTHON} ${CC_WRAPPER_SOURCE} "--output=${CC_WRAPPER}"
                       WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
    install(PROGRAMS ${CC_WRAPPER} RENAME diffkemp-cc-wrapper DESTINATION ${CMAKE_INSTALL_BINDIR})
  endif ()
endif ()
