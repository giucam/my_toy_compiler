
add_test(NAME divide_by_0 COMMAND ${CMAKE_BINARY_DIR}/parser divide_by_0.ppig WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME divide_by_rand COMMAND ${CMAKE_BINARY_DIR}/parser divide_by_rand.ppig WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME constraints_fail COMMAND ${CMAKE_BINARY_DIR}/parser constraints_fail.ppig WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME constraints_ptr_fail COMMAND ${CMAKE_BINARY_DIR}/parser constraints_ptr_fail.ppig WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME ptr_bad_constraints COMMAND ${CMAKE_BINARY_DIR}/parser ptr_bad_constraints.ppig WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

set_tests_properties(divide_by_0
                     divide_by_rand
                     constraints_fail
                     constraints_ptr_fail
                     ptr_bad_constraints
                     PROPERTIES WILL_FAIL TRUE)
