# run_test.cmake: runs the test program's tests (its input "test") and fails unless they all pass
# (cmake -DTEST_PROGRAM=<path> -P run_test.cmake)
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/neonumeric_test.input" "test\n")
execute_process(COMMAND "${TEST_PROGRAM}" INPUT_FILE "${CMAKE_CURRENT_BINARY_DIR}/neonumeric_test.input"
    OUTPUT_VARIABLE output ERROR_VARIABLE errors RESULT_VARIABLE result)
if(NOT result EQUAL 0 OR NOT output MATCHES "tests pass")
    message(FATAL_ERROR "neonumeric tests failed (exit code ${result}):\n${errors}")
endif()
string(REGEX MATCH "All [0-9]+ tests pass" summary "${output}")
message(STATUS "${summary}")
