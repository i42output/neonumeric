cmake_minimum_required(VERSION 3.15)
cmake_policy(VERSION 3.15)

# meta
project(neonumeric VERSION 0.0.1 LANGUAGES CXX)

# deployment
IF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  SET(CMAKE_INSTALL_PREFIX "/usr/local" CACHE PATH "Default install prefix" FORCE)
ENDIF(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

include(GNUInstallDirs)
set(CMAKE_INSTALL_CMAKEDIR "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}")

# project (header only)
file(GLOB_RECURSE HEADER_FILES include/*.hpp)

add_library(neonumeric INTERFACE)
add_library(neonumeric::neonumeric ALIAS neonumeric)
target_include_directories(neonumeric INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)
target_compile_features(neonumeric INTERFACE cxx_std_20)

include(CMakePackageConfigHelpers)
configure_package_config_file(neonumericConfig.cmake.in neonumericConfig.cmake INSTALL_DESTINATION "${CMAKE_INSTALL_CMAKEDIR}")
write_basic_package_version_file(neonumericConfigVersion.cmake COMPATIBILITY AnyNewerVersion ARCH_INDEPENDENT)

install(TARGETS neonumeric EXPORT neonumeric)
install(DIRECTORY include/neonumeric DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")
install(EXPORT neonumeric DESTINATION "${CMAKE_INSTALL_CMAKEDIR}" FILE neonumericTargets.cmake NAMESPACE neonumeric::)
install(FILES "${CMAKE_CURRENT_BINARY_DIR}/neonumericConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/neonumericConfigVersion.cmake" DESTINATION "${CMAKE_INSTALL_CMAKEDIR}")
install(FILES "LICENSE" DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/licenses/${PROJECT_NAME}")

# tests (the test program runs its tests when its input is "test")
if(CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
  set(NEONUMERIC_TOP_LEVEL ON)
else()
  set(NEONUMERIC_TOP_LEVEL OFF)
endif()
option(NEONUMERIC_TESTS "Build neonumeric tests" ${NEONUMERIC_TOP_LEVEL})
if(NEONUMERIC_TESTS)
  enable_testing()
  add_executable(neonumeric_test unit_tests/src/test.cpp ${HEADER_FILES})
  target_link_libraries(neonumeric_test PRIVATE neonumeric)
  if(MSVC)
    target_compile_options(neonumeric_test PRIVATE /std:c++latest /bigobj /Zc:__cplusplus)
    set_property(TARGET neonumeric_test PROPERTY MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
  endif()
  add_test(NAME neonumeric_test COMMAND ${CMAKE_COMMAND} -DTEST_PROGRAM=$<TARGET_FILE:neonumeric_test> -P ${CMAKE_CURRENT_SOURCE_DIR}/unit_tests/run_test.cmake)
endif()
