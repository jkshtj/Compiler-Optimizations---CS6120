# Test executable
add_executable(
    unit_tests
    test_main.cpp
)

# Link test executable against gtest & gtest_main
target_link_libraries(
    unit_tests
    brilcpp
    GTest::gtest_main
    GTest::gtest
    GTest::gmock
)

# Include directories for tests
target_include_directories(unit_tests PRIVATE 
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/lib
    ${PROJECT_SOURCE_DIR}/src
)

# # Add tests to CTest
# include(GoogleTest)
# gtest_discover_tests(unit_tests)
