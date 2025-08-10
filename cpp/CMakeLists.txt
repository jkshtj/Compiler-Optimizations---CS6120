# llvm-toolchain.cmake
# Force all compiler- and archive-tools to come from Homebrew’s LLVM
set(CMAKE_C_COMPILER   /opt/homebrew/opt/llvm/bin/clang      CACHE STRING "" FORCE)
set(CMAKE_CXX_COMPILER /opt/homebrew/opt/llvm/bin/clang++    CACHE STRING "" FORCE)

set(CMAKE_AR    /opt/homebrew/opt/llvm/bin/llvm-ar     CACHE STRING "" FORCE)
set(CMAKE_RANLIB /opt/homebrew/opt/llvm/bin/llvm-ranlib CACHE STRING "" FORCE)

# (Optionally, also force lld if you want LLVM’s linker:)
#set(CMAKE_LINKER /opt/homebrew/opt/llvm/bin/ld.lld      CACHE STRING "" FORCE)

cmake_minimum_required(VERSION 3.13.4)
project(CPPBasic LANGUAGES CXX C)

add_compile_options(-g)

set(LLVM_PREFIX "/opt/homebrew/opt/llvm")
add_link_options("-L${LLVM_PREFIX}/lib/c++")

set(CMAKE_CXX_STANDARD 23 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)

# Enable compile_commands.json generation
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Get all source files
file(GLOB_RECURSE ALL_SOURCE_FILES
    ${CMAKE_SOURCE_DIR}/src/*.cpp
    ${CMAKE_SOURCE_DIR}/src/*.h
    ${CMAKE_SOURCE_DIR}/lib/*.cpp
    ${CMAKE_SOURCE_DIR}/lib/*.h
    ${CMAKE_SOURCE_DIR}/include/*.h
    ${CMAKE_SOURCE_DIR}/include/*.hpp
    ${CMAKE_SOURCE_DIR}/test/*.cpp
    ${CMAKE_SOURCE_DIR}/test/*.h
)

# Clang-tidy configuration
find_program(CLANG_TIDY_EXE NAMES "clang-tidy" PATHS /opt/homebrew/opt/llvm/bin)
if(CLANG_TIDY_EXE)
    message(STATUS "clang-tidy found: ${CLANG_TIDY_EXE}")
    set(CMAKE_CXX_CLANG_TIDY "${CLANG_TIDY_EXE}")

    add_custom_target(check
        COMMAND ${CLANG_TIDY_EXE} ${ALL_SOURCE_FILES} -- -I${CMAKE_SOURCE_DIR}/include -I${CMAKE_SOURCE_DIR}/lib -std=c++23
        COMMENT "Running clang-tidy on all source files"
        VERBATIM
    )
else()
    message(WARNING "clang-tidy not found!")
endif()

# Clang-format configuration
find_program(CLANG_FORMAT_EXE NAMES "clang-format" PATHS /opt/homebrew/opt/llvm/bin)
if(CLANG_FORMAT_EXE)
    message(STATUS "clang-format found: ${CLANG_FORMAT_EXE}")
    
    # Add format target
    add_custom_target(format
        COMMAND ${CLANG_FORMAT_EXE} -i ${ALL_SOURCE_FILES}
        COMMENT "Formatting code with clang-format"
        VERBATIM
    )
    
    # # Add format-check target
    # add_custom_target(format-check
    #     COMMAND ${CLANG_FORMAT_EXE} --dry-run --Werror ${ALL_SOURCE_FILES}
    #     COMMENT "Checking code formatting with clang-format"
    #     VERBATIM
    # )
else()
    message(WARNING "clang-format not found!")
endif()

enable_testing()

# External packages
find_package(GTest REQUIRED)
find_package(nlohmann_json REQUIRED)

add_subdirectory(lib)
add_subdirectory(src)
add_subdirectory(test)

# LLVM/MLIR stuff
# find_package(MLIR REQUIRED CONFIG PATHS "${MLIR_DIR}")

# list(APPEND CMAKE_MODULE_PATH "${MLIR_CMAKE_DIR}")
# list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")

# include(TableGen)
# include(HandleLLVMOptions)
# include(AddLLVM)
# include(AddMLIR)

# # Include MLIR's cmake macros.
# # include_directories(${CMAKE_CURRENT_BINARY_DIR})
# include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
# include_directories(SYSTEM ${MLIR_INCLUDE_DIRS})
# include_directories(${PROJECT_SOURCE_DIR}/include)

# link_directories(${LLVM_BUILD_LIBRARY_DIR})
# add_definitions(${LLVM_DEFINITIONS})
