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

set(LLVM_PREFIX "/opt/homebrew/opt/llvm")
add_link_options("-L${LLVM_PREFIX}/lib/c++")

set(CMAKE_CXX_STANDARD 23 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)

# Enable compile_commands.json generation
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

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
