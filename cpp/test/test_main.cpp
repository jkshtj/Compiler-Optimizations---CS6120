// NOLINTBEGIN(*)

#include <fstream>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <print>
#include <sstream>
#include <stdlib.h>
#include <string_view>

#include "BrilCpp/Core.h"
#include "BrilCpp/Format.h"

static std::string exec(const char *cmd) {
  char buffer[128];
  std::stringstream result;
  std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd, "r"), pclose);

  if (!pipe) {
    throw std::runtime_error("popen() failed!");
  }

  while (fgets(buffer, sizeof buffer, pipe.get()) != nullptr) {
    result << buffer;
  }
  return result.str();
}

static json brilFromFile(std::string_view filePath) {
  std::ifstream input(filePath.data());
  if (!input.is_open()) {
    throw std::runtime_error("Could not open file: " +
                             std::string(filePath.data()));
  }

  // Convert `.json` if file type is `.bril`
  if (filePath.ends_with(".bril")) {
    auto command = "bril2json <" + std::string(filePath);
    auto convertedInput = exec(command.c_str());
    return json::parse(convertedInput);
  }

  return json::parse(input);
}

// Parameterized test fixture for testing with different Bril files
class BrilInputFiles : public ::testing::TestWithParam<std::string> {};

TEST_P(BrilInputFiles, BasicProgramsFromFile) {
  std::string filePath = GetParam();

  std::println("Reading Bril program from input file: {}", filePath);

  // Test that we can load the file and create a Program from it
  EXPECT_NO_THROW({
    json j = brilFromFile(filePath);
    Program program(j);
  });
}

TEST_P(BrilInputFiles, BasicControlFlowGraphsFromFile) {
  std::string filePath = GetParam();

  std::println("Reading Bril program from input file: {}", filePath);

  // Test that we can load the file and create a Program from it
  EXPECT_NO_THROW({
    json j = brilFromFile(filePath);
    Program program(j);

    std::println("Program has {} functions.", program.functions.size());

    for (auto function : program.functions) {
      ControlFlowGraph cfg(function);
      // std::println("{}", cfg);
    }
  });
}

// Instantiate the parameterized test with some sample Bril files
INSTANTIATE_TEST_SUITE_P(
    InputFiles, BrilInputFiles,
    ::testing::Values("../bril/type-infer/tests/parse/add.json",
                      "../bril/type-infer/tests/parse/div.json",
                      "../bril/examples/test/df/fact.bril",
                      "../bril/examples/test/df/cond-args.bril",
                      "../bril/examples/test/df/cond.bril",
                      "../bril/examples/test/tdce/combo.bril",
                      "../bril/examples/test/tdce/diamond.bril",
                      "../bril/examples/test/tdce/double.bril",
                      "../bril/examples/test/tdce/reassign.bril",
                      "../bril/examples/test/tdce/skipped.bril",
                      "../bril/examples/test/lvn/commute.bril"));

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
// NOLINTEND(*)