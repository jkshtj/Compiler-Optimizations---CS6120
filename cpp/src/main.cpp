// NOLINTBEGIN(*)

#include <array>
#include <fstream>
#include <memory>
#include <print>
#include <sstream>
#include <stdexcept>
#include <stdlib.h>
#include <string>
#include <string_view>
#include <vector>

#include <better_variant.h>

#include "BrilCpp/ControlFlowGraph.h"
#include "BrilCpp/Dominance.h"
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


int main() {
  // std::string filePath = "../bril/examples/test/dom/while.bril";
  std::string filePath = "/Users/kjain/workspace/Compiler-Optimizations---CS6120/bril/examples/test/dom/loopcond.bril";

  std::println("Reading Bril program from input file: {}", filePath);

  json j = brilFromFile(filePath);
  Program program(j);

  std::println("Program has {} functions.", program.functions.size());

  for (auto function : program.functions) {
    for (auto instr : function.instrs) {
      // instr.match(
      //   [&](const Label& label) { std::println("LABEL ====> {}", label); },
      //   [&](const Instruction& instr) { std::println("INSTR ====> {}", instr); }
      // );

      // instr | Overload {
      //           [&](const Label& label) { std::println("LABEL ====> {}", label); }, 
      //           [&](const Instruction& instr) { std::println("INSTR ====> {}", instr); }
      //         };

      // instr | MATCH(
      //   [&](const Label& label) { std::println("LABEL ====> {}", label); }, 
      //   [&](const Instruction& instr) { std::println("INSTR ====> {}", instr); }
      // );

      instr | bv::Match {
        [&](const Label& label) { std::println("LABEL ====> {}", label); }, 
        [&](const Instruction& instr) { std::println("INSTR ====> {}", instr); }
      };
    }
  }
}

// NOLINTEND(*)