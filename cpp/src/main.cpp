// NOLINTBEGIN(*)

#include <array>
#include <memory>
#include <print>
#include <sstream>
#include <stdexcept>
#include <stdlib.h>
#include <string>
#include <string_view>
#include <vector>

#include "BrilCpp/ControlFlowGraph.h"
#include "BrilCpp/Core.h"
#include "BrilCpp/Enum.h"
#include "BrilCpp/Format.h"

int main() {
  Function func{
      .name = "main",
      .arguments = {{"arg1", IntType{}}, {"arg2", BoolType{}}},
      .type = VoidType{},
      .instrs =
          {ConstantInstr{"x", IntType{}, 42, EmptyLoc{}},
           ValueInstr{
               "add", "result", IntType{}, {"x", "arg1"}, {}, {}, EmptyLoc{}}},
      .location = EmptyLoc{}};

  ControlFlowGraph cfg(func);

  std::println("Control Flow Graph for function: {}", cfg);
}

// NOLINTEND(*)