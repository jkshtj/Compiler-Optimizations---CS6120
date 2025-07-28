#pragma once

#include "Core.h"

#include <unordered_map>
#include <vector>

struct BasicBlock {
  std::vector<Instruction> instr;
};

struct ControlFlowGraph {
  std::vector<BasicBlock> blocks;
  std::vector<Argument> arguments;
  std::unordered_map<std::string, unsigned> blockLabel2Index;
  std::vector<std::vector<unsigned>> successors;
  std::vector<std::vector<unsigned>> predecessors;
};