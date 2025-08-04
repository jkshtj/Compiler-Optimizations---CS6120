#pragma once

#include <unordered_map>
#include <vector>

#include "Core.h"

// TODO: Implement iterator for iteration of instructions.
struct BasicBlock {
  std::string label;
  std::vector<Instruction> instrs;

  static std::vector<BasicBlock> getBlocksFor(const Function &function);
};

// TODO: Implement iterator for iteration of basic blocks
// and instructions.
struct ControlFlowGraph {
  std::vector<BasicBlock> blocks;
  std::vector<Argument> arguments;
  std::unordered_map<std::string, unsigned> blockLabel2Index;
  std::vector<std::vector<unsigned>> successors;
  std::vector<std::vector<unsigned>> predecessors;

  ControlFlowGraph(const Function &function);
};
