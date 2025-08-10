#include <ranges>
#include <string>
#include <utility>

#include "ControlFlowGraph.h"
#include "Format.h"

std::vector<BasicBlock> BasicBlock::getBlocksFor(const Function &function) {
  std::vector<BasicBlock> result;
  
  std::string entryLabel = "entry";
  if (auto label = function.instrs[0].tryAs<Label>()) {
    entryLabel = (*label).name;
  }
  BasicBlock currBlock{.label = entryLabel, .instrs = {}};

  auto afterFirstLabel =
      function.instrs | std::views::drop_while([](auto code) {
        return code.template is<Label>();
      });

  for (auto instr : afterFirstLabel) {
    if (auto label = instr.tryAs<Label>()) {
      result.push_back(currBlock);
      currBlock = {.label = label->name, .instrs = {}};
      continue;
    }

    currBlock.instrs.push_back(instr.as<Instruction>());
  }

  result.push_back(currBlock);
  return result;
}

ControlFlowGraph::ControlFlowGraph(const Function &function)
    : arguments(function.arguments), blocks(BasicBlock::getBlocksFor(function)),
      successors(blocks.size(), std::vector<unsigned>{}),
      predecessors(blocks.size(), std::vector<unsigned>{}) {
  for (unsigned i = 0; i < blocks.size(); ++i) {
    blockLabel2Index[blocks[i].label] = i;
  }

  for (auto block : blocks) {
    auto terminator = block.instrs.back();
    auto currBlockIndex = blockLabel2Index[block.label];

    if (auto jmpOrBr = terminator.tryAs<EffectInstr>();
        jmpOrBr && 
        (jmpOrBr->op == "jmp" || jmpOrBr->op == "br")
    ) {
      for (auto label : jmpOrBr->labels) {
        auto targetBlockIndex = blockLabel2Index[label];
        successors[currBlockIndex].push_back(targetBlockIndex);
        predecessors[targetBlockIndex].push_back(currBlockIndex);
      }
      continue;
    }

    if (currBlockIndex < blocks.size() - 1) {
      auto targetBlockIndex = currBlockIndex + 1;
      successors[currBlockIndex].push_back(targetBlockIndex);
      predecessors[targetBlockIndex].push_back(currBlockIndex);
    }
  }
}
