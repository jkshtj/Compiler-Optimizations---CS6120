#include <assert.h>
#include <iterator>
#include <ranges>
#include <unordered_set>
#include <vector>

#include "ControlFlowGraph.h"
#include "Dominance.h"

// template<typename T>
// void print_type() {
//     std::println("Type is: {}", __PRETTY_FUNCTION__); // GCC/Clang
//     // or std::println("Type is: {}", __FUNCSIG__); // MSVC
// }

DominanceTree::DominanceTree(ControlFlowGraph cfg) : nodes(cfg.blocks), blockLabel2Index(cfg.blockLabel2Index) {
  DominanceAnalysis dominanceAnalysis = cfg;
  auto dominators = dominanceAnalysis.findDominators();
  immediatelyDominatedByMe = std::vector(cfg.blocks.size(), std::unordered_set<unsigned>());

  for (auto zipped : std::views::zip(std::views::iota(0), dominators)) {
    auto& [blockIndex, itsDominators] = zipped;
    auto& itsPredecessors = cfg.predecessors[blockIndex];
    
    // Find the sinle-point intersection of predecessors and dominators, there should only be zero or one.
    auto intersection = itsPredecessors | std::views::filter([&](const auto& predecessor) {
                                            return itsDominators.contains(predecessor);
                                          });
    
    auto numImmediateDominators = std::ranges::distance(intersection);

    assert(numImmediateDominators <= 1 && "Only a zero or one predecessor blocks can be a basic block's dominator!");
    
    if (numImmediateDominators) {
      immediatelyDominatedByMe[intersection.front()].insert(blockIndex);
    }
  }
}

std::vector<std::unordered_set<unsigned>> DominanceAnalysis::findDominators() {
  // At the start all blocks will be dominated by all other blocks
  auto defaultDominatorSet = std::views::iota(0u, (unsigned)cfg.blocks.size()) 
                              | std::ranges::to<std::unordered_set<unsigned>>();
  std::vector<std::unordered_set<unsigned>> dominateMe(cfg.blocks.size(), defaultDominatorSet); 
  
  // Entry block will only be dominated by itself
  dominateMe[0] = std::unordered_set<unsigned> {0};

  auto changed = true;
  while (changed) {
    changed = false;

    // For all blocks except for entry...
    for (auto i : std::views::iota(1, (int)cfg.blocks.size())) {
      // Update the dominators for each block.
      //
      // The new dominator set for each block will be an intersection of
      // the dominator sets for all its predecessors.
      auto newDomSet = dominateMe[i];
      for (auto pred : cfg.predecessors[i]) {
        newDomSet = newDomSet 
                      | std::views::filter([&](const auto& elem) {
                          return dominateMe[pred].contains(elem);
                        })
                      | std::ranges::to<std::unordered_set<unsigned>>();
      }

      newDomSet.insert(i);

      if (newDomSet != dominateMe[i]) {
        changed = true;
        dominateMe[i] = newDomSet;
      }
    }
  }

  return dominateMe;
}
