#include <ranges>
#include <unordered_set>
#include <vector>

#include "ControlFlowGraph.h"
#include "Dominance.h"

DominanceTree::DominanceTree(ControlFlowGraph cfg) {}

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
