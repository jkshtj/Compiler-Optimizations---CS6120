/// This module contains dominance utilities for Bril functions.
///
/// Dominance relationships have the following 2 variants and can be defined
/// as follows.
/// 1. A dominates B iff all paths from the entry to B include A.
/// 2. A post-dominates B iff all paths from B to the exit include A.
///
/// Within the above mentioned variants there are further nuanced definitions
/// of dominance as well.
/// 1. Strict - A strictly dominates B iff A dominates B and A ≠ B.
///     Dominance is reflexive, so “strict” dominance just takes
///     that part away.
/// 2. Immediate - A immediately dominates B iff A dominates B but A does not strictly
///    dominate any other node that strictly dominates B, in which case A
///     is B’s direct parent in the dominator tree.
/// 3. PostStrict - A strictly post-dominates B iff A post-dominates B and A ≠ B.
/// 4. PostImmediate - A immediately post-dominates B iff A post-dominates B but A does not
///    strictly post-dominate any other node that strictly dominates B, in which
///    case A is B’s direct child in the dominator tree.

#pragma once

#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "ControlFlowGraph.h"

/// The dominator tree is a convenient data structure for storing the dominance
/// relationships in an entire function. The recursive children of a given node
/// in a tree are the nodes that that node dominates.
struct DominanceTree {
  DominanceTree(ControlFlowGraph cfg);

  std::vector<BasicBlock> nodes;
  std::unordered_map<std::string, unsigned> blockLabel2Index;
  std::vector<std::unordered_set<unsigned>> immediatelyDominatedByMe;
};

struct DominanceAnalysis {
  DominanceAnalysis(ControlFlowGraph cfg): cfg(cfg) {}
  
  /// Returns the dominator sets associated to a bril function.
  std::vector<std::unordered_set<unsigned>> findDominators();

  ControlFlowGraph cfg;
};