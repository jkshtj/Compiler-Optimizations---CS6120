// NOLINTBEGIN(*)

#include <fstream>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <print>
#include <ranges>
#include <sstream>
#include <stdlib.h>
#include <string_view>

#include "BrilCpp/Core.h"
#include "BrilCpp/Dominance.h"
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
class TypeConstructionTests : public ::testing::TestWithParam<std::string> {};

// Instantiate the parameterized test with some sample Bril files
INSTANTIATE_TEST_SUITE_P(,TypeConstructionTests,
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
                    "../bril/examples/test/lvn/commute.bril")
);

TEST_P(TypeConstructionTests, VerifyProgramFromFile) {
  std::string filePath = GetParam();

  std::println("Reading Bril program from input file: {}", filePath);

  // Test that we can load the file and create a Program from it
  EXPECT_NO_THROW({
    json j = brilFromFile(filePath);
    Program program(j);
  });
}

TEST_P(TypeConstructionTests, VerifyControlFlowGraphFromFile) {
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

TEST(TypeVerificationTests, VerifyPredecessorsAndSuccessors) {
  std::string filePath = "../bril/examples/test/dom/loopcond.bril";

  std::println("Reading Bril program from input file: {}", filePath);

  // Test that we can load the file and create a Program from it
  EXPECT_NO_THROW({
    json j = brilFromFile(filePath);
    Program program(j);

    std::println("Program has {} functions.", program.functions.size());

    for (auto function : program.functions) {
      ControlFlowGraph cfg(function);
      
      // Sort predecessors and successors based on block IDs to get a deterministic order during testing
      auto predecessors = cfg.predecessors | std::views::transform([](auto& preds) {
        std::ranges::sort(preds);
        return preds;
      });
      auto successors = cfg.successors | std::views::transform([](auto& succs) {
        std::ranges::sort(succs);
        return succs;
      });

      /* 
        @main {
          .entry:
            x: int = const 0;
            i: int = const 0;
            one: int = const 1;

          .loop:
            max: int = const 10;
            cond: bool = lt i max;
            br cond .body .exit;

          .body:
            mid: int = const 5;
            cond: bool = lt i mid;
            br cond .then .endif;

          .then:
            x: int = add x one;
            jmp .endif;

          .endif:
            factor: int = const 2;
            x: int = mul x factor;

            i: int = add i one;
            jmp .loop;

          .exit:
            print x;
        }
      */

      // .entry
      EXPECT_EQ(cfg.blocks[0].label, "entry");

      EXPECT_EQ(predecessors[0].size(), 0); // .entry has no predecessors

      EXPECT_EQ(successors[0].size(), 1); // .entry has one successor (.loop)
      EXPECT_EQ(cfg.blocks[successors[0][0]].label, "loop");

      // .loop
      EXPECT_EQ(cfg.blocks[1].label, "loop");
      
      EXPECT_EQ(predecessors[1].size(), 2); // .loop has 2 predecessors (.entry, .endif)
      EXPECT_EQ(cfg.blocks[predecessors[1][0]].label, "entry");
      EXPECT_EQ(cfg.blocks[predecessors[1][1]].label, "endif");

      EXPECT_EQ(successors[1].size(), 2); // .loop has two successors (.body, .exit)
      EXPECT_EQ(cfg.blocks[successors[1][0]].label, "body");
      EXPECT_EQ(cfg.blocks[successors[1][1]].label, "exit");

      // .body
      EXPECT_EQ(cfg.blocks[2].label, "body");
      
      EXPECT_EQ(predecessors[2].size(), 1); // .body has one predecessor (.loop)
      EXPECT_EQ(cfg.blocks[predecessors[2][0]].label, "loop");

      EXPECT_EQ(successors[2].size(), 2); // .body has two successors (.then, .endif)
      EXPECT_EQ(cfg.blocks[successors[2][0]].label, "then");
      EXPECT_EQ(cfg.blocks[successors[2][1]].label, "endif");

      // .then
      EXPECT_EQ(cfg.blocks[3].label, "then");

      EXPECT_EQ(predecessors[3].size(), 1); // .then has one predecessor (.body)
      EXPECT_EQ(cfg.blocks[predecessors[3][0]].label, "body");

      EXPECT_EQ(successors[3].size(), 1); // .then has one successor (.endif)
      EXPECT_EQ(cfg.blocks[successors[3][0]].label, "endif");

      // .endif
      EXPECT_EQ(cfg.blocks[4].label, "endif");

      EXPECT_EQ(predecessors[4].size(), 2); // .endif has two predecessor (.body, .then)
      EXPECT_EQ(cfg.blocks[predecessors[4][0]].label, "body"); 
      EXPECT_EQ(cfg.blocks[predecessors[4][1]].label, "then"); 

      EXPECT_EQ(successors[4].size(), 1); // .endif has one successor (.loop)
      EXPECT_EQ(cfg.blocks[successors[4][0]].label, "loop");

      // .exit
      EXPECT_EQ(cfg.blocks[5].label, "exit");
      
      EXPECT_EQ(predecessors[5].size(), 1); // .exit has one predecessor (.loop)
      EXPECT_EQ(cfg.blocks[predecessors[5][0]].label, "loop");

      EXPECT_EQ(successors[5].size(), 0); // .exit has no successors
    }
  });
}

TEST(TypeVerificationTests, VerifyDominators) {
  std::string filePath = "../bril/examples/test/dom/loopcond.bril";

  std::println("Reading Bril program from input file: {}", filePath);

  // Test that we can load the file and create a Program from it
  EXPECT_NO_THROW({
    json j = brilFromFile(filePath);
    Program program(j);

    std::println("Program has {} functions.", program.functions.size());

    for (auto function : program.functions) {
      ControlFlowGraph cfg(function);
      DominanceAnalysis dom(cfg);
      
      auto dominators = dom.findDominators();

      // Sort dominators for each block by their labels for deterministic testing
      auto sortedDominators = dominators 
                                | std::views::transform([&](auto& domSet) {
                                    std::vector<unsigned> sorted(domSet.begin(), domSet.end());
                                    std::ranges::sort(sorted, {}, [&](unsigned index) {
                                      return cfg.blocks[index].label; // Sort by block label
                                    });
                                    return sorted;
                                  });
      
      // for (auto i = 0; i < sortedDominators.size(); ++i) {
      //   auto dom = sortedDominators[i];
      //   std::println("Dominators for {}: ", cfg.blocks[i].label);
      //   for (auto index : dom) {
      //     std::println("  - {}", cfg.blocks[index].label);
      //   }
      // }

      /*
        {
          "body": [
            "body",
            "entry",
            "loop"
          ],
          "endif": [
            "body",
            "endif",
            "entry",
            "loop"
          ],
          "entry": [
            "entry"
          ],
          "exit": [
            "entry",
            "exit",
            "loop"
          ],
          "loop": [
            "entry",
            "loop"
          ],
          "then": [
            "body",
            "entry",
            "loop",
            "then"
          ]
        }
      */

      // .entry
      EXPECT_EQ(sortedDominators[0].size(), 1); // .entry only dominates itself
      EXPECT_EQ(cfg.blocks[sortedDominators[0][0]].label, "entry"); // .entry only dominates itself

      // .loop
      EXPECT_EQ(sortedDominators[1].size(), 2); // .loop is dominated by .entry and itself
      EXPECT_EQ(cfg.blocks[sortedDominators[1][0]].label, "entry");
      EXPECT_EQ(cfg.blocks[sortedDominators[1][1]].label, "loop");

      // .body
      EXPECT_EQ(sortedDominators[2].size(), 3); // .body is dominated by .entry, .loop, and itself
      EXPECT_EQ(cfg.blocks[sortedDominators[2][0]].label, "body");
      EXPECT_EQ(cfg.blocks[sortedDominators[2][1]].label, "entry");
      EXPECT_EQ(cfg.blocks[sortedDominators[2][2]].label, "loop");

      // .then
      EXPECT_EQ(sortedDominators[3].size(), 4); // .then is dominated by .body, .entry, .loop, and itself
      EXPECT_EQ(cfg.blocks[sortedDominators[3][0]].label, "body");
      EXPECT_EQ(cfg.blocks[sortedDominators[3][1]].label, "entry");
      EXPECT_EQ(cfg.blocks[sortedDominators[3][2]].label, "loop");
      EXPECT_EQ(cfg.blocks[sortedDominators[3][3]].label, "then");

      // .endif
      EXPECT_EQ(sortedDominators[4].size(), 4); // .endif is dominated by .body, .entry, .loop, and itself
      EXPECT_EQ(cfg.blocks[sortedDominators[4][0]].label, "body");
      EXPECT_EQ(cfg.blocks[sortedDominators[4][1]].label, "endif");
      EXPECT_EQ(cfg.blocks[sortedDominators[4][2]].label, "entry");
      EXPECT_EQ(cfg.blocks[sortedDominators[4][3]].label, "loop"); 

      // .exit
      EXPECT_EQ(sortedDominators[5].size(), 3); // .exit is dominated by .entry, .exit, and .loop
      EXPECT_EQ(cfg.blocks[sortedDominators[5][0]].label, "entry");
      EXPECT_EQ(cfg.blocks[sortedDominators[5][1]].label, "exit");
      EXPECT_EQ(cfg.blocks[sortedDominators[5][2]].label, "loop");
    }
  });
}

TEST(TypeVerificationTests, VerifyDominanceTree) {
  std::string filePath = "../bril/examples/test/dom/loopcond.bril";

  std::println("Reading Bril program from input file: {}", filePath);

  // Test that we can load the file and create a Program from it
  EXPECT_NO_THROW({
    json j = brilFromFile(filePath);
    Program program(j);

    std::println("Program has {} functions.", program.functions.size());

    for (auto function : program.functions) {
      ControlFlowGraph cfg(function);
      DominanceTree tree(cfg);

      /*
        {
          "body": [
            "endif",
            "then"
          ],
          "endif": [],
          "entry": [
            "loop"
          ],
          "exit": [],
          "loop": [
            "body",
            "exit"
          ],
          "then": []
        }
      */

      // .entry
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["entry"]].size(), 1); // .entry imm dominates .loop
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["entry"]].contains(tree.blockLabel2Index["loop"]), true); // .entry imm dominates .loop

      // .loop
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["loop"]].size(), 2); // .loop imm dominates .body and .exit
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["loop"]].contains(tree.blockLabel2Index["body"]), true); // .loop imm dominates .body
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["loop"]].contains(tree.blockLabel2Index["exit"]), true); // .loop imm dominates .exit
            
      // .body
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["body"]].size(), 2); // .body imm dominates .then and .endif
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["body"]].contains(tree.blockLabel2Index["then"]), true); // .body imm dominates .then
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["body"]].contains(tree.blockLabel2Index["endif"]), true); // .body imm dominates .endif

      // .then
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["then"]].size(), 0); // .then imm dominates no one

      // .endif
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["endif"]].size(), 0); // .endif imm dominates no one

      // .exit
      EXPECT_EQ(tree.immediatelyDominatedByMe[tree.blockLabel2Index["exit"]].size(), 0); // .exit imm dominates no one
    }
  });
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
// NOLINTEND(*)
