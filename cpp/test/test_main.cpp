#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "BrilCpp/Core.h"

TEST(BrilProgram, CreateFromJson) {
  std::string brilProgram = R"({
  "functions": [
    {
      "instrs": [
        {
          "dest": "v0",
          "op": "const",
          "type": "int",
          "value": 9,
          "args" : []
        },
        {
          "dest": "v1",
          "op": "const",
          "type": "int",
          "value": -20,
          "args" : []
        },
        {
          "args": [
            "v0",
            "v1"
          ],
          "dest": "res",
          "op": "div",
          "type": "int"
        },
        {
          "args": [
            "res"
          ],
          "op": "print"
        }
      ],
      "name": "main"
    }
  ]
})";

  // Parse JSON string
  json j = json::parse(brilProgram);

  // Create Program from JSON
  EXPECT_NO_THROW({ Program program(j); });

  // Test that the program can be created successfully
  Program program(j);

  // The program should be created without throwing exceptions
  // Additional assertions could be added here to verify the program structure
  // if the Program class had public accessors for its data
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
