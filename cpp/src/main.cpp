#include <print>
#include <vector>

#include "BrilCpp/Core.h"
#include "BrilCpp/Enum.h"

/*
ENUM Instruction {
    ConstantInstr,
    ValueInstr,
    EffectInstr
}
*/

int main() {
    std::vector<Function> instrs{};

    auto func = Function {
        .name = "f",
        .arguments = std::vector<Argument>{},
        .type = IntType{},
        .instrs = std::vector<Instruction>{},
        .location = EmptyLoc{}
    };

    instrs.push_back(func);
    // instrs.emplace_back(func);

    // for (auto instr : instrs) {
        // std::println("{0}", func.instrs.size());
    // }
    // const Type type = IntType{};

    // std::println("{0}", std::is_copy_constructible_v<std::vector<Instruction>>);
    // std::println("{0}", std::is_trivially_copy_constructible_v<std::vector<Instruction>>);
    // std::println("{0}", std::is_default_constructible_v<std::vector<Instruction>>);

    // type.match(
    //     [](IntType& a) { std::println("Visiting IntType ref..."); },
    //     [](IntType&& a) { std::println("Visiting IntType R-Value ref..."); },
    //     [](const IntType& a) { std::println("Visiting IntType const ref..."); },
    //     [](BoolType& a) { std::println("Visiting BoolType ref..."); },
    //     [](const BoolType& a) { std::println("Visiting BoolType const ref..."); },
    //     [](const VoidType& a) { std::println("Visiting VoidType const ref..."); }
    // );

    // std::move(type).match(
    //     [](IntType& a) { std::println("Visiting IntType ref..."); },
    //     [](IntType&& a) { std::println("Visiting IntType R-Value ref..."); },
    //     [](const IntType& a) { std::println("Visiting IntType const ref..."); },
    //     [](const IntType&& a) { std::println("Visiting IntType const RValue ref..."); },
    //     [](BoolType& a) { std::println("Visiting BoolType ref..."); },
    //     [](const BoolType& a) { std::println("Visiting BoolType const ref..."); },
    //     [](const VoidType& a) { std::println("Visiting VoidType const ref..."); }
    // );
}
