#include <sstream>
#include <string_view>

#include "Core.h"
#include "Format.h"

namespace {
Type typeFromStr(const std::string_view str) {
  if (str == "int") {
    return IntType{};
  }

  if (str == "bool") {
    return BoolType{};
  }

  return VoidType{};
}
} // namespace

Program::Program(json &j) {
  for (const auto &func : j["functions"]) {
    Function function{
        .name = func["name"],
        .arguments =
            func.value("args", std::vector<json>{}) |
            std::views::transform([](const json &arg) {
              return Argument{.name = arg["name"],
                              .type = typeFromStr(arg["type"].get_ref<const std::string &>())};
            }) |
            std::ranges::to<std::vector<Argument>>(),
        .type = typeFromStr(func.value("type", "")),
        .instrs = std::vector<Code>{},
        .location = EmptyLoc{}};

    for (const auto &instr : func["instrs"]) {
      auto opName = instr.value("op", "");
      auto args = instr.value("args", std::vector<std::string>{});
      auto funcs = instr.value("funcs", std::vector<std::string>{});
      auto labels = instr.value("labels", std::vector<std::string>{});
      auto dest = instr.value("dest", "");
      auto type = typeFromStr(instr.value("type", ""));

      auto newCode = [&]() -> Code {
        if (opName.empty()) {
          return Label{.name = instr["label"], .location = EmptyLoc{}};
        }

        if (opName == "const") {
          return ConstantInstr{.dest = dest,
                               .type = type,
                               .value = instr["value"].is_boolean()
                                            ? instr["value"].get<bool>()
                                            : instr["value"].get<int64_t>(),
                               .location = EmptyLoc{}};
        }

        if (!dest.empty()) {
          return ValueInstr{.op = opName,
                            .dest = dest,
                            .type = type,
                            .args = args,
                            .funcs = funcs,
                            .labels = labels,
                            .location = EmptyLoc{}};
        }

        return EffectInstr{.op = opName,
                           .args = args,
                           .funcs = funcs,
                           .labels = labels,
                           .location = EmptyLoc{}};
      }();

      function.instrs.push_back(newCode);
    }

    // std::println("Function: {}", function);
    functions.push_back(function);
  }
}
