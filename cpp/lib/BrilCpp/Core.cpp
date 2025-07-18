#include <format>
#include <print>
#include <sstream>
#include <string_view>

#include "Core.h"

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

namespace std {
template <> struct std::formatter<Location> {
  constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }

  auto format(const Location &location, std::format_context &ctx) const {
    return location.match(
        [&](const EmptyLoc &) { return std::format_to(ctx.out(), "EmptyLoc"); },
        [&](const Loc &loc) {
          return std::format_to(ctx.out(), "Loc {{ start: {}, end: {} }}",
                                loc.start, loc.end);
        },
        [&](const LocWithFile &locWithFile) {
          return std::format_to(
              ctx.out(), "LocWithFile {{ start: {}, end: {}, file: {} }}",
              locWithFile.loc.start, locWithFile.loc.end, locWithFile.file);
        });
  }
};

template <> struct std::formatter<Type> {
  constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }
  auto format(const Type &type, std::format_context &ctx) const {
    return type.match(
        [&](const VoidType &) { return std::format_to(ctx.out(), "VoidType"); },
        [&](const IntType &) { return std::format_to(ctx.out(), "IntType"); },
        [&](const BoolType &) {
          return std::format_to(ctx.out(), "BoolType");
        });
  }
};

template <> struct std::formatter<Value> {
  constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }

  auto format(const Value &value, std::format_context &ctx) const {
    return value.match(
        [&](int64_t v) {
          return std::format_to(ctx.out(), "Value<int64_t>({})", v);
        },
        [&](bool v) {
          return std::format_to(ctx.out(), "Value<bool>({})", v);
        });
  }
};

template <> struct std::formatter<Instruction> {
  constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }

  auto format(const Instruction &instr, std::format_context &ctx) const {
    return instr.match(
        [&](const ConstantInstr &c) {
          return std::format_to(
              ctx.out(), "ConstantInstr {{ dest: {}, type: {}, value: {} }}",
              c.dest, c.type, c.value);
        },
        [&](const ValueInstr &v) {
          return std::format_to(ctx.out(),
                                "ValueInstr {{ op: {}, dest: {}, type: {}, "
                                "args: {}, funcs: {}, labels: {} }}",
                                v.op, v.dest, v.type, v.args, v.funcs,
                                v.labels);
        },
        [&](const EffectInstr &e) {
          return std::format_to(
              ctx.out(),
              "EffectInstr {{ op: {}, args: {}, funcs: {}, labels: {} }}", e.op,
              e.args, e.funcs, e.labels);
        });
  }
};

template <> struct std::formatter<Argument> {
  constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }

  auto format(const Argument &arg, std::format_context &ctx) const {
    return std::format_to(ctx.out(), "Argument {{ name: {}, type: {} }}",
                          arg.name, arg.type);
  }
};

template <typename T>
  requires requires(std::format_context &ctx, const T &t) {
    std::print("{}", t);
  }
// requires std::formattable<T, char>
struct std::formatter<std::vector<T>> {
  constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }

  auto format(const std::vector<T> &vec, std::format_context &ctx) const {
    // stringstream ss;

    // ss << "[";
    // for (auto& toPrint : vec) {
    //     ss << std::format("{}, ", toPrint);
    // }
    // ss << "]";

    // return std::format_to(ctx.out(), "{}", ss.str());

    auto out = std::format_to(ctx.out(), "[");
    for (auto &toPrint : vec) {
      out = std::format_to(out, "{}", toPrint);
    }
    return std::format_to(out, "]");
  }
};

template <> struct std::formatter<Function> {
  constexpr auto parse(std::format_parse_context &ctx) { return ctx.begin(); }

  auto format(const Function &func, std::format_context &ctx) const {
    return std::format_to(
        ctx.out(),
        "Function {{ name: {}, arguments: {}, type: {}, instrs: {} }}",
        func.name, func.arguments, func.type, func.instrs);
  }
};
} // namespace std

Program::Program(json &j) {
  for (const auto &func : j["functions"]) {
    Function function{
        .name = func["name"],
        .arguments =
            func.value("args", std::vector<std::string>{}) |
            std::views::transform([](const json &arg) {
              return Argument{.name = arg["name"],
                              .type = typeFromStr(
                                  arg["type"].get_ref<const std::string &>())};
            }) |
            std::ranges::to<std::vector<Argument>>(),
        .type = typeFromStr(func.value("type", "")),
        .instrs = std::vector<Instruction>{},
        .location = EmptyLoc{}};

    for (const auto &instr : func["instrs"]) {
      auto opName = instr["op"];
      auto args = instr.value("args", std::vector<std::string>{});
      auto funcs = instr.value("funcs", std::vector<std::string>{});
      auto labels = instr.value("labels", std::vector<std::string>{});
      auto dest = instr.value("dest", "");
      auto type = typeFromStr(instr.value("type", ""));

      auto newInstr = [&]() -> Instruction {
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

      function.instrs.push_back(newInstr);
    }

    std::println("Function: {}", function);
    functions.push_back(function);
  }
}
