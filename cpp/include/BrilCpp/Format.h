#pragma once

#include <format>
#include <print>
#include <ranges>

#include "Core.h"
#include "ControlFlowGraph.h"

namespace std {
  template<> struct std::formatter<Location> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }
  
    auto format(const Location &location, std::format_context& ctx) const {
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
  
  template<> struct std::formatter<Type> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }
    auto format(const Type &type, std::format_context& ctx) const {
      return type.match(
          [&](const VoidType &) { return std::format_to(ctx.out(), "VoidType"); },
          [&](const IntType &) { return std::format_to(ctx.out(), "IntType"); },
          [&](const BoolType &) {
            return std::format_to(ctx.out(), "BoolType");
          });
    }
  };
  
  template<> struct std::formatter<Value> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }
  
    auto format(const Value &value, std::format_context& ctx) const {
      constexpr auto s = R"({})";
      return value.match(
          [&](int64_t v) {
            return std::format_to(ctx.out(), s, v);
          },
          [&](bool v) {
            return std::format_to(ctx.out(), s, v);
          });
    }
  };
  
  // TODO: Add formatting based on instruction type
  template<> struct std::formatter<Instruction> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }
  
    auto format(const Instruction &instr, std::format_context& ctx) const {
      return instr.match(
          [&](const ConstantInstr &c) {
            constexpr auto s = R"({}: {} = {})";
            return std::format_to(ctx.out(), s, c.dest, c.type, c.value);
          },
          [&](const ValueInstr &v) {
            constexpr auto s = R"({}: {} = {}, args: {}, funcs: {}, labels: {})";
            return std::format_to(ctx.out(), s, v.dest, v.type, v.op, v.args, v.funcs, v.labels);
          },
          [&](const EffectInstr &e) {
            constexpr auto s = R"({}, args: {}, funcs: {}, labels: {})";
            return std::format_to(ctx.out(), s, e.op, e.args, e.funcs, e.labels);
          });
    }
  };
  
  template<> struct std::formatter<Label> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }
  
    auto format(const Label& arg, std::format_context& ctx) const {
      constexpr auto s = R"({})";
      return std::format_to(ctx.out(), s, arg.name, arg.location);
    }
  };
  
  template<> struct std::formatter<Code> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }
  
    auto format(const Code& arg, std::format_context& ctx) const {
      return arg.match(
        [&](const Instruction& instr) {
          return std::format_to(ctx.out(), "{}", instr);
        },
        [&](const Label& label) {
          return std::format_to(ctx.out(), "{}", label);
        }
      );
    }
  };
  
  template<> struct std::formatter<Argument> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }
  
    auto format(const Argument& arg, std::format_context& ctx) const {
      constexpr auto s = R"({}: {})";
      return std::format_to(ctx.out(), s, arg.name, arg.type);
    }
  };
  
template <typename T>
  requires requires(const T& t) {
    std::print("{}", t);
  }
struct std::formatter<std::vector<T>> {
  constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }

  auto format(const std::vector<T>& vec, std::format_context& ctx) const {
    constexpr auto itemFormat = R"({})";
    constexpr auto commaWithNewLine = R"(,
          )";
    
    for (auto [index, toPrint] : std::views::zip(std::views::iota(0), vec)) {
      auto out = std::format_to(ctx.out(), itemFormat, toPrint);
      if (index < vec.size()-1) {
        std::format_to(out, commaWithNewLine);
      }
    }
    
    return ctx.out();
  }
};

  template<> struct std::formatter<BasicBlock> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }

    auto format(const BasicBlock& block, std::format_context& ctx) const {
      constexpr auto s = R"(
      {}: 
          {})";
      return std::format_to(ctx.out(), s, block.label, block.instrs);
    }
  };

  template<> struct std::formatter<ControlFlowGraph> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }

    auto format(const ControlFlowGraph& cfg, std::format_context& ctx) const {
      constexpr auto s = R"(
ControlFlowGraph {{ 
  arguments: {}, 
  blocks: {} 
}}
)";
      return std::format_to(ctx.out(), s, cfg.arguments, cfg.blocks);
    }
  };
  
  template<> struct std::formatter<Function> {
    constexpr auto parse(std::format_parse_context& ctx) { return ctx.begin(); }
  
    auto format(const Function& func, std::format_context& ctx) const {
      return std::format_to(
          ctx.out(),
          "Function {{ name: {}, arguments: {}, type: {}, instrs: {} }}",
          func.name, func.arguments, func.type, func.instrs);
    }
  };
} // namespace std
  