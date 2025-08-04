#pragma once

#include <algorithm>
#include <nlohmann/json.hpp>
#include <ranges>
#include <string>
#include <variant>
#include <vector>

#include "Enum.h"

using json = nlohmann::json;

/////////////////////////////////////////
/////////////// Locations ///////////////
/////////////////////////////////////////
using Position = std::tuple<int, int>;

struct Loc {
  Position start;
  Position end;
};

struct LocWithFile {
  Loc loc;
  std::string file;
};

struct EmptyLoc {};

using Location = template_enum::Enum<EmptyLoc, Loc, LocWithFile>;

/////////////////////////////////////////
///////////////// Types /////////////////
/////////////////////////////////////////
struct IntType {};
struct BoolType {};
struct VoidType {};

using Type = template_enum::Enum<VoidType, IntType, BoolType>;

//////////////////////////////////////////
///////////////// Values /////////////////
//////////////////////////////////////////
using Value = template_enum::Enum<int64_t, bool>;

//////////////////////////////////////////
////////////// Instructions //////////////
//////////////////////////////////////////
struct ConstantInstr {
  std::string dest;
  Type type;
  Value value;
  Location location;
};

struct ValueInstr {
  std::string op;
  std::string dest;
  Type type;
  std::vector<std::string> args;
  std::vector<std::string> funcs;
  std::vector<std::string> labels;
  Location location;
};

struct EffectInstr {
  std::string op;
  std::vector<std::string> args;
  std::vector<std::string> funcs;
  std::vector<std::string> labels;
  Location location;
};

using Instruction = template_enum::Enum<ConstantInstr, ValueInstr, EffectInstr>;

//////////////////////////////////////////
///////////////// Label //////////////////
//////////////////////////////////////////
struct Label {
  std::string name;
  Location location;
};

//////////////////////////////////////////
////////////////// Code //////////////////
//////////////////////////////////////////
using Code = template_enum::Enum<Label, Instruction>;

//////////////////////////////////////////
//////////////// Argument ////////////////
//////////////////////////////////////////
struct Argument {
  std::string name;
  Type type;
};

//////////////////////////////////////////
//////////////// Function ////////////////
//////////////////////////////////////////
struct Function {
  std::string name;
  std::vector<Argument> arguments;
  Type type;
  std::vector<Code> instrs;
  Location location;
};

//////////////////////////////////////////
//////////////// Program /////////////////
//////////////////////////////////////////
struct Program {
  Program(json &j);

  std::vector<Function> functions;
};
