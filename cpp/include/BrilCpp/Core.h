#pragma once

#include <algorithm>
#include <nlohmann/json.hpp>
#include <ranges>
#include <string>
#include <variant>
#include <vector>

#include <better_variant.h>

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

using Location = bv::Variant<EmptyLoc, Loc, LocWithFile>;

/////////////////////////////////////////
///////////////// Types /////////////////
/////////////////////////////////////////
struct IntType {};
struct BoolType {};
struct VoidType {};

using Type = bv::Variant<VoidType, IntType, BoolType>;

//////////////////////////////////////////
///////////////// Values /////////////////
//////////////////////////////////////////
using Value = bv::Variant<int64_t, bool>;

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

using Instruction = bv::Variant<ConstantInstr, ValueInstr, EffectInstr>;

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
using Code = bv::Variant<Label, Instruction>;

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
