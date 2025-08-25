#pragma once

#include <optional>
#include <variant>

template<typename... Ts>
struct Match : Ts... {
  using Ts::operator()...;
};

#define MATCH(...) Match {__VA_ARGS__}

namespace template_enum {
template<typename... Variants> class Enum {
public:
  template<typename T>
    requires(!std::same_as<std::decay_t<T>, Enum>)
  Enum(T&& value) : data(std::forward<T>(value)) {}

  template<typename T> static Enum make(T&& value) {
    return Enum{std::forward<T>(value)};
  }

  template<typename T> bool is() const {
    return std::holds_alternative<T>(data);
  }

  template<typename T> std::optional<T> tryAs(this auto&& self) {
    if (!self.template is<T>()) {
      return std::nullopt;
    }

    return std::get<T>(self.data);
  }

  template<typename T> T as(this auto&& self) {
    if (!self.template is<T>()) {
      throw std::runtime_error("Invalid enum variant access");
    }

    return std::get<T>(self.data);
  }

  template<typename Self, typename... Visitors>
  auto match(this Self&& self, Visitors&& ...visitors) {
    return std::visit(Match{std::forward<Visitors>(visitors)...},
                      std::forward<Self>(self).data);
  }

  template<typename Self, typename... Visitors>
  auto operator|(this Self&& self, Match<Visitors...> match) {
    return std::visit(match, std::forward<Self>(self).data);
  }

private:
  std::variant<Variants...> data;
};
} // namespace template_enum
