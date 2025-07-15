#pragma once

#include <variant>

namespace template_enum {
    template<typename... Variants>
    class Enum {
    public:
        template<typename T>
        requires (!std::same_as<std::decay_t<T>, Enum>)
        Enum(T&& value) : data(std::forward<T>(value)) {}

        template<typename T>
        static Enum make(T&& value) {
            return Enum{std::forward<T>(value)};
        }
        
        template<typename T>
        bool is() const {
            return std::holds_alternative<T>(data);
        }
        
        template<typename T>
        T& as() {
            if (!is<T>()) {
                throw std::runtime_error("Invalid enum variant access");
            }
            return std::get<T>(data);
        }
        
        template<typename T>
        const T& as() const {
            if (!is<T>()) {
                throw std::runtime_error("Invalid enum variant access");
            }
            return std::get<T>(data);
        }
        
        template<typename Self, typename... Visitors>
        auto match(this Self&& self, Visitors&&... visitors) {
            return std::visit(overload{std::forward<Visitors>(visitors)...}, std::forward<Self>(self).data);
        }
        
    private:
        std::variant<Variants...> data;

        template<class... Ts> struct overload : Ts... { using Ts::operator()...; };
        template<class... Ts> overload(Ts...) -> overload<Ts...>;
    };
}
