#include "MultioGrib2.h"
#include "MultioGrib2Test.h"

#include <exception>
#include <iostream>

template <class... Ts>
struct Overloaded : Ts... {
    using Ts::operator()...;
};

template <class... Ts>
Overloaded(Ts...) -> Overloaded<Ts...>;

template <typename T>
std::ostream& operator<<(std::ostream& os, const multio::grib2::Result<T>& dt) {
    std::visit(Overloaded{[&](const T& v) { os << v; },
                          [&](const multio::grib2::Error& v) { os << "Error{" << v.msg << "}"; }},
               dt);
    return os;
}

using StrDict = std::unordered_map<std::string, std::string>;

std::ostream& operator<<(std::ostream& os, const StrDict& dict) {
    bool isStart = true;
    std::cerr << "{";
    for (auto it = dict.cbegin(); it != dict.end(); ++it) {
        if (!isStart) {
            std::cerr << ", ";
        }
        std::cerr << it->first << ": " << it->second;
        isStart = false;
    }
    std::cerr << "}";
    return os;
}

std::optional<std::string> lookUp(const StrDict& dict, const std::string& key) noexcept {
    auto it = dict.find(key);
    if (it == dict.end()) {
        return std::nullopt;
    }
    return it->second;
}

decltype(auto) makeLookupAble(const StrDict& dict) {
    return [dict](const std::string& key) { return lookUp(dict, key); };
}

int main() {
    multio::grib2::Grib2ProductHandler<> grib2ProductHandler;
    std::cout << "PDT: "
              << grib2ProductHandler.inferProductDefinitionTemplateNumber(
                     makeLookupAble({{"timeExtent", "pointInTime"}, {"forecastType", "individualEnsemble"}}))
              << std::endl;

    for (const auto& pdtAndSelector : multio::grib2::test::mappedPdtAndSelectors) {
        std::visit(
            Overloaded{[&pdtAndSelector](const unsigned int& productDefinitionTemplateNumber) {
                           if (productDefinitionTemplateNumber == pdtAndSelector.productDefinitionTemplateNumber) {
                               std::cout << "Mapped productDefinitionTemplateNumber " << productDefinitionTemplateNumber
                                         << " from " << pdtAndSelector.selector << std::endl;
                           }
                           else {
                               std::cerr << "Wrongly infered productDefinitionTemplateNumber "
                                         << productDefinitionTemplateNumber
                                         << " != " << pdtAndSelector.productDefinitionTemplateNumber << " for selector "
                                         << pdtAndSelector.selector << std::endl;
                           }
                       },
                       [&pdtAndSelector](const multio::grib2::Error& error) {
                           std::cerr << "Error mapping selector " << pdtAndSelector.selector << ": " << error.msg
                                     << std::endl;
                       }},
            grib2ProductHandler.inferProductDefinitionTemplateNumber(makeLookupAble(pdtAndSelector.selector)));
    }
}
