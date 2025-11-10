/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <array>
#include <cstdint>
#include <optional>
#include <tuple>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/testing/Test.h"

#include "multio/util/config/Parser.h"
#include "multio/util/config/detail/Parser.h"


namespace multio::test {


template <typename TIn, typename TOut>
void testValidConversion(TIn inputValue, TOut outputValue, TOut defaultValue) {
    {
        // Given required value
        eckit::LocalConfiguration config;
        config.set("foo", inputValue);

        TOut foo;
        EXPECT_NO_THROW(util::config::detail::parseRequiredEntry(foo, "foo", config));
        EXPECT(foo == outputValue);
    }

    {
        // Missing required value
        eckit::LocalConfiguration config;

        TOut foo;
        EXPECT_THROWS(util::config::detail::parseRequiredEntry(foo, "foo", config));
    }

    {
        // Given optional value
        eckit::LocalConfiguration config;
        config.set("foo", inputValue);

        std::optional<TOut> foo;
        EXPECT_NO_THROW(util::config::detail::parseOptionalEntry(foo, "foo", config));
        EXPECT(foo);
        EXPECT(*foo == outputValue);
    }

    {
        // Missing optional value
        eckit::LocalConfiguration config;

        std::optional<TOut> foo;
        EXPECT_NO_THROW(util::config::detail::parseOptionalEntry(foo, "foo", config));
        EXPECT(!foo);
    }

    {
        // Default value
        eckit::LocalConfiguration config;
        config.set("foo", inputValue);

        TOut foo = defaultValue;
        EXPECT_NO_THROW(util::config::detail::parseOptionalEntry(foo, "foo", config));
        EXPECT(foo == outputValue);
    }

    {
        // Missing default value
        eckit::LocalConfiguration config;

        TOut foo = defaultValue;
        EXPECT_NO_THROW(util::config::detail::parseOptionalEntry(foo, "foo", config));
        EXPECT(foo == defaultValue);
    }
}


template <typename TIn, typename TOut>
void testInvalidConversion(TIn inputValue) {
    eckit::LocalConfiguration config;
    config.set("foo", inputValue);

    TOut foo;
    EXPECT_THROWS(util::config::detail::parseRequiredEntry(foo, "foo", config));
}


// String value

CASE("parse string -> string") {
    testValidConversion<std::string, std::string>("Hello, World!", "Hello, World!", "Hello, Default!");
}


// Int value

CASE("parse int -> int") {
    testValidConversion<std::int64_t, std::int64_t>(42, 42, 999);
}

CASE("parse string -> int") {
    testValidConversion<std::string, std::int64_t>("42", 42, 999);
}

CASE("parse invalid string -> int") {
    testInvalidConversion<std::string, std::int64_t>("");
    testInvalidConversion<std::string, std::int64_t>("42.42");
}


// Double value

CASE("parse double -> double") {
    testValidConversion<double, double>(42.42, 42.42, 999.0);
}

CASE("parse int -> double") {
    testValidConversion<std::int64_t, double>(42, 42.0, 999.0);
}

CASE("parse string (decimal) -> double") {
    testValidConversion<std::string, double>("42.42", 42.42, 999.0);
}

CASE("parse string (integer) -> double") {
    testValidConversion<std::string, double>("42", 42.0, 999.0);
}

CASE("parse invalid string -> double") {
    testInvalidConversion<std::string, double>("");
    testInvalidConversion<std::string, double>("42,42");  // This can be locale dependent
}


// Boolean value

CASE("parse bool -> bool") {
    testValidConversion<bool, bool>(true, true, true);
    testValidConversion<bool, bool>(true, true, false);
    testValidConversion<bool, bool>(false, false, true);
    testValidConversion<bool, bool>(false, false, false);
}

CASE("parse string -> bool") {
    testValidConversion<std::string, double>("1", true, true);
    testValidConversion<std::string, double>("1", true, false);
    testValidConversion<std::string, double>("0", false, true);
    testValidConversion<std::string, double>("0", false, false);
}

CASE("parse invalid string -> bool") {
    testInvalidConversion<std::string, bool>("");
    testInvalidConversion<std::string, bool>("2");
    testInvalidConversion<std::string, bool>("true");
    testInvalidConversion<std::string, bool>("false");
    testInvalidConversion<std::string, bool>("on");
    testInvalidConversion<std::string, bool>("off");
}


// Enum value

enum class FancyEnum
{
    Rock,
    Paper,
    Scissors,
};

CASE("parse string -> enum") {
    testValidConversion<std::string, FancyEnum>("paper", FancyEnum::Paper, FancyEnum::Scissors);
}

CASE("parse invalid int -> bool") {
    testInvalidConversion<std::int64_t, FancyEnum>(0);
}

CASE("parse invalid string -> bool") {
    testInvalidConversion<std::string, FancyEnum>("");
    testInvalidConversion<std::string, FancyEnum>("Paper");
    testInvalidConversion<std::string, FancyEnum>("mountain");
}


// Configuration to struct parsing

struct MyConfig {
    std::string name;
    bool enabled{true};
    std::optional<FancyEnum> choice;

    static constexpr auto fields_ = std::make_tuple(multio::util::config::requiredEntry("name", &MyConfig::name),
                                                    multio::util::config::optionalEntry("enabled", &MyConfig::enabled),
                                                    multio::util::config::optionalEntry("choice", &MyConfig::choice));
};

CASE("parse configuration  -- valid") {
    eckit::LocalConfiguration config;
    config.set("name", "my-config");

    const auto myConfig = util::config::parseConfig<MyConfig>(config);
    EXPECT(myConfig.name == "my-config");
    EXPECT(myConfig.enabled);
    EXPECT(!myConfig.choice);
}

CASE("parse configuration -- missing required key") {
    eckit::LocalConfiguration config;

    EXPECT_THROWS(util::config::parseConfig<MyConfig>(config));
}

CASE("parse configuration -- unknown key") {
    eckit::LocalConfiguration config;
    config.set("name", "my-config");
    config.set("foo", "bar");

    EXPECT_THROWS(util::config::parseConfig<MyConfig>(config));
}


}  // namespace multio::test


template <>
struct multio::util::config::detail::EnumTrait<multio::test::FancyEnum> {
    static constexpr std::array values{
        std::pair{multio::test::FancyEnum::Rock, "rock"},
        std::pair{multio::test::FancyEnum::Paper, "paper"},
        std::pair{multio::test::FancyEnum::Scissors, "scissors"},
    };
};


int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
