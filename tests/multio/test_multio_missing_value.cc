/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <cstdint>
#include <limits>
#include <string>

#include "eckit/exception/Exceptions.h"
#include "eckit/testing/Test.h"

#include "multio/message/Metadata.h"
#include "multio/message/MissingValue.h"
#include "multio/message/SharedMetadata.h"


namespace multio::test {

using multio::message::Metadata;
using multio::message::normaliseMissingValue;
using multio::message::SharedMetadata;

namespace {

// Not exactly representable as float: a float field filled with it holds -9999.990234375
constexpr double inexact = -9999.99;
constexpr double inexactAsFloat = static_cast<double>(static_cast<float>(inexact));

SharedMetadata makeMetadata(double missingValue) {
    Metadata md;
    md.set("misc-missingValue", missingValue);
    md.set("missingValue", missingValue);
    md.set("misc-bitmapPresent", true);
    md.set("param", std::int64_t{167});
    return SharedMetadata{std::move(md)};
}

}  // namespace


CASE("Missing value is rounded to the precision of a single precision field") {
    EXPECT(inexactAsFloat != inexact);

    const auto md = makeMetadata(inexact);
    const auto normalised = normaliseMissingValue<float>(md);

    for (const char* key : {"misc-missingValue", "missingValue"}) {
        const auto missing = normalised.read().get<double>(key);

        EXPECT(missing == inexactAsFloat);
        // This is the comparison performed by the consumers (actions, metkit, ecCodes)
        EXPECT(static_cast<double>(static_cast<float>(inexact)) == missing);
    }

    // Other keys are preserved
    EXPECT(normalised.read().get<bool>("misc-bitmapPresent"));
    EXPECT(normalised.read().get<std::int64_t>("param") == 167);
}


CASE("The metadata of the caller is not modified") {
    const auto md = makeMetadata(inexact);
    const auto normalised = normaliseMissingValue<float>(md);

    EXPECT(md.read().get<double>("misc-missingValue") == inexact);
    EXPECT(md.read().get<double>("missingValue") == inexact);
    EXPECT(&md.read() != &normalised.read());

    // The same metadata can subsequently be used for a double precision field
    const auto normalisedDouble = normaliseMissingValue<double>(md);
    EXPECT(normalisedDouble.read().get<double>("misc-missingValue") == inexact);
}


CASE("Metadata is shared if nothing needs to be rounded") {
    // Exactly representable as float
    for (double exact : {-9999.0, 0.0, 1.5, static_cast<double>(std::numeric_limits<float>::max())}) {
        const auto md = makeMetadata(exact);
        const auto normalised = normaliseMissingValue<float>(md);
        EXPECT(&md.read() == &normalised.read());
        EXPECT(normalised.read().get<double>("misc-missingValue") == exact);
    }

    // Double precision fields are never modified
    {
        const auto md = makeMetadata(inexact);
        const auto normalised = normaliseMissingValue<double>(md);
        EXPECT(&md.read() == &normalised.read());
        EXPECT(normalised.read().get<double>("misc-missingValue") == inexact);
    }

    // No missing value at all
    {
        Metadata raw;
        raw.set("param", std::int64_t{167});
        const SharedMetadata md{std::move(raw)};
        const auto normalised = normaliseMissingValue<float>(md);
        EXPECT(&md.read() == &normalised.read());
        EXPECT(normalised.read().find("misc-missingValue") == normalised.read().end());
        EXPECT(normalised.read().find("missingValue") == normalised.read().end());
    }
}


CASE("Only the present key is normalised") {
    Metadata raw;
    raw.set("misc-missingValue", inexact);
    const SharedMetadata md{std::move(raw)};
    const auto normalised = normaliseMissingValue<float>(md);

    EXPECT(normalised.read().get<double>("misc-missingValue") == inexactAsFloat);
    EXPECT(normalised.read().find("missingValue") == normalised.read().end());
}


CASE("Values that are not floating point numbers are left alone") {
    Metadata raw;
    raw.set("misc-missingValue", std::int64_t{-9999});
    raw.set("missingValue", std::string{"-9999.99"});
    const SharedMetadata md{std::move(raw)};
    const auto normalised = normaliseMissingValue<float>(md);

    EXPECT(&md.read() == &normalised.read());
    EXPECT(normalised.read().get<std::int64_t>("misc-missingValue") == -9999);
    EXPECT(normalised.read().get<std::string>("missingValue") == "-9999.99");
}


CASE("NaN and infinite missing values are passed through") {
    for (double special : {std::numeric_limits<double>::quiet_NaN(), std::numeric_limits<double>::infinity(),
                           -std::numeric_limits<double>::infinity()}) {
        const auto md = makeMetadata(special);
        const auto normalised = normaliseMissingValue<float>(md);
        EXPECT(&md.read() == &normalised.read());
    }
}


CASE("A missing value outside of the single precision range is rejected") {
    EXPECT_THROWS_AS(normaliseMissingValue<float>(makeMetadata(1.0e300)), eckit::UserError);
    EXPECT_THROWS_AS(normaliseMissingValue<float>(makeMetadata(-1.0e300)), eckit::UserError);
    EXPECT_NO_THROW(normaliseMissingValue<double>(makeMetadata(1.0e300)));
}

}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
