/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


/// @author Philipp Geier

#include "eckit/testing/Test.h"
#include "multio/message/Metadata.h"
#include "multio/message/Parametrization.h"
#include "multio/util/VariantHelpers.h"


namespace multio::test {

using multio::message::BaseMetadata;
using multio::message::Metadata;
using multio::message::MetadataException;
using multio::message::MetadataKeyException;
using multio::message::MetadataMissingKeyException;
using multio::message::MetadataTypes;
using multio::message::MetadataValue;
using multio::message::MetadataWrongTypeException;
using multio::message::Null;
using multio::message::Parametrization;


CASE("Test update parametrization") {
    Parametrization::instance().clear();
    Parametrization::instance().update(Metadata{{"a", 1}, {"b", 1.0}});

    EXPECT_NO_THROW(Parametrization::instance().update(Metadata{{"a", 1}, {"b", 1.0}}));
    EXPECT_THROWS_AS(Parametrization::instance().update(Metadata{{"a", 2}}), MetadataException);

    const BaseMetadata& par = Parametrization::instance().get();
    EXPECT(par.get<std::int64_t>("a") == 1);
    EXPECT(par.get<double>("b") == 1.0);

    Metadata m;
    EXPECT(m.get<std::int64_t>("a") == 1);
    EXPECT(m.get<double>("b") == 1.0);


    unsigned char data[5] = {0x1, 0x2, 0x3, 0x4, 0x5};
    unsigned char data2[5] = {0x1, 0x0, 0x0, 0x0, 0x5};
    EXPECT_NO_THROW(Parametrization::instance().update("domain", data, 5));
    EXPECT_THROWS_AS(Parametrization::instance().update("domain", data2, 5), MetadataException);
};


}  // namespace multio::test

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
