/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <unistd.h>
#include <cstring>

#include "eckit/filesystem/TmpFile.h"
#include "eckit/message/Message.h"
#include "eckit/testing/Test.h"

#include "multio/api/multio_c.h"
#include "multio/api/multio_c_cpp_utils.h"

#include "TestDataContent.h"
#include "TestHelpers.h"

namespace multio {
namespace test {

CASE("Metadata is created and delected sucessfully") {
    // DataSinkFactory::list appends the results to a ostream&, so we need to extract them.
    multio_metadata_t* mdp = nullptr;
    int err;
    err = multio_new_metadata(&mdp);
    EXPECT(err == MULTIO_SUCCESS);
    err = multio_delete_metadata(mdp);
    EXPECT(err == MULTIO_SUCCESS);
}

CASE("Metadata can be moved") {
    // DataSinkFactory::list appends the results to a ostream&, so we need to extract them.
    using multio::message::Metadata;
    multio_metadata_t* mdp = nullptr;
    int err;
    err = multio_new_metadata(&mdp);
    EXPECT(err == MULTIO_SUCCESS);

    err = multio_metadata_set_int_value(mdp, "testKey", 123);
    EXPECT(err == MULTIO_SUCCESS);

    Metadata* md_pCpp = multio_from_c(mdp);

    EXPECT(md_pCpp->getInt("testKey") == 123);
    Metadata md_moved = std::move(*md_pCpp);

    EXPECT(!md_moved.empty());
    // EXPECT(md_pCpp->empty()); // THIS IS FAILING; Change request: https://jira.ecmwf.int/browse/ECKIT-601

    err = multio_delete_metadata(mdp);
    EXPECT(err == MULTIO_SUCCESS);
}
}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
