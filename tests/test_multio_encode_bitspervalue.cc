/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <fstream>

#include "eckit/testing/Test.h"
#include "eckit/filesystem/TmpFile.h"
#include "eckit/io/DataHandle.h"

#include "multio/ifsio/ifsio.h"
#include "multio/ifsio/EncodeBitsPerValue.h"

namespace multio {
namespace test {

/// NOTE: the paramids used in this test are absolutely artificial and invented and do not represent actual codes

std::string create_table() {
    std::stringstream ss;
    ss << R"json(
          {
            "sfc": {
                "precipitation": {
                    "decimalScaleFactor": 1,
                    "paramIDs": [128144]
                }
            },
            "pl": {
                "chems": {
                    "bitsPerValue": 18,
                    "paramIDs": [123, 124]
                },
                "clouds": {
                    "bitsPerValue": 14,
                    "paramIDs": [150139, 150138, 150137]
                }
            },
            "ml": {
                "precipitation": {
                    "bitsPerValue": 8,
                    "paramIDs": [124]
                }
            }
          }
          )json";
    return ss.str();
}


struct TestHarness {
    TestHarness(){

        std::string tablestr = create_table();

        std::unique_ptr<eckit::DataHandle> dh(table.fileHandle(true));
        dh->openForWrite(0);
        dh->write(tablestr.data(), tablestr.size());
        dh->close();

        path = table;

        ::setenv("MULTIO_CONFIG", "{sinks:[]}", 1);
        ::setenv("COMPR_FC_GP_ML", "1", 1);
        ::setenv("MULTIO_ENCODING_TABLE", path.c_str(), 1);
    }

    eckit::TmpFile table;
    std::string path;
};


//----------------------------------------------------------------------------------------------------------------------

CASE("default bitspervalue") {

    TestHarness test;

    int bpv = 0;
    int paramid = 0;

    double min = 200.;
    double max = 300.;

    std::string levtype{"p"};

    SECTION("0") {
      paramid = 0;  // returns error
      EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == -2);
    }

    SECTION("paramid = 130") {
        paramid = 130;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 16);
    }

    SECTION("paramid = 128130") {
        paramid = 128130;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 16);
    }

    SECTION("from table, paramid = 150139 with bitsPerValue") {
        paramid = 150139;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 14);
    }

    SECTION("from table, paramid = 128144 with decimalScaleFactor") {
        std::string levtype = "SFC";
        paramid = 128144;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 10);
    }

    SECTION("Cloud cover [cc] 248") {
      paramid = 248;
      EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
      EXPECT_EQUAL(bpv, 8);
    }

    SECTION("Cloud liquid water content [clwc] 246 and Cloud ice water content [ciwc] 247") {
      std::string levtype = "PL";
      paramid = 246;
      EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
      EXPECT_EQUAL(bpv, 12);
      paramid = 247;
      EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
      EXPECT_EQUAL(bpv, 12);
    }

    SECTION("210000 < paramid < 228000") {
      paramid = 210100;
      EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
      EXPECT_EQUAL(bpv, 24);
      paramid = 227130;
      EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
      EXPECT_EQUAL(bpv, 24);
    }

    SECTION("Cloudy brightness temperature [clbt] 260510 and Clear-sky brightness temperature [csbt] 260511") {
      paramid = 260510;
      EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
      EXPECT_EQUAL(bpv, 10);
      paramid = 260511;
      EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
      EXPECT_EQUAL(bpv, 10);
    }

    SECTION("Presence of env variable COMPR_FC_GP_ML for ML fields") {
      levtype = "p";
      paramid = 128130;
      EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
      EXPECT_EQUAL(bpv, 16);

      levtype = "ML";
      paramid = 128130;
      EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
      EXPECT_EQUAL(bpv, 10);
    }
}

CASE("repeated queries use cache") {

    TestHarness test;

    int bpv = 0;
    int paramid = 0;

    double min = 10.;
    double max = 50.;

    std::string levtype{"p"};

    SECTION("calling after error") {
        paramid = 0;  // returns error
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == -2);
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == -2);
    }

    SECTION("Repeat 3x -- paramid = 128131") {
        paramid = 128131;

        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 16);

        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 16);

        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 16);
    }


    SECTION("Repeat 2x -- 210000 < paramid < 228000") {
        paramid = 210100;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 24);

        paramid = 227130;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 24);

        paramid = 210100;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 24);

        paramid = 227130;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 24);
    }

    SECTION("Repeat 2x -- Presence of env variable COMPR_FC_GP_ML for ML fields") {
        levtype = "p";
        paramid = 128130;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 16);

        levtype = "ML";
        paramid = 128130;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 10);

        levtype = "PL";
        paramid = 128130;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 16);

        levtype = "m";
        paramid = 128130;
        EXPECT(imultio_encode_bitspervalue_(&bpv, &paramid, levtype.c_str(), levtype.size(), &min, &max) == 0);
        EXPECT_EQUAL(bpv, 10);
    }
}

//----------------------------------------------------------------------------------------------------------------------

CASE("Encode") {
    SECTION("Bits per value") {
        multio::Encoding encode;
        encode.bitsPerValue = 13;
        EXPECT_EQUAL(encode.computeBitsPerValue(200, 300), 13);
    }

    SECTION("Decimal Scale Factor") {
        multio::Encoding encode;
        encode.decimalScaleFactor = 1;
        EXPECT_EQUAL(encode.computeBitsPerValue(200, 300), 10);
    }

    SECTION("Precision") {
        multio::Encoding encode;
        encode.precision = 0.25;
        EXPECT_EQUAL(encode.computeBitsPerValue(200, 300), 9);
    }

}

//----------------------------------------------------------------------------------------------------------------------

}  // namespace test
}  // namespace multio

int main(int argc, char** argv) {
    return eckit::testing::run_tests(argc, argv);
}
