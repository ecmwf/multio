/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#pragma once

#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/message/Message.h"

namespace multio::grib2MarsMisc {

enum class MessageDisposition {
    Encode,
    CopyGrib2Verbatim,
    CopyExceptMatched,
    CopyInvalidMessage,
    CopyDiscipline192,
    CopyTimespanNonPositive,
    SkipExcluded,
    SkipFilteredOut,
    SkipInvalidMessage,
    SkipDiscipline192,
    SkipTimespanNonPositive,
    FailToExtract,
    FailToEncode,
    FailToArchive,
};

struct ExtractedMsg {
    MessageDisposition disposition{MessageDisposition::FailToExtract};
    eckit::LocalConfiguration mars;
    eckit::LocalConfiguration misc;
    std::vector<double> values;
};

ExtractedMsg grib2MarsMisc(const eckit::message::Message& msg, const eckit::LocalConfiguration& options);

}  // namespace multio::grib2MarsMisc
