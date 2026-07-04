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

#include <string>
#include <vector>

#include "multio/tools/utils/distGrib1ToGrib2Logging.h"

namespace multio::distGrib1ToGrib2 {

struct DistGrib1ToGrib2ReportPaths {
    std::string perFileLog;
    std::string byClassStreamTypeLevtypeLog;
    std::string byClassStreamTypeLog;
    std::string fullSuccessList;
    std::string skipSuccessList;
    std::string encodeFailureList;
    std::string archiveFailureList;
    std::string extractFailureList;
};

std::string serializeFileOutcomes(const std::vector<FileOutcome>& outcomes);
std::vector<FileOutcome> deserializeFileOutcomes(const std::string& payload);

DistGrib1ToGrib2ReportPaths makeReportPaths(const std::string& outputPrefix);
void writeOutcomeReports(const std::vector<FileOutcome>& outcomes, const DistGrib1ToGrib2ReportPaths& paths);

}  // namespace multio::distGrib1ToGrib2
