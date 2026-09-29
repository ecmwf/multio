/*
 * (C) Copyright 2025- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

/// @file
/// @brief Process one `UnitOfWork` through the message-level pipeline.

#include "multio/tools/grib-to-mtg2/ProcessOneUnitOfWork.h"

#include "multio/tools/grib-to-mtg2/ProcessOneMessage.h"
#include "multio/tools/grib-to-mtg2/Sink.h"
#include "multio/tools/grib-to-mtg2/Utils.h"

namespace multio::grib_to_mtg2 {

FileStageOutcomes processOneUnitOfWork(UnitOfWork& unitOfWork, const GlobalContext& context,
                                       GribToMtg2Sinks& writer) noexcept {
    FileStageOutcomes outcomes;
    outcomes.filename = unitOfWork.workUnit().filename;

    try {
        try {
            unitOfWork.open();
            outcomes.openFile.bump(OpenFileCode::Valid);
        }
        catch (...) {
            printTrappedErrorDisclaimer();
            outcomes.openFile.bump(OpenFileCode::OpenFailed);
            if (!unitOfWork.close()) {
                ++outcomes.nCloseFailures;
            }
            return outcomes;
        }

        while (unitOfWork.newMessageAvailable()) {
            std::unique_ptr<metkit::codes::CodesHandle> message;
            try {
                message = unitOfWork.nextMessage();
            }
            catch (...) {
                printTrappedErrorDisclaimer();
                outcomes.readMessage.bump(ReadMessageCode::ReadFailed);
                break;
            }

            if (!message) {
                break;
            }

            outcomes.readMessage.bump(ReadMessageCode::Valid);
            processOneMessage(*message, context, writer, outcomes);
        }

        try {
            writer.flush();
            outcomes.fileFlush.bump(FileFlushCode::Valid);
        }
        catch (...) {
            printTrappedErrorDisclaimer();
            outcomes.fileFlush.bump(FileFlushCode::FileFlushFailed);
        }
    }
    catch (...) {
        ++outcomes.nGenericProcessUnitOfWorkFailures;
    }

    if (!unitOfWork.close()) {
        ++outcomes.nCloseFailures;
    }

    return outcomes;
}

}  // namespace multio::grib_to_mtg2
