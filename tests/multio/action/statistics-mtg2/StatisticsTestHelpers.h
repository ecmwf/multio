/*
 * (C) Copyright 2026- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// Helpers to produce the input required by statistics-mtg2 (see src/multio/action/statistics-mtg2/CONTEXT.md).

#pragma once

#include <cstdint>
#include <optional>

#include "eckit/testing/Test.h"

#include "multio/message/Message.h"
#include "multio/message/Metadata.h"

#include "../../MultioTestEnvironment.h"


namespace multio::test::statistics_mtg2 {

inline constexpr std::int64_t INTEGRATION_STEP_IN_SECONDS = 600;

/// Send the simulation-start flush that statistics-mtg2 requires before any field. The flush is forwarded by the
/// action, so it is removed from the (initially empty) debug sink again.
inline void sendSimulationStart(MultioTestEnvironment& env, std::int64_t date, std::int64_t time = 0,
                                std::int64_t step = 0) {
    env.process({{message::Message::Tag::Flush,
                  {},
                  {},
                  {{"flushKind", "first-step"}, {"date", date}, {"time", time}, {"step", step}}}});
    EXPECT_EQUAL(env.debugSink().size(), 1);
    EXPECT(env.debugSink().front().tag() == message::Message::Tag::Flush);
    env.debugSink().pop();
}

/// Add the timing metadata statistics-mtg2 requires on every field. The test inputs are emitted at the IO-server
/// output cadence, so the output step equals the distance to the previous step.
/// timeIncrementInSeconds must be given for statistical inputs (with timespan) and omitted for instantaneous ones.
inline void setTimingMetadata(message::Metadata& md, std::int64_t distanceFromPreviousStepInSeconds,
                              std::optional<std::int64_t> timeIncrementInSeconds = std::nullopt) {
    md.set("misc-outputStepInSeconds", distanceFromPreviousStepInSeconds);
    md.set("misc-integrationStepInSeconds", INTEGRATION_STEP_IN_SECONDS);
    md.set("misc-distanceFromPreviousStepInSeconds", distanceFromPreviousStepInSeconds);
    if (timeIncrementInSeconds) {
        md.set("misc-timeIncrementInSeconds", *timeIncrementInSeconds);
    }
}

}  // namespace multio::test::statistics_mtg2
