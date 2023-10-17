/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "EncodeGrib2.h"

#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "eckit/io/StdFile.h"
#include "eckit/log/Log.h"
#include "eckit/types/Date.h"
#include "eckit/types/DateTime.h"
#include "eckit/types/Time.h"

#include "multio/LibMultio.h"
#include "multio/action/encode-grib2/Exception.h"
#include "multio/util/DateTime.h"
#include "multio/util/ScopedTimer.h"

namespace multio::action {

using config::configuration_path_name;

namespace {

eckit::LocalConfiguration getEncodingConfiguration(const ComponentConfiguration& compConf) {
    if (compConf.parsedConfig().has("encoding")) {
        return compConf.parsedConfig().getSubConfiguration("encoding");
    }
    else {
        return compConf.parsedConfig();
    }
}

void applyOverwrites(MioGribHandle& h, const std::optional<eckit::LocalConfiguration>& configOverwrites,
                     const message::Metadata& md) {
    // TODO clean up?? avoid copying metadata and pass metadata by ref to a lambda?
    auto overwrites = md.getOpt<message::Metadata>("encoder-overwrites").value_or(message::Metadata{});
    for (auto&& kv : message::toMetadata(configOverwrites->get())) {
        overwrites.set(std::move(kv.first), std::move(kv.second));
    }
    for (const auto& kv : overwrites) {
        // TODO handle type... however eccodes should support string as well. For
        // some representations the string and integer representation in eccodes
        // differ significantly and my produce wrong results
        if (h.hasKey(kv.first.c_str())) {
            kv.second.visit(eckit::Overloaded{
                [](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataTypes::Nested> {},
                [&h, &kv](const auto& vec) -> util::IfTypeOf<decltype(vec), message::MetadataTypes::Lists> {
                    h.setValue(kv.first, vec);
                },
                [&h, &kv](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataTypes::NonNullScalars> {
                    h.setValue(kv.first, v);
                },
                [&h, &kv](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataTypes::Nulls> {
                    h.setValue(kv.first, 0);
                }});
        }
    }
}


}  // namespace


using message::Message;
using message::Peer;

EncodeGrib2::EncodeGrib2(const ComponentConfiguration& compConf, const eckit::LocalConfiguration& encConf) :
    ChainedAction{compConf},
    sampleManager_{ComponentConfiguration{encConf, compConf.multioConfig()}},
    overwrite_{encConf.has("overwrite")
                   ? std::optional<eckit::LocalConfiguration>{encConf.getSubConfiguration("overwrite")}
                   : std::optional<eckit::LocalConfiguration>{}}

{}

EncodeGrib2::EncodeGrib2(const ComponentConfiguration& compConf) :
    EncodeGrib2(compConf, getEncodingConfiguration(compConf)) {}

namespace {
using message::Metadata;

// https://codes.ecmwf.int/grib/format/grib2/ctables/4/4/
std::int64_t timeUnitCodes(util::TimeUnit u) {
    switch (u) {
        case util::TimeUnit::Year:
            return 4;
        case util::TimeUnit::Month:
            return 3;
        case util::TimeUnit::Day:
            return 2;
        case util::TimeUnit::Hour:
            return 1;
        case util::TimeUnit::Minute:
            return 0;
        case util::TimeUnit::Second:
            return 13;
        default:
            std::ostringstream oss;
            oss << "timeUnitCodes: Unexpcted TimeUnit " << util::timeUnitToChar(u);
            throw EncodeGrib2Exception(std::string(oss.str()), Here());
    }
}


//-----------------------------------------------------------------------------

struct MetadataToGrib {
    const Metadata& metadata;
    MioGribHandle& handle;

    template <typename T>
    void transfer(const std::string& getKey, const std::string& setKey) {
        handle.setValue(getKey, metadata.get<T>(setKey));
    }
    template <typename T>
    bool transferOpt(const std::string& getKey, const std::string& setKey) {
        if (auto opt = metadata.getOpt<T>(getKey); opt) {
            handle.setValue(setKey, *opt);
            return true;
        }
        return false;
    }

    template <typename T>
    void transfer(const std::string& k) {
        transfer<T>(k, k);
    }
    template <typename T>
    bool transferOpt(const std::string& k) {
        return transferOpt<T>(k, k);
    }


    void transferSection1Keys() {
        this->transfer<std::int64_t>("year");
        this->transfer<std::int64_t>("month");
        this->transfer<std::int64_t>("day");
        this->transfer<std::int64_t>("hour");
        this->transfer<std::int64_t>("minute");
        this->transfer<std::int64_t>("second");
    }


    void transferMarsKeys() {
        this->transferOpt<std::string>("class");
        this->transferOpt<std::string>("stream");
        this->transferOpt<std::string>("expver") || this->transferOpt<std::string>("experimentVersionNumber", "expver");
    }
};


//-----------------------------------------------------------------------------

void multioToEccodesParamId(const std::string& op, const Metadata& in, Metadata& out) {
    static const std::unordered_map<std::string, std::int64_t> OPS_TO_CODE{{"instant", 0000},    {"average", 1000},
                                                                           {"accumulate", 2000}, {"maximum", 3000},
                                                                           {"minimum", 4000},    {"stddev", 5000}};

    std::int64_t paramId = in.get<std::int64_t>("paramId");
    if (paramId >= 212000 && paramId < 213000) {
        // HACK! Support experimental averages.
        out.set("paramId", paramId + 4000);
    }
    else {
        out.set("paramId", paramId + OPS_TO_CODE.at(op));
    }
}


//-----------------------------------------------------------------------------

std::tuple<std::int64_t, std::int64_t> getReferenceDateTime(const std::string& timeRef, const Metadata& in) {
    static std::unordered_map<std::string, std::tuple<std::string, std::string>> REF_TO_DATETIME_KEYS{
        {"start", {"startDate", "startTime"}},
        {"previous", {"previousDate", "previousTime"}},
        {"current", {"currentDate", "currentTime"}},
    };

    auto search = REF_TO_DATETIME_KEYS.find(timeRef);
    if (search == REF_TO_DATETIME_KEYS.end()) {
        std::ostringstream oss;
        oss << "getReferenceDateTime: Time reference \"" << timeRef << "\" can not be mapped";
        throw EncodeGrib2Exception(oss.str(), Here());
    }

    return std::make_tuple(in.get<std::int64_t>(std::get<0>(search->second)),
                           in.get<std::int64_t>(std::get<1>(search->second)));
}

void multioToEccodesDatetime(const std::optional<std::string>& op, const Metadata& in, Metadata& out) {
    auto timeExtent = in.getOpt<std::string>("timeExtent");

    if (!timeExtent) {
        return;
    }
    auto timeFormat = in.getOpt<std::string>("timeFormat");

    auto type = in.getOpt<std::string>("type");

    // bool isTimeRange = op && op != "instant";
    bool isTimeRange = timeExtent == "timeRange";

    if ((op && op == "instant") && isTimeRange) {
        throw EncodeGrib2Exception(
            "multioToEccodesDatetime - Inconsintent metadata. Key \"timeExtent\" has value \"timeRange\" but "
            "\"operation\" is "
            "\"instant\".",
            Here());
    }


    bool isLocalTime = timeFormat == "localTime";
    // Will be named to indicatorOfUnitForForecastTime consistently
    const char* forecasteTimeUnitKey = isLocalTime ? "indicatorOfUnitForForecastTime" : "indicatorOfUnitOfTimeRange";


    // TODO to be moved to some metadata util or put on top of message?
    // Maybe have a separate place for data model that checks these things....
    std::string timeRef = std::invoke([&]() -> std::string {
        if (auto optTimeRef = in.getOpt<std::string>("timeReference"); optTimeRef) {
            return *optTimeRef;
        }

        // TODO: this will not hold in the future - maybe the new category "processType" can be used to check if it's a
        // forecaste
        bool isForecast = type && (type == "fc" || type == "pf");
        return isForecast ? "start" : (isTimeRange ? "previous" : "current");
    });


    auto refDateTime = getReferenceDateTime(timeRef, in);
    auto refDate = util::toDateInts(std::get<0>(refDateTime));
    out.set("year", refDate.year);
    out.set("month", refDate.month);
    out.set("day", refDate.day);

    auto refTime = util::toTimeInts(std::get<1>(refDateTime));
    out.set("hour", refTime.hour);
    out.set("minute", refTime.minute);
    out.set("second", refTime.second);

    auto currentDate = util::toDateInts(in.get<std::int64_t>("currentDate"));
    auto currentTime = util::toTimeInts(in.get<std::int64_t>("currentTime"));
    if (!isTimeRange) {
        if (timeRef.compare("current") != 0) {
            // Compute diff to current time in some appropriate unit
            util::DateTimeDiff diff = util::dateTimeDiff(currentDate, currentTime, refDate, refTime);
            out.set(forecasteTimeUnitKey, timeUnitCodes(diff.unit));
            out.set("forecastTime", diff.diff);
        }
        else {
            out.set(forecasteTimeUnitKey, 0);
            out.set("forecastTime", 0);
        }
    }
    else {
        auto previousDate = util::toDateInts(in.get<std::int64_t>("previousDate"));
        auto previousTime = util::toTimeInts(in.get<std::int64_t>("previousTime"));
        if (timeRef.compare("previous") != 0) {
            // Compute diff to current time in some appropriate unit
            util::DateTimeDiff diff = util::dateTimeDiff(previousDate, previousTime, refDate, refTime);
            out.set(forecasteTimeUnitKey, timeUnitCodes(diff.unit));
            out.set("forecastTime", diff.diff);
        }
        else {
            // No forecast time is used
            out.set(forecasteTimeUnitKey, 0);
            out.set("forecastTime", 0);
        }

        out.set("yearOfEndOfOverallTimeInterval", currentDate.year);
        out.set("monthOfEndOfOverallTimeInterval", currentDate.month);
        out.set("dayOfEndOfOverallTimeInterval", currentDate.day);
        out.set("hourOfEndOfOverallTimeInterval", currentTime.hour);
        out.set("minuteOfEndOfOverallTimeInterval", currentTime.minute);
        out.set("secondOfEndOfOverallTimeInterval", currentTime.second);

        util::DateTimeDiff lengthTimeRange = util::dateTimeDiff(currentDate, currentTime, previousDate, previousTime);

        out.set("indicatorOfUnitForTimeRange", timeUnitCodes(lengthTimeRange.unit));
        out.set("lengthOfTimeRange", lengthTimeRange.diff);

        if (op) {
            static const std::map<const std::string, const std::int64_t> TYPE_OF_STATISTICAL_PROCESSING{
                {"average", 0}, {"accumulate", 1}, {"maximum", 2}, {"minimum", 3}, {"stddev", 6}};
            if (auto searchStat = TYPE_OF_STATISTICAL_PROCESSING.find(*op);
                searchStat != TYPE_OF_STATISTICAL_PROCESSING.end()) {
                out.set("typeOfStatisticalProcessing", searchStat->second);
            }
            else {
                std::ostringstream oss;
                oss << "multioToEccodesDatetime - Can not map value \"" << *op
                    << "\"for key \"operation\" (statistical output) to a valid grib2 type of statistical processing.";
                throw EncodeGrib2Exception(oss.str(), Here());
            }
        }


        // # CODE TABLE 4.11, Type of time intervals
        // 1 1  Successive times processed have same forecast time, start time of forecast is incremented
        // 2 2  Successive times processed have same start time of forecast, forecast time is incremented
        // 3 3  Successive times processed have start time of forecast incremented and forecast time decremented so that
        // valid time remains constant 4 4  Successive times processed have start time of forecast decremented and
        // forecast time incremented so that valid time remains constant 5 5  Floating subinterval of time between
        // forecast time and end of overall time interval
        out.set("typeOfTimeIncrement", timeRef == "start" ? 2 : 1);

        auto sampleIntervalUnitStr = in.get<std::string>("sampleIntervalUnit");
        auto sampleIntervalUnit = util::timeUnitFromString(sampleIntervalUnitStr);
        if (!sampleIntervalUnit) {
            std::ostringstream oss;
            oss << "multioToEccodesDatetime - Value for passed metadatakey \"sampleIntervalUnit\": "
                << sampleIntervalUnitStr << " can not be parsed to a valid unit (Y,m,d,H,M,S). ";
            throw EncodeGrib2Exception(oss.str(), Here());
        }
        out.set("indicatorOfUnitForTimeIncrement", timeUnitCodes(*sampleIntervalUnit));
        out.set("timeIncrement", in.get<std::int64_t>("sampleInterval"));
    }


    // Set some additional local ECMWF keys...
    if (auto analysisDate = in.getOpt<std::int64_t>("date-of-analysis"); analysisDate) {
        auto date = util::toDateInts(*analysisDate);
        out.set("yearOfAnalysis", date.year);
        out.set("monthOfAnalysis", date.month);
        out.set("dayOfAnalysis", date.day);
    }

    if (auto analysisTime = in.getOpt<std::int64_t>("time-of-analysis"); analysisTime) {
        auto time = util::toTimeInts(*analysisTime);
        out.set("hourOfAnalysis", time.hour);
        out.set("minuteOfAnalysis", time.minute);
        out.set("secondOfAnalysis", time.second);
    }
}

//-----------------------------------------------------------------------------


using CustomMapFunction = std::function<void(const Metadata&, Metadata&)>;
using CustomMapping = std::unordered_map<std::string, CustomMapFunction>;


void mapLevelToFirstFixedSurface(const Metadata& in, Metadata& out) {
    auto level = in.get<std::int64_t>("level");
    ASSERT(level > 0);
    out.set("scaledValueOfFirstFixedSurface", level);
};

void mapLevelToFixedSurfaces(const Metadata& in, Metadata& out) {
    auto level = in.get<std::int64_t>("level");
    ASSERT(level > 0);
    out.set("scaledValueOfFirstFixedSurface", level - 1);
    out.set("scaledValueOfSecondFixedSurface", level);
};


const CustomMapping TYPE_OF_LEVEL_MAPPINGS{
    {          // First only
     {"snow",  // sol
      [](const Metadata& in, Metadata& out) { mapLevelToFirstFixedSurface(in, out); }},
     {"soil",  // sol
      [](const Metadata& in, Metadata& out) { mapLevelToFirstFixedSurface(in, out); }},
     {"seaIce",  // sol
      [](const Metadata& in, Metadata& out) { mapLevelToFirstFixedSurface(in, out); }},
     {"hybrid",  // ml
      [](const Metadata& in, Metadata& out) { mapLevelToFirstFixedSurface(in, out); }},
     {"oceanModel",  // o3d
      [](const Metadata& in, Metadata& out) { mapLevelToFirstFixedSurface(in, out); }},

     // First and second
     {"snowLayer",  // sol
      [](const Metadata& in, Metadata& out) { mapLevelToFixedSurfaces(in, out); }},
     {"soilLayer",  // sol
      [](const Metadata& in, Metadata& out) { mapLevelToFixedSurfaces(in, out); }},
     {"seaIceLayer",  // sol
      [](const Metadata& in, Metadata& out) { mapLevelToFixedSurfaces(in, out); }},
     {"hybridLayer",  // ml
      [](const Metadata& in, Metadata& out) { mapLevelToFixedSurfaces(in, out); }},
     {"oceanModelLayer",  // o3d
      [](const Metadata& in, Metadata& out) { mapLevelToFixedSurfaces(in, out); }}}};


void multioToEccodesVertical(const Metadata& in, Metadata& out) {
    if (auto typeOfLevel = in.getOpt<std::string>("typeOfLevel"); typeOfLevel) {
        if (auto searchTOL = TYPE_OF_LEVEL_MAPPINGS.find(*typeOfLevel); searchTOL != TYPE_OF_LEVEL_MAPPINGS.end()) {
            std::invoke(searchTOL->second, in, out);
        }
    }
}


//-----------------------------------------------------------------------------

void multioToEccodes(const Metadata& in, Metadata& out) {
    auto op = in.getOpt<std::string>("operation");

    if (op) {
        multioToEccodesParamId(*op, in, out);
    }
    multioToEccodesVertical(in, out);
    multioToEccodesDatetime(op, in, out);
}

//-----------------------------------------------------------------------------


}  // namespace


//-----------------------------------------------------------------------------

eckit::Buffer EncodeGrib2::encodeSample(MioGribHandle& sample) const {
    eckit::Buffer buf{sample.length()};
    sample.write(buf);
    return buf;
}


//-----------------------------------------------------------------------------

message::Message EncodeGrib2::encodeMessageWithData(MioGribHandle& sample, const message::Message& inputMsg) const {
    return dispatchPrecisionTag(inputMsg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        sample.setDataValues(reinterpret_cast<const Precision*>(inputMsg.payload().data()), inputMsg.globalSize());
        return message::Message{
            Message::Header{Message::Tag::Grib, Peer{inputMsg.source().group()}, Peer{inputMsg.destination()}},
            encodeSample(sample)};
    });
}

//-----------------------------------------------------------------------------

message::Message EncodeGrib2::encodeMessageWithoutData(MioGribHandle& sample, const message::Message& inputMsg) const {
    return dispatchPrecisionTag(inputMsg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        return message::Message{
            Message::Header{Message::Tag::Grib, Peer{inputMsg.source().group()}, Peer{inputMsg.destination()}},
            encodeSample(sample)};
    });
}


//-----------------------------------------------------------------------------

void EncodeGrib2::transferRelevantValues(const encodeGrib2::SampleKey& sampleKey, const message::Metadata& from,
                                         MioGribHandle& to) const {
    MetadataToGrib m2g{from, to};
    m2g.transferSection1Keys();
    sampleManager_.transferProductKeys(sampleKey, from, to);
    m2g.transferMarsKeys();
};


void EncodeGrib2::transferRelevantValues(const message::Metadata& from, MioGribHandle& to) const {
    MetadataToGrib m2g{from, to};
    m2g.transferSection1Keys();
    m2g.transferMarsKeys();
};


//-----------------------------------------------------------------------------

void EncodeGrib2::executeImpl(Message msg) {
    auto& md = msg.metadata();

    /**
     * Handles messages of tag Field and Domain.
     *
     * Domain:
     *  - Prepare a sample with grid information.
     *  - Can be reused for messages with tag field working on the same domain
     *  - may produce messages with grid coordinates to encode in a separate message (in case of unstructured grids)
     *
     * Field:
     *  - Encode a message with proper grid and product information
     *  - may reuse a pre-initiated sample with domain (grid) information
     *  - or may prepare a grid information from scratch
     */
    switch (msg.tag()) {
        case Message::Tag::Field: {
            encodeGrib2::SampleKey sampleKey = sampleManager_.sampleKeyFromMetadata(md);

            // Get a prepared sample with proper grid information and product definition template selected
            auto handleFieldRes = sampleManager_.prepareSample(sampleKey, md);

            // Use metadata with overwrites as input to allow mapping of these values...
            multioToEccodes(handleFieldRes.metadataWithOverwrites, handleFieldRes.metadataWithOverwrites);

            // Handle additional messages (i.e. coordinates in case of lazy/dynamic init of grid information)
            if (handleFieldRes.encodeAdditionalHandles) {
                for (auto& handlePtr : *handleFieldRes.encodeAdditionalHandles) {
                    transferRelevantValues(sampleKey, handleFieldRes.metadataWithOverwrites, *handlePtr.get());

                    applyOverwrites(*handlePtr.get(), this->overwrite_, md);
                    executeNext(encodeMessageWithoutData(*handlePtr.get(), msg));
                }
            }

            // Add all relevant product information
            {
                MioGribHandle& handle = *handleFieldRes.encodeFieldHandle.get();

                transferRelevantValues(sampleKey, handleFieldRes.metadataWithOverwrites, handle);

                applyOverwrites(handle, this->overwrite_, md);
                executeNext(encodeMessageWithData(handle, msg));
            }
        }
        case Message::Tag::Domain: {
            if (auto domain = md.getOpt<std::string>(encodeGrib2::DOMAIN_KEY); domain) {
                // Initialize a sample for a domain with given grid information
                auto initDomainRes = sampleManager_.initDomain(*domain, md);

                // Handle possible messages encoding grid information in separate messages
                if (initDomainRes.encodeAdditionalHandles) {
                    auto sampleKeyMb = sampleManager_.tryGetSampleKeyFromMetadata(md);
                    for (auto& handlePtr : *initDomainRes.encodeAdditionalHandles) {
                        multioToEccodes(initDomainRes.metadataWithOverwrites, initDomainRes.metadataWithOverwrites);

                        if (sampleKeyMb) {
                            // TODO handle local definition number
                            handlePtr->setValue(encodeGrib2::PDT_KEY, sampleKeyMb->productDefinitionTemplateNumber);
                            transferRelevantValues(*sampleKeyMb, initDomainRes.metadataWithOverwrites,
                                                   *handlePtr.get());
                        }
                        else {
                            transferRelevantValues(initDomainRes.metadataWithOverwrites, *handlePtr.get());
                        }

                        applyOverwrites(*handlePtr.get(), this->overwrite_, md);

                        executeNext(encodeMessageWithoutData(*handlePtr.get(), msg));
                    }
                }
            }
            executeNext(std::move(msg));
            return;
        }
        default: {
            executeNext(std::move(msg));
            return;
        }
    }
}

//-----------------------------------------------------------------------------


void EncodeGrib2::print(std::ostream& os) const {
    // TODO
    // - maybe print sample manager with all prepared samples?
    os << "EncodeGrib2(encoder=TBD";
    os << ")";
}


static ActionBuilder<EncodeGrib2> EncodeGrib2Builder("encode-grib2");

}  // namespace multio::action
