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

#include "GridDownloader.h"
#include "multio/LibMultio.h"
#include "multio/config/ConfigurationPath.h"
#include "multio/util/ScopedTimer.h"

namespace multio::action {

using config::configuration_path_name;

using util::lookUp;
using util::lookUpTranslate;

namespace {

eckit::LocalConfiguration getEncodingConfiguration(const ComponentConfiguration& compConf) {
    if (compConf.parsedConfig().has("encoding")) {
        return compConf.parsedConfig().getSubConfiguration("encoding");
    }
    else {
        return compConf.parsedConfig();
    }
}

std::unique_ptr<MioGribHandle> loadTemplate(const eckit::LocalConfiguration& conf,
                                            const config::MultioConfiguration& multioConfig) {
    ASSERT(conf.has("template"));
    std::string tmplPath = conf.getString("template");
    // TODO provide utility to distinguish between relative and absolute paths
    eckit::AutoStdFile fin{multioConfig.replaceCurly(tmplPath)};
    int err;
    return std::make_unique<MioGribHandle>(codes_handle_new_from_file(nullptr, fin, PRODUCT_GRIB, &err), conf);
}

std::string encodeGrib2ExceptionReason(const std::string& r) {
    std::string s("EncodeGrib2 exception: ");
    s.append(r);
    return s;
}

void setDateTime(MioGribHandle& h, const std::optional<std::int64_t>& date, const std::optional<std::int64_t>& time) {
    if (date) {
        h.setValue("year", *date / 10000);
        h.setValue("month", *date % 10000) / 100);
        h.setValue("day", *date % 100);
    }
    if (time) {
        h.setValue("hour", *time / 10000);
        h.setValue("minute", *time % 10000) / 100);
        h.setValue("second", *time % 100);
    }
}

void setStep(MioGribHandle& h, const std::optional<std::int64_t>& startStep,
             const std::optional<std::int64_t>& endStep) {
    if (startStep) {
        h.setValue("startStep", *startStep);
    }
    if (endStep) {
        h.setValue("endStep", *endStep);
    }
}

message::Metadata applyOverwrites(MioGribHandle& h, const eckit::LocalConfiguration& configOverwrites,
                                  message::Metadata md) {
    auto overwrites = md.getOpt<message::Metadata>("encoder-overwrites").or_else(message::Metadata{});
    for (auto&& kv : message::toMetadata(configOverwrites.get())) {
        overwrites.set(std::move(kv->first), std::move(kv->second));
    }
    for (const auto& kv : overwrites) {
        // TODO handle type... however eccodes should support string as well. For
        // some representations the string and integer representation in eccodes
        // differ significantly and my produce wrong results
        if (h.hasKey(kv.first.c_str())) {
            kv.second.visit(Overloaded{
                [](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataNestedTypes> {},
                [&h, &kv](const auto& vec) -> util::IfTypeOf<decltype(vec), message::MetadataVectorTypes> {
                    h.setValues(kv.first, vec);
                },
                [&h, &kv](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataNonNullScalarTypes> {
                    h.setValue(kv.first, v);
                },
                [&h, &kv](const auto& v) -> util::IfTypeOf<decltype(v), message::MetadataNullTypes> {
                    h.setValue(kv.first, 0);
                }});
        }
    }
    return md;
}


}  // namespace


EncodeGrib2Exception::EncodeGrib2Exception(const std::string& r, const eckit::CodeLocation& l) :
    eckit::Exception(encodeGrib2ExceptionReason(r), l) {}

using message::Message;
using message::Peer;

EncodeGrib2::EncodeGrib2(const ComponentConfiguration& compConf, const eckit::LocalConfiguration& encConf) :
    ChainedAction{compConf},
    overwrite_{encConf.has("overwrite")
                   ? std::optional<eckit::LocalConfiguration>{encConf.getSubConfiguration("overwrite")}
                   : std::optional<eckit::LocalConfiguration>{}},
    template_{loadTemplate(encConf, compConf.multioConfig())},
    encoder_{nullptr} gridDownloader_{std::make_unique<multio::action::GridDownloader>(compConf)} {}

EncodeGrib2::EncodeGrib2(const ComponentConfiguration& compConf) :
    EncodeGrib2(compConf, getEncodingConfiguration(compConf)) {}

void EncodeGrib2::executeImpl(Message msg) {
    if (msg.tag() != Message::Tag::Field) {
        executeNext(std::move(msg));
        return;
    }

    encoder_.reset(template_.duplicate());
    auto md = this->overwrite_ ? applyOverwrites(*encoder_.get(), *this->overwrite_, msg.metadata()) : msg.metadata();


    auto typeMaybe = lookUp<std::string>(md, "type")();
    auto operationMaybe = lookUp<std::string>(md, "operation")();

    // Special date/time handling to operate with statistics action
    if (typeMaybe) {
        auto& type = *typeMaybe;
        encoder_->setValue("type", type);

        // List of forecast-type data
        if (type == "fc") {
            setDateTime(*encoder_.get(), lookUp<std::int64_t>(md, "startDate")(),
                        lookUp<std::int64_t>(md, "startTime")());
        }
        else if (type == "pf") {
            setDateTime(*encoder_.get(), lookUp<std::int64_t>(md, "startDate")(),
                        lookUp<std::int64_t>(md, "startTime")());
        }
        else
            // List time-processed analysis data
            if (type == "tpa") {
                setDateTime(*encoder_.get(), lookUp<std::int64_t>(md, "previousDate")(),
                            lookUp<std::int64_t>(md, "previousTime")());
            }
            else {
                // Analysis data
                setDateTime(*encoder_.get(), lookUp<std::int64_t>(md, "currentDate")(),
                            lookUp<std::int64_t>(md, "currentTime")());
            }
    }

    if (operationMaybe) {
        auto& operation = *operationMaybe;
        bool isOpInstant = operation == "instant";

        auto stepInHours = lookUp<std::int64_t>(md, "stepInHours")();
        auto timeSpanInHours = lookUp<std::int64_t>(md, "timeSpanInHours")();

        if (isOpInstant && typeMaybe && (*typeMaybe == "fc")) {
            if (!stepInHours) {
                throw eckit::SeriousBug("Not enough information to encode startStep for point-in-time of forecast");
            }
            setStep(*encoder_.get(), stepInHours, std::nullopt);
        }
        else {
            if (!(stepInHours && timeSpanInHours)) {
                throw eckit::SeriousBug("Not enough information to encode step range");
            }

            if (typeMaybe && ((*typeMaybe == "fc") || (*typeMaybe == "pf"))) {
                auto prevStep = std::max(*stepInHours - *timeSpanInHours, (std::int64_t)0L);
                setStep(*encoder_.get(), std::max(*stepInHours - *timeSpanInHours, (std::int64_t)0L), stepInHours);
            }
            else {
                setStep(*encoder_.get(), 0, *timeSpanInHours);
            }

            withFirstOf(valueSetter(g, "indicatorOfUnitForTimeIncrement"),
                        std::optional<std::int64_t>{13l});  // always seconds
            withFirstOf(valueSetter(g, "timeIncrement"), lookUp<std::int64_t>(md, "timeStep"));
        }
    }

    if (auto dateOfAnalysis = lookUp<std::int64_t>(md, "date-of-analysis")()) {
        encoder_->setValue("yearOfAnalysis", *dateOfAnalysis / 10000);
        encoder_->setValue("monthOfAnalysis", *dateOfAnalysis % 10000) / 100);
        encoder_->setValue("dayOfAnalysis", *dateOfAnalysis % 100);
    }

    if (auto timeOfAnalysis = lookUp<std::int64_t>(md, "time-of-analysis")()) {
        encoder_->setValue("hourOfAnalysis", *timeOfAnalysis / 10000);
        encoder_->setValue("minuteOfAnalysis", (*timeOfAnalysis % 10000) / 100);
    }

    // The least mars keys we want to set explicitly
    withFirstOf(valueSetter(g, "class"), lookUp<std::string>(md, "class"), lookUp<std::string>(md, "marsClass"));
    withFirstOf(valueSetter(g, "stream"), lookUp<std::string>(md, "stream"), lookUp<std::string>(md, "marsStream"));
    withFirstOf(valueSetter(g, "expver"), lookUp<std::string>(md, "expver"),
                lookUp<std::string>(md, "experimentVersionNumber"));
}


message::Message EncodeGrib2::setFieldValues(const message::Message& msg) {
    return dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        auto beg = reinterpret_cast<const Precision*>(msg.payload().data());

        encoder_->setDataValues(beg, msg.globalSize());

        eckit::Buffer buf{this->encoder_->length()};
        encoder_->write(buf);

        return Message{Message::Header{Message::Tag::Grib, Peer{msg.source().group()}, Peer{msg.destination()}},
                       std::move(buf)};
    });
}


message::Message EncodeGrib2::setFieldValues(const double* values, size_t count) {
    encoder_->setDataValues(values, count);

    eckit::Buffer buf{this->encoder_->length()};
    encoder_->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{}, Peer{}}, std::move(buf)};
}


message::Message EncodeGrib2::setFieldValues(const float* values, size_t count) {
    std::vector<double> dvalues(count, 0.0);
    for (int i = 0; i < count; ++i) {
        dvalues[i] = double(values[i]);
    }

    encoder_->setDataValues(dvalues.data(), count);

    eckit::Buffer buf{this->encoder_->length()};
    encoder_->write(buf);

    return Message{Message::Header{Message::Tag::Grib, Peer{}, Peer{}}, std::move(buf)};
}


void EncodeGrib2::print(std::ostream& os) const {
    os << "EncodeGrib2(encoder=";
    if (encoder_)
        encoder_->print(os);
    os << ")";
}

namespace {}  // namespace

message::Message EncodeGrib2::encodeField(const message::Message& msg,
                                          const std::optional<std::string>& gridUID) const {
    try {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
        auto md = this->overwrite_ ? applyOverwrites(*this->overwrite_, msg.metadata()) : msg.metadata();
        if (gridUID) {
            md.set("uuidOfHGrid", gridUID.value());
        }
        return encoder_->encodeField(msg.modifyMetadata(std::move(md)));
    }
    catch (...) {
        std::ostringstream oss;
        oss << "Encode::encodeField with Message: " << msg;
        std::throw_with_nested(EncodeGrib2Exception(oss.str(), Here()));
    }
}

static ActionBuilder<EncodeGrib2> EncodeGrib2Builder("encode-grib2");

}  // namespace multio::action
