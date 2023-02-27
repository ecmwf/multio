/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */


#include "multio/action/interpolate/Interpolate.h"

#include <algorithm>
#include <regex>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "eckit/parser/YAMLParser.h"
#include "eckit/types/Fraction.h"

#include "metkit/mars/MarsLanguage.h"

#include "mir/api/MIRJob.h"
#include "mir/input/RawInput.h"
#include "mir/output/ResizableOutput.h"
#include "mir/param/SimpleParametrisation.h"
#include "mir/repres/gauss/reduced/Reduced.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/PrecisionTag.h"


namespace multio::action::interpolate {


void fill_input(const eckit::LocalConfiguration& cfg, mir::param::SimpleParametrisation& param) {
    ASSERT(cfg.has("input"));

    auto regular_ll = [&param](double west_east_increment, double south_north_increment) {
        auto N = [](double delta, double range) {
            const auto f = eckit::Fraction(range) / eckit::Fraction(delta);
            return static_cast<long>(f.integralPart());
        };

        param.set("gridded", true);
        param.set("gridType", "regular_ll");

        param.set("west_east_increment", west_east_increment);
        param.set("south_north_increment", south_north_increment);
        param.set("Ni", N(west_east_increment, 360.));
        param.set("Nj", N(south_north_increment, 180.) + 1L /* "endpoint" */);

        param.set("north", 90.).set("west", 0.).set("south", -90.).set("east", 360.);
    };

    if (cfg.getSubConfiguration("input").get().isString()) {
        const auto input = cfg.getString("input");

        static const std::regex sh("(T|TCO|TL)([1-9][0-9]*)");
        static const std::regex gg("([FNO])([1-9][0-9]*)");

#define fp "([+]?([0-9]*[.])?[0-9]+([eE][-+][0-9]+)?)"
        static const std::regex ll(fp "/" fp);
#undef fp

        std::smatch match;
        if (std::regex_match(input, match, sh)) {
            param.set("spectral", true);
            param.set("gridType", "sh");
            param.set("truncation", std::stol(match[2].str()));
            return;
        }

        if (std::regex_match(input, match, gg)) {
            param.set("gridded", true);
            param.set("gridType", match[1].str() == "F" ? "regular_gg" : "reduced_gg");
            param.set("N", std::stol(match[2].str()));
            param.set("north", 90.).set("west", 0.).set("south", -90.).set("east", 360.);

            if (match[1].str() != "F") {
                param.set("pl", mir::repres::gauss::reduced::Reduced::pls(match[0].str()));
            }
            return;
        }

        if (std::regex_match(input, match, ll)) {
            regular_ll(std::stod(match[1].str()), std::stod(match[4].str()));
            return;
        }
    }

    if (cfg.getSubConfiguration("input").get().isList()) {
        const auto input = cfg.getDoubleVector("input");
        if (input.size() == 2) {
            regular_ll(input[0], input[1]);
            return;
        }
    }

    NOTIMP;
}


void fill_job(const eckit::LocalConfiguration& cfg, mir::param::SimpleParametrisation& destination) {

    static const struct PostProcKeys : std::vector<std::string> {
        PostProcKeys() {
            const auto yaml = eckit::YAMLParser::decodeFile(metkit::mars::MarsLanguage::languageYamlFile());
            for (const auto& key : yaml["_postproc"].keys().as<eckit::ValueList>()) {
                emplace_back(key.as<std::string>());
            }
        }
        bool contains(const std::string& key) const { return std::find(begin(), end(), key) != end(); }
    } postproc;

    ASSERT(not postproc.contains("input"));
    ASSERT(not postproc.contains("options"));

    auto set = [&destination](const eckit::LocalConfiguration& cfg, const std::string& key, const eckit::Value& value) {
        if (value.isList()) {
          if ( value.head().isDouble() ) {
            destination.set(key, cfg.getDoubleVector(key));
          } else if ( value.head().isNumber() ) {
             destination.set(key, cfg.getLongVector(key));
          } else if ( value.isString() ) {
             destination.set(key, cfg.getStringVector(key));
          } else {
             NOTIMP;
          }
          return;
        }
        if ( value.isBool() ){
          destination.set(key, cfg.getBool(key));
        } else if ( value.isDouble() ){
          destination.set(key, cfg.getDouble(key));
        } else  if ( value.isNumber() ) {
          destination.set(key, cfg.getInt(key));
        } else if ( value.isString()  ) {
          destination.set(key, cfg.getString(key).c_str());
        } else {
          NOTIMP;
        }
    };

    for (const auto& key : postproc) {
        if (cfg.has(key)) {
            set(cfg, key, cfg.getSubConfiguration(key).get());
        }
    }


    if (cfg.has("options")) {
        const auto& options = cfg.getSubConfiguration("options");
        for (const auto& key : options.keys()) {
            set(options, key, options.getSubConfiguration(key).get());
        }
    }
}

template <>
message::Message Interpolate::InterpolateMessage<double>(message::Message&& msg) const {
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the input message :: " << std::endl
                             << msg.metadata() << std::endl
                             << std::endl;

    const auto& config = Action::confCtx_.config();

    const double* data = reinterpret_cast<const double*>(msg.payload().data());
    const size_t size = msg.payload().size() / sizeof(double);

    mir::param::SimpleParametrisation inputPar;
    fill_input(config, inputPar);

    mir::input::RawInput input(data, size, inputPar);

    mir::api::MIRJob job;
    fill_job(config, job);

    if (msg.metadata().has("missingValue")) {
        job.set("missing_value", msg.metadata().getDouble("missingValue"));
    }

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: input :: " << std::endl << inputPar << std::endl << std::endl;

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: job " << std::endl << job << std::endl << std::endl;

    std::vector<double> outData;
    mir::param::SimpleParametrisation outMetadata;
    mir::output::ResizableOutput output(outData, outMetadata);

    job.execute(input, output);

    message::Metadata md=msg.metadata();
    md.set("globalSize", outData.size());
    md.set("precision", "double");
    md.set("gridType", "regular_ll");

    eckit::Buffer buffer(reinterpret_cast<const char*>(outData.data()), outData.size() * sizeof(double));

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the output message :: " << std::endl
                             << md << std::endl
                             << std::endl;

    return {message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(), std::move(md)},
            std::move(buffer)};
}

template <>
message::Message Interpolate::InterpolateMessage<float>(message::Message&& msg) const {
    // convert single/double precision, interpolate, convert double/single
    return InterpolateMessage<double>(message::convert_precision<float, double>(std::move(msg)));
}

void Interpolate::executeImpl(message::Message msg) {
    switch (msg.tag()) {
        case (message::Message::Tag::Field): {
            executeNext(util::dispatchPrecisionTag(msg.precision(), [&](auto pt) -> message::Message {
                using Precision = typename decltype(pt)::type;
                return InterpolateMessage<Precision>(std::move(msg));
            }));
            break;
        }
        default: {
            executeNext(msg);
            break;
        }
    };
}


void Interpolate::print(std::ostream& os) const {
    os << "Interpolate";
}


static ActionBuilder<Interpolate> InterpolateBuilder("interpolate");


}  // namespace multio::action::interpolate
