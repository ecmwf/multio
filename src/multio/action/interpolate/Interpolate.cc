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

#include <fstream>
#include <type_traits>
#include <vector>

#include "eckit/exception/Exceptions.h"

#include "mir/api/MIRJob.h"
#include "mir/input/RawInput.h"
#include "mir/output/RawOutput.h"
#include "mir/param/SimpleParametrisation.h"

#include "multio/LibMultio.h"
#include "multio/util/PrecisionTag.h"


namespace multio::action::interpolate {


void fill(const eckit::LocalConfiguration& sub, mir::param::SimpleParametrisation& destination) {
    if (sub.has("loadFromFile")) {
        auto cfg = sub.getSubConfiguration("loadFromFile");

        for (const auto& key : cfg.keys()) {
            std::fstream inputFile;
            inputFile.open(cfg.getString(key));
            ASSERT(inputFile.is_open());

            std::vector<long> val;
            std::string str;
            while (getline(inputFile, str)) {
                val.push_back(long(atoi(str.c_str())));
            }
            destination.set(key, val);
        }
    };

    if (sub.has("setByValue")) {
        auto cfg = sub.getSubConfiguration("setByValue");

        for (const auto& key : cfg.keys()) {
            eckit::Value cfgVal = cfg.getSubConfiguration(key).get();
            if (cfgVal.isMap()) {
                throw eckit::NotImplemented("Action::Interpolate :: Nested forwarding is not supported", Here());
            }

            if (cfgVal.isList()) {
                if (cfgVal.head().isDouble()) {
                    destination.set(key, cfg.getDoubleVector(key));
                }
                else if (cfgVal.head().isNumber()) {
                    destination.set(key, cfg.getLongVector(key));
                }
                else if (cfgVal.head().isString()) {
                    destination.set(key, cfg.getStringVector(key));
                }
                else {
                    throw eckit::NotImplemented("Action::Interpolate :: Unsupported datatype", Here());
                };
            }
            else if (cfgVal.isBool()) {
                destination.set(key, cfg.getBool(key));
            }
            else if (cfgVal.isDouble()) {
                destination.set(key, cfg.getDouble(key));
            }
            else if (cfgVal.isNumber()) {
                destination.set(key, cfg.getInt(key));
            }
            else if (cfgVal.isString()) {
                destination.set(key, cfg.getString(key).c_str());
            }
            else {
                throw eckit::NotImplemented("Action::Interpolate :: Unsupported datatype", Here());
            }
        }
    };
};


message::Message Interpolate::InterpolateInSinglePrecision(message::Message&& msg) const {
    using Header = message::Message::Header;

    // convert data from single into double precision
    const size_t in_size = msg.payload().size() / sizeof(float);

    eckit::Buffer dbl_buffer(in_size * sizeof(double));

    auto dbl_md = msg.metadata();
    dbl_md.set("globalSize", dbl_buffer.size()).set("precision", "double");

    {
        const auto* flt = reinterpret_cast<const float*>(msg.payload().data());
        auto* dbl = reinterpret_cast<double*>(dbl_buffer.data());
        for (size_t i = 0; i < in_size; ++i) {
            *(dbl++) = static_cast<double>(*(flt++));
        }
    }

    // interpolate
    auto out = InterpolateInDoublePrecision(
        {Header{msg.tag(), msg.source(), msg.destination(), std::move(dbl_md)}, std::move(dbl_buffer)});

    // convert data from double into single precision
    const size_t out_size = out.payload().size() / sizeof(double);

    eckit::Buffer flt_buffer(out_size * sizeof(float));

    auto flt_md = out.metadata();
    flt_md.set("globalSize", flt_buffer.size()).set("precision", "single");

    {
        const auto* dbl = reinterpret_cast<const double*>(out.payload().data());
        auto* flt = reinterpret_cast<float*>(flt_buffer.data());
        for (size_t i = 0; i < out_size; ++i) {
            *(flt++) = static_cast<float>(*(dbl++));
        }
    }

    return {Header{out.tag(), out.source(), out.destination(), std::move(flt_md)}, std::move(flt_buffer)};
}


message::Message Interpolate::InterpolateInDoublePrecision(message::Message&& msg) const {
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the input message :: " << std::endl
                             << msg.metadata() << std::endl
                             << std::endl;

    const auto& config = Action::confCtx_.config();
    const auto outputSize = config.getLong("outputSize");

    const double* data = reinterpret_cast<const double*>(msg.payload().data());
    const size_t size = msg.payload().size() / sizeof(double);

    mir::param::SimpleParametrisation inputPar;
    fill(config.getSubConfiguration("inputConfiguration"), inputPar);

    mir::input::RawInput input(data, size, inputPar);

    mir::api::MIRJob job;
    fill(config.getSubConfiguration("jobConfiguration"), job);

    if (msg.metadata().has("missingValue")) {
        job.set("missing_value", msg.metadata().getDouble("missingValue"));
    }

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: input :: " << std::endl << inputPar << std::endl << std::endl;

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: job " << std::endl << job << std::endl << std::endl;

    std::vector<double> outData(outputSize, 0.0);

    mir::param::SimpleParametrisation outMetadata;

    mir::output::RawOutput output(outData.data(), outData.size(), outMetadata);

    job.execute(input, output);

    message::Metadata md;
    md.set("globalSize", outData.size());
    md.set("precision", "double");

    eckit::Buffer buffer(reinterpret_cast<const char*>(outData.data()), outputSize * sizeof(double));

    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the output message :: " << std::endl
                             << md << std::endl
                             << std::endl;

    return {message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(), std::move(md)},
            std::move(buffer)};
}


void Interpolate::executeImpl(message::Message msg) const {
    switch (msg.tag()) {
        case (message::Message::Tag::Field): {
            executeNext(util::dispatchPrecisionTag(msg.precision(), [&](auto pt) -> message::Message {
                using PT = typename decltype(pt)::type;
                if (std::is_same<double, PT>::value) {
                    return InterpolateInDoublePrecision(std::move(msg));
                }
                else if (std::is_same<float, PT>::value) {
                    return InterpolateInSinglePrecision(std::move(msg));
                }
                NOTIMP;
            }));
            break;
        };
        case (message::Message::Tag::StepComplete): {
            executeNext(msg);
            break;
        }
        default: {
            throw eckit::BadValue("Action::Interpolate :: Unsupported message tag", Here());
            break;
        }
    };
}


void Interpolate::print(std::ostream& os) const {
    os << "Interpolate";
}


static ActionBuilder<Interpolate> InterpolateBuilder("interpolate");


}  // namespace multio::action::interpolate
