/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "multio/util/PrecisionTag.h"

#include "Interpolate.h"


namespace multio {
namespace action {

using message::Message;
using message::Peer;

Interpolate::Interpolate(const ConfigurationContext& confCtx) :
    ChainedAction{confCtx},
    mainConfiguration_(interpolate::InterpolateParserFactory::instance().build(Action::confCtx_.config())) {}

Interpolate::~Interpolate() {}

void Interpolate::executeImpl(Message msg) const {
    switch (msg.tag()) {
        case (Message::Tag::Field): {
            if (msg.metadata().has("precision")) {
                switch (multio::util::decodePrecisionTag(msg.metadata().getString("precision"))) {
                    case multio::util::PrecisionTag::Float:
                        InterpolateRawMessageFloat(msg);
                        break;
                    case multio::util::PrecisionTag::Double:
                        InterpolateRawMessageDouble(msg);
                        break;
                    default:
                        throw eckit::BadValue("Action::Interpolate :: Unsupported datatype for input message",
                                              eckit::CodeLocation(__FILE__, __LINE__, __FUNCTION__));
                }
            }
            else {
                throw eckit::SeriousBug("Action::Interpolate :: Unable to find \"precision\" keyword in input metadata",
                                        eckit::CodeLocation(__FILE__, __LINE__, __FUNCTION__));
            }
            break;
        }
        case (Message::Tag::StepComplete): {
            executeNext(msg);
            break;
        }
        default: {
            throw eckit::BadValue("Action::Interpolate :: Unsupported message tag",
                                  eckit::CodeLocation(__FILE__, __LINE__, __FUNCTION__));
            break;
        }
    }
};

void Interpolate::print(std::ostream& os) const {
    os << "Interpolate";
}

void Interpolate::InterpolateRawMessageFloat(Message& msg) const {
    // Print Input metadata
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the input message :: " << std::endl
                             << msg.metadata() << std::endl
                             << std::endl;

    // Prepare input data for the interpolator (value of the input field)
    const float* data = reinterpret_cast<const float*>(msg.payload().data());
    const size_t size = msg.payload().size() / sizeof(float);

    // Prepare a double precision buffer
    std::vector<double> inData(size, 0.0);

    // Recast the fata from single precision input data to double precion buffer
    // needed to mir
    for (int i = 0; i < size; ++i) {
        inData[i] = double(data[i]);
    }

    // Prepare input parameters (description of the input field)
    mir::param::SimpleParametrisation inputPar;
    mainConfiguration_->MIRInput(msg.metadata(), inputPar);
    std::unique_ptr<mir::input::RawInput> input(new mir::input::RawInput(inData.data(), size, inputPar));

    // Prepare interpolation Job (configuration of the interpolation task)
    mir::api::MIRJob job;
    mainConfiguration_->MIRJob(msg.metadata(), job);

    // Show mir input configuration
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: mir input field description :: " << std::endl
                             << inputPar << std::endl
                             << std::endl;

    // Show mir job configuration
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: nir job description " << std::endl << job << std::endl << std::endl;

    // Allocate and initialize the out values
    std::vector<double> outDataDouble(mainConfiguration_->outputSize(), 0.0);

    // Construction of the output metadata (mir just forward the job configuration
    // in this object)
    mir::param::SimpleParametrisation outMetadata;

    // Preparation of the output object
    std::unique_ptr<mir::output::RawOutput> output(
        new mir::output::RawOutput(outDataDouble.data(), outDataDouble.size(), outMetadata));

    // Execute the job
    job.execute(*input, *output);

    // Allocation of the single precision output vector
    std::vector<float> outData(mainConfiguration_->outputSize(), 0.0);

    // Recast the fata from double precision mit output data to single precion
    // buffer needed to output a single precision message
    for (int i = 0; i < outDataDouble.size(); ++i) {
        outData[i] = float(outDataDouble[i]);
    }

    // Construction of the output message
    message::Metadata md{};

    // By default the only metadata to in the outptu message is the global size
    md.set("globalSize", outData.size());
    // Needed by the Grib encoder always needs to be forwarded
    md.set("precision", "single");

    // Fill the output metdata from configuration file
    mainConfiguration_->MIROutput(msg.metadata(), md);

    // construction of the buffer
    eckit::Buffer buffer(reinterpret_cast<const char*>(outData.data()),
                         mainConfiguration_->outputSize() * sizeof(float));

    // Show metadata of the output message
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the output message :: " << std::endl
                             << md << std::endl
                             << std::endl;

    // Next action
    executeNext(multio::message::Message{
        Message::Header{Message::Tag::Field, msg.source(), msg.destination(), std::move(md)}, std::move(buffer)});
}

void Interpolate::InterpolateRawMessageDouble(Message& msg) const {
    // Print Input metadata
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the input message :: " << std::endl
                             << msg.metadata() << std::endl
                             << std::endl;

    // Prepare input data for the interpolator (value of the input field)
    const double* data = reinterpret_cast<const double*>(msg.payload().data());
    const size_t size = msg.payload().size() / sizeof(double);

    // Prepare input parameters (description of the input field)
    mir::param::SimpleParametrisation inputPar;
    mainConfiguration_->MIRInput(msg.metadata(), inputPar);
    std::unique_ptr<mir::input::RawInput> input(new mir::input::RawInput(data, size, inputPar));

    // Prepare interpolation Job (configuration of the interpolation task)
    mir::api::MIRJob job;
    mainConfiguration_->MIRJob(msg.metadata(), job);

    // Show mir input configuration
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: mir input field description :: " << std::endl
                             << inputPar << std::endl
                             << std::endl;

    // Show mir job configuration
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: nir job description " << std::endl << job << std::endl << std::endl;

    // Allocate and initialize the out values
    std::vector<double> outData(mainConfiguration_->outputSize(), 0.0);

    // Construction of the output metadata (mir just forward the job configuration
    // in this object)
    mir::param::SimpleParametrisation outMetadata;

    // Preparation of the output object
    std::unique_ptr<mir::output::RawOutput> output(
        new mir::output::RawOutput(outData.data(), outData.size(), outMetadata));

    // Execute the job
    job.execute(*input, *output);

    // Construction of the output message
    message::Metadata md{};

    // By default the only metadata to in the outptu message is the global size
    md.set("globalSize", outData.size());
    // Needed by the Grib encoder always needs to be forwarded
    md.set("precision", "double");

    // Fill the output metdata from configuration file
    mainConfiguration_->MIROutput(msg.metadata(), md);

    // construction of the buffer
    eckit::Buffer buffer(reinterpret_cast<const char*>(outData.data()),
                         mainConfiguration_->outputSize() * sizeof(double));

    // Show metadata of the output message
    LOG_DEBUG_LIB(LibMultio) << "Interpolate :: Metadata of the output message :: " << std::endl
                             << md << std::endl
                             << std::endl;

    // Next action
    executeNext(multio::message::Message{
        Message::Header{Message::Tag::Field, msg.source(), msg.destination(), std::move(md)}, std::move(buffer)});
}

static ActionBuilder<Interpolate> InterpolateBuilder("interpolate");

}  // namespace action
}  // namespace multio
