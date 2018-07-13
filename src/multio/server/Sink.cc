
#include "Sink.h"

#include <iostream>

#include "eckit/parser/JSONDataBlob.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/PlainDataBlob.h"
#include "multio/server/Message.h"
#include "multio/server/SerialisationHelpers.h"

namespace multio {
namespace server {

Sink::Sink(DataSink* ds, const std::string nm) : Action{std::move(nm)}, dataSink_(ds) {}
Sink::Sink(std::unique_ptr<DataSink>&& ds, const std::string nm) :
    Action{std::move(nm)},
    dataSink_(std::move(ds)) {}

void Sink::do_execute(const atlas::Field& field, int /*unused*/) const {

    configure(field.metadata());

    Message msg(0, -1, msg_tag::field_data);
    atlas_field_to_message(field, msg);

    eckit::DataBlobPtr blob(eckit::DataBlobFactory::build("plain", msg.data(), msg.size()));

    dataSink_->write(blob);
}

bool Sink::do_complete(atlas::Field& field) const {
    // no blocking condition -- is write blocking, anyway?
    return true;
}

void Sink::configure(const atlas::util::Metadata& metadata) const {
    eckit::LocalConfiguration config;
    config.set("path", eckit::PathName{metadata.get<std::string>("name") +
                                       "::" + std::to_string(metadata.get<int>("levels")) +
                                       "::" + std::to_string(metadata.get<int>("steps"))});
    dataSink_.reset(DataSinkFactory::instance().build("file", config));
}

}  // namespace server
}  // namespace multio
