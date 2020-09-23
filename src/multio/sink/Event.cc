
#include "Event.h"

#include "eckit/log/JSON.h"
#include "eckit/types/Types.h"

namespace multio {

std::ostream& operator<<(std::ostream& s, const Event& o) {
    o.print(s);
    return s;
}

Event::Event(const std::string& event, eckit::StringDict info) : type_(event), info_(info) {}

void Event::info(const std::string& k, const std::string& v) {
    info_[k] = v;
}

//----------------------------------------------------------------------------------------------------------------------

const char* MetadataChange::eventType() {
    return "MetadataChange";
}

MetadataChange::MetadataChange(eckit::StringDict info, eckit::StringDict metadata) :
    Event(eventType(), info), metadata_(metadata) {}

void MetadataChange::metadata(const std::string& k, const std::string& v) {
    metadata_[k] = v;
}

void MetadataChange::json(eckit::JSON& s) const {
    s.startObject();
    s << "type" << type_;
    s << "info" << info_;
    s << "metadata" << metadata_;
    s.endObject();
}

void MetadataChange::print(std::ostream& o) const {
    o << eventType() << "("
      << "type=" << type_ << ",info=" << info_ << ",metadata=" << metadata_ << ")";
}

//----------------------------------------------------------------------------------------------------------------------

const char* NotifyMetadata::eventType() {
    return "NotifyMetadata";
}

NotifyMetadata::NotifyMetadata(eckit::StringDict info, eckit::StringDict metadata) :
    Event(eventType(), info), metadata_(metadata) {}

void NotifyMetadata::metadata(const std::string& k, const std::string& v) {
    metadata_[k] = v;
}

void NotifyMetadata::json(eckit::JSON& s) const {
    s.startObject();
    s << "type" << type_;
    s << "info" << info_;
    s << "metadata" << metadata_;
    s.endObject();
}

void NotifyMetadata::print(std::ostream& o) const {
    o << eventType() << "("
      << "type=" << type_
      << ",info=" << info_
      << ",metadata=" << metadata_
      << ")";
}


}  // namespace multio
