
#ifndef multio_server_SerialisationHelpers_H
#define multio_server_SerialisationHelpers_H

#include <string>

// Forward declarations from atlas
namespace atlas {
class Field;
namespace util {
class Metadata;
}
}  // namespace atlas

// Forward declarations from eckit
namespace eckit {
class Buffer;
class LocalConfiguration;
}  // namespace eckit

namespace multio {

// Forward declarations form multio
struct PartialMapping;
class Message;

namespace server {

auto pack_metadata(const eckit::LocalConfiguration& config) -> std::string;

auto pack_mapping(const PartialMapping& mapping) -> std::string;
void mapping_to_message(const PartialMapping& mapping, Message& msg);

auto pack_atlas_field(const atlas::Field& field) -> std::string;
void atlas_field_to_message(const atlas::Field& field, Message& msg);

auto unpack_metadata(const std::string& str) -> eckit::LocalConfiguration;
auto unpack_metadata(const eckit::Buffer& buf) -> eckit::LocalConfiguration;
auto fetch_metadata(const Message& msg) -> atlas::util::Metadata;
auto unpack_atlas_field(const Message& msg) -> atlas::Field;

auto recreate_atlas_field(const atlas::util::Metadata& metadata) -> atlas::Field;

}  // namespace server
}  // namespace multio

#endif
