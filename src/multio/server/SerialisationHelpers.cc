#include <cstring>

#include "SerialisationHelpers.h"

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/io/Buffer.h"
#include "eckit/parser/JSON.h"
#include "eckit/exception/Exceptions.h"

#include "atlas/array.h"
#include "atlas/field/Field.h"
#include "atlas/util/Metadata.h"

#include "multio/server/PartialMapping.h"
#include "multio/server/Message.h"

namespace multio {
namespace server {

auto pack_metadata(const eckit::LocalConfiguration& config) -> std::string {
    std::stringstream ss;
    eckit::JSON json(ss);
    json << config;
    return ss.str();
}

auto pack_mapping(const PartialMapping& mapping) -> std::string {
    auto meta_str = pack_metadata(mapping.metadata);

    auto sz = meta_str.size();
    auto sz_bm = sizeof(decltype(sz));
    auto data_size = mapping.indices.size() * sizeof(decltype(mapping.indices)::value_type);

    // Create destination buffer
    auto tot_size = sz_bm + sz + data_size;
    std::string dest(tot_size, ' ');

    // Copy the size of the metatdate into the destination buffer
    auto pos = 0ul;
    std::memcpy(&dest[0], &sz, sz_bm);
    pos += sz_bm;

    // Copy metadata to destination buffer
    std::memcpy(&dest[pos], meta_str.data(), meta_str.size());
    pos += meta_str.size();

    // Copy field data to destination buffer
    std::memcpy(&dest[pos], mapping.indices.data(), data_size);
    pos += data_size;
    ASSERT(pos == dest.size());

    return dest;
}

void mapping_to_message(const PartialMapping& mapping, Message& msg) {
    ASSERT(msg.tag() == msg_tag::plan_data);

    auto meta_str = pack_metadata(mapping.metadata);

    auto sz = meta_str.size();
    auto sz_bm = sizeof(decltype(sz));
    auto data_size = mapping.indices.size() * sizeof(decltype(mapping.indices)::value_type);

    // Create destination buffer
    auto tot_size = sz_bm + sz + data_size;
    msg.resize(tot_size);

    // Copy the size of the metadate into the destination buffer
    msg.write(&sz, sz_bm);

    // Copy metadata to destination buffer
    msg.write(meta_str.data(), meta_str.size());

    // Copy field data to destination buffer
    msg.write(mapping.indices.data(), data_size);
}

auto pack_atlas_field(const atlas::Field& field) -> std::string {
    auto meta_str = pack_metadata(field.metadata());

    auto sz = meta_str.size();
    auto sz_bm = sizeof(decltype(sz));
    auto data_size = field.bytes();

    // Create destination buffer
    auto tot_size = sz_bm + sz + data_size;
    std::string dest(tot_size, ' ');

    // Copy the size of the metatdate into the destination buffer
    auto pos = 0ul;
    std::memcpy(&dest[0], &sz, sz_bm);
    pos += sz_bm;

    // Copy metadata to destination buffer
    std::memcpy(&dest[pos], meta_str.data(), meta_str.size());
    pos += meta_str.size();

    // Copy field data to destination buffer
    auto view = atlas::array::make_view<double, 1>(field);
    std::memcpy(&dest[pos], view.data(), data_size);
    pos += data_size;
    ASSERT(pos == dest.size());

    return dest;
}

// TODO: If we stick with atlas fields, we'd like to be able to write 'msg << field'
void atlas_field_to_message(const atlas::Field& field, Message& msg) {
    ASSERT(msg.tag() == msg_tag::field_data);

    auto meta_str = pack_metadata(field.metadata());

    auto sz = meta_str.size();
    auto sz_bm = sizeof(decltype(sz));
    auto data_size = field.bytes();

    // Create destination buffer
    auto tot_size = sz_bm + sz + data_size;
    msg.resize(tot_size);

    // Copy the size of the metadate into the destination buffer
    msg.write(&sz, sz_bm);

    // Copy metadata to destination buffer
    msg.write(meta_str.data(), meta_str.size());

    // Copy field data to destination buffer
    auto view = atlas::array::make_view<double, 1>(field);
    msg.write(view.data(), data_size);
}

auto unpack_metadata(const std::string& str) -> eckit::LocalConfiguration {
    const eckit::Configuration& config = eckit::YAMLConfiguration{str};
    return eckit::LocalConfiguration{config};
}

auto unpack_metadata(const eckit::Buffer& buf) -> eckit::LocalConfiguration {
    auto str = std::string(buf.size(), ' ');
    std::memcpy(const_cast<char*>(str.data()), buf, buf.size());
    return unpack_metadata(str);
}

auto fetch_metadata(const Message& msg) -> atlas::util::Metadata {
    auto meta_size = 0ul;
    msg.read(&meta_size, sizeof(unsigned long));

    auto meta_buf = eckit::Buffer(meta_size);
    msg.read(meta_buf, meta_size);
    msg.rewind();

    return unpack_metadata(meta_buf);
}

auto unpack_atlas_field(const Message& msg) -> atlas::Field {
    auto meta_size = 0ul;
    msg.read(&meta_size, sizeof(unsigned long));

    auto meta_buf = eckit::Buffer(meta_size);
    msg.read(meta_buf, meta_size);

    auto byte_size = msg.size() - meta_size - sizeof(long);
    auto data_size = byte_size / sizeof(double);

    auto local_data = eckit::Buffer(byte_size);
    msg.read(local_data, byte_size);
    auto field_data = reinterpret_cast<const double*>((const char*)(local_data));

    atlas::util::Metadata metadata = unpack_metadata(meta_buf);
    auto field = atlas::Field{metadata.get<std::string>("name"), atlas::array::DataType("real64"),
                              atlas::array::make_shape(static_cast<int>(data_size))};
    field.metadata() = metadata;

    auto view = atlas::array::make_view<double, 1>(field);
    std::copy_n(field_data, data_size, view.data());

    return field;
}

auto recreate_atlas_field(const atlas::util::Metadata& metadata) -> atlas::Field {
    auto field = atlas::Field{};

    auto gl_size = metadata.get<size_t>("gl_size");
    auto shape = atlas::array::make_shape(gl_size);

    auto name = metadata.get<std::string>("name");
    field = atlas::Field(name, atlas::array::DataType("real64"), shape);

    field.metadata() = metadata;
    return field;
}

}  // namespace server
}  // namespace multio
