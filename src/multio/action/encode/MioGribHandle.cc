#include "MioGribHandle.h"

#include <iomanip>

#include "GridInfo.h"
#include "multio/LibMultio.h"


namespace multio {
namespace action {

namespace {
template <typename T>
void codesCheckRelaxed(int ret, const std::string& name, const T& value) {
    if (ret == CODES_READ_ONLY) {
        // If value is read only, do not panic...
        eckit::Log::info() << "Multio GribEncoder: Ignoring readonly field " << name << " (tried to set value " << value
                           << ")" << std::endl;
        return;
    }
    // Avoid calling  CODES_CHECK and throw an exception instead. CODES_CHECK often panics with logs properly being
    // flushed
    if (ret != 0) {
        std::ostringstream oss;
        oss << "Multio GribEncoder: CODES return value != NULL for operation on field: " << name << " with value "
            << value << ". EECODES error message: " << codes_get_error_message(ret) << std::endl;
        throw eckit::SeriousBug(oss.str(), Here());
    }
    CODES_CHECK(ret, NULL);
}
}  // namespace


MioGribHandle::MioGribHandle(metkit::grib::GribHandle* hdl) : metkit::grib::GribHandle{hdl->raw()} {};
MioGribHandle::MioGribHandle(codes_handle* hdl) : metkit::grib::GribHandle{hdl} {};


void MioGribHandle::setValue(const std::string& key, long value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    codesCheckRelaxed(codes_set_long(raw(), key.c_str(), value), key, value);
};

void MioGribHandle::setValue(const std::string& key, double value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    codesCheckRelaxed(codes_set_double(raw(), key.c_str(), value), key, value);
};

void MioGribHandle::setValue(const std::string& key, const std::string& value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    size_t sz = value.size();
    codesCheckRelaxed(codes_set_string(raw(), key.c_str(), value.c_str(), &sz), key, value);
};

void MioGribHandle::setValue(const std::string& key, const unsigned char* value) {
    std::ostringstream oss;
    for (int i = 0; i < DIGEST_LENGTH; ++i) {
        oss << ((i == 0) ? "" : "-") << std::hex << std::setfill('0') << std::setw(2) << static_cast<short>(value[i]);
    }
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << oss.str() << " for key " << key << std::endl;
    size_t sz = DIGEST_LENGTH;
    codesCheckRelaxed(codes_set_bytes(raw(), key.c_str(), value, &sz), key, value);
};

void MioGribHandle::setValue(const std::string& key, bool value) {
    long longValue = value;
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << "(" << longValue << ") for key " << key << std::endl;
    codesCheckRelaxed(codes_set_long(raw(), key.c_str(), longValue), key, value);
}

// Set values
void MioGribHandle::setDataValues(const float* data, size_t count) {
    std::vector<double> dvalues(count, 0.0);
    auto values = reinterpret_cast<const float*>(data);
    for (int i = 0; i < count; ++i) {
        dvalues[i] = double(values[i]);
    }

    setDataValues(dvalues.data(), count);

    return;
}


}  // namespace action
}  // namespace multio