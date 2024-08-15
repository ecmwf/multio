#include "MioGribHandle.h"

#include <iomanip>

#include "eckit/exception/Exceptions.h"
#include "eckit/utils/MD5.h"
#include "multio/LibMultio.h"

#define DIGEST_LENGTH MD5_DIGEST_LENGTH

namespace multio::util {

namespace {
template <typename T>
void codesCheckRelaxed(int ret, const char* name, const T& value) {
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


MioGribHandle::MioGribHandle(codes_handle* hdl) : metkit::grib::GribHandle{hdl} {};

std::unique_ptr<MioGribHandle> MioGribHandle::duplicate() const {
    codes_handle* h = codes_handle_clone(raw());
    if (!h) {
        throw eckit::SeriousBug("failed to clone output grib", Here());
    }
    return std::make_unique<MioGribHandle>(h);
}

namespace {
void setLongValue(codes_handle* hdl, const char* key, long value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting long value " << value << " for key " << key << std::endl;
    codesCheckRelaxed(codes_set_long(hdl, key, value), key, value);
};
}  // namespace
void MioGribHandle::setValue(const char* key, std::int64_t value) {
    setLongValue(this->raw(), key, value);
};
void MioGribHandle::setValue(const char* key, std::int32_t value) {
    setLongValue(this->raw(), key, value);
};
void MioGribHandle::setValue(const char* key, std::int16_t value) {
    setLongValue(this->raw(), key, value);
};
void MioGribHandle::setValue(const char* key, std::int8_t value) {
    setLongValue(this->raw(), key, value);
};

void MioGribHandle::setValue(const char* key, double value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    codesCheckRelaxed(codes_set_double(raw(), key, value), key, value);
};
void MioGribHandle::setValue(const char* key, float value) {
    setValue(key, static_cast<double>(value));
};

void MioGribHandle::setValue(const char* key, const std::string& value) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << " for key " << key << std::endl;
    size_t sz = value.size();
    codesCheckRelaxed(codes_set_string(raw(), key, value.c_str(), &sz), key, value);
};

void MioGribHandle::setValue(const char* key, const unsigned char* value) {
    std::ostringstream oss;
    for (int i = 0; i < DIGEST_LENGTH; ++i) {
        oss << ((i == 0) ? "" : "-") << std::hex << std::setfill('0') << std::setw(2) << static_cast<short>(value[i]);
    }
    LOG_DEBUG_LIB(LibMultio) << "*** Setting unsigned char* value " << oss.str() << " for key " << key << std::endl;
    size_t sz = DIGEST_LENGTH;
    codesCheckRelaxed(codes_set_bytes(raw(), key, value, &sz), key, value);
};

void MioGribHandle::setValue(const char* key, bool value) {
    long longValue = value;
    LOG_DEBUG_LIB(LibMultio) << "*** Setting value " << value << "(" << longValue << ") for key " << key << std::endl;
    codesCheckRelaxed(codes_set_long(raw(), key, longValue), key, value);
}

void MioGribHandle::setMissing(const char* key) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting missing for key " << key << std::endl;
    codesCheckRelaxed(codes_set_missing(raw(), key), key, "missing");
}

void MioGribHandle::setValue(const char* key, const std::vector<std::string>& values) {
    std::vector<const char*> v;
    v.reserve(values.size());
    LOG_DEBUG_LIB(LibMultio) << "*** Setting values (";
    for (const std::string& s : values) {
        v.push_back(s.c_str());
        LOG_DEBUG_LIB(LibMultio) << s << ", ";
    }
    LOG_DEBUG_LIB(LibMultio) << ") for key " << key << std::endl;
    codesCheckRelaxed(codes_set_string_array(raw(), key, v.data(), v.size()), key, "<string array ...>");
}

void MioGribHandle::setValue(const char* key, const std::vector<bool>& values) {
    std::vector<long> v;
    v.reserve(values.size());
    LOG_DEBUG_LIB(LibMultio) << "*** Setting values (";
    for (bool b : values) {
        v.push_back(b);
        LOG_DEBUG_LIB(LibMultio) << b << ", ";
    }
    LOG_DEBUG_LIB(LibMultio) << ") for key " << key << std::endl;
    codesCheckRelaxed(codes_set_long_array(raw(), key, v.data(), v.size()), key, "<bool/long array ...>");
}

void MioGribHandle::setValue(const char* key, const std::vector<unsigned char>& values) {
    setValue(key, values.data());
}

void MioGribHandle::setValue(const char* key, const std::vector<double>& values) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting values (";
    for (const auto& d : values) {
        LOG_DEBUG_LIB(LibMultio) << d << ", ";
    }
    LOG_DEBUG_LIB(LibMultio) << ") for key " << key << std::endl;
    codesCheckRelaxed(codes_set_double_array(raw(), key, values.data(), values.size()), key, "<double array ...>");
}
void MioGribHandle::setValue(const char* key, const std::vector<float>& values) {
    std::vector<double> vec;
    vec.reserve(values.size());
    for (float f : values) {
        vec.push_back(f);
    }
    setValue(key, vec);
}

namespace {
template <typename T, std::enable_if_t<std::is_same_v<T, long>, bool> = true>
void setLongValues(codes_handle* hdl, const char* key, const std::vector<T>& values) {
    LOG_DEBUG_LIB(LibMultio) << "*** Setting values (";
    for (const T& l : values) {
        LOG_DEBUG_LIB(LibMultio) << l << ", ";
    }
    LOG_DEBUG_LIB(LibMultio) << ") for key " << key << std::endl;
    codesCheckRelaxed(codes_set_long_array(hdl, key, values.data(), values.size()), key, "<long array ...>");
}

template <typename T, std::enable_if_t<!std::is_same_v<T, long>, bool> = true>
void setLongValues(codes_handle* hdl, const char* key, const std::vector<T>& values) {
    std::vector<long> vec;
    LOG_DEBUG_LIB(LibMultio) << "*** Setting values (";
    for (const T& l : values) {
        vec.push_back(l);
        LOG_DEBUG_LIB(LibMultio) << l << ", ";
    }
    LOG_DEBUG_LIB(LibMultio) << ") for key " << key << std::endl;
    codesCheckRelaxed(codes_set_long_array(hdl, key, vec.data(), vec.size()), key, "<long array ...>");
}
}  // namespace

void MioGribHandle::setValue(const char* key, const std::vector<std::int64_t>& values) {
    setLongValues(this->raw(), key, values);
}
void MioGribHandle::setValue(const char* key, const std::vector<std::int32_t>& values) {
    setLongValues(this->raw(), key, values);
}
void MioGribHandle::setValue(const char* key, const std::vector<std::int16_t>& values) {
    setLongValues(this->raw(), key, values);
}
void MioGribHandle::setValue(const char* key, const std::vector<std::int8_t>& values) {
    setLongValues(this->raw(), key, values);
}


// Set values
void MioGribHandle::setDataValues(const float* data, size_t count) {
    setDataValues(std::vector<double>{data, data + count});
    return;
}

void MioGribHandle::setDataValues(const std::vector<float>& data) {
    setDataValues(std::vector<double>{data.begin(), data.end()});
    return;
}

void MioGribHandle::setDataValues(const std::vector<double>& data) {
    setDataValues(data.data(), data.size());
    return;
}


}  // namespace multio::util
