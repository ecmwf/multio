#pragma once

#include <cstdint>
#include <memory>

#include "metkit/codes/GribHandle.h"

#include "eccodes.h"

namespace multio::util {

class MioGribHandle : public metkit::grib::GribHandle {
public:
    // owning constructor
    MioGribHandle(codes_handle* hdl);
    // Non owning constructor
    MioGribHandle(codes_handle& hdl);
    ~MioGribHandle() = default;
    using metkit::grib::GribHandle::setDataValues;
    std::unique_ptr<MioGribHandle> duplicate() const;
    void setValue(const char* key, std::int64_t value);
    void setValue(const char* key, std::int32_t value);
    void setValue(const char* key, std::int16_t value);
    void setValue(const char* key, std::int8_t value);

    void setValue(const char* key, double value);
    void setValue(const char* key, float value);

    void setValue(const char* key, const std::string& value);

    void setValue(const char* key, const unsigned char* value);

    void setValue(const char* key, bool value);

    void setValue(const char* key, const std::vector<std::string>& values);

    void setValue(const char* key, const std::vector<bool>& values);
    void setValue(const char* key, const std::vector<unsigned char>& values);

    void setValue(const char* key, const std::vector<double>& values);
    void setValue(const char* key, const std::vector<float>& values);

    void setValue(const char* key, const std::vector<std::int64_t>& values);
    void setValue(const char* key, const std::vector<std::int32_t>& values);
    void setValue(const char* key, const std::vector<std::int16_t>& values);
    void setValue(const char* key, const std::vector<std::int8_t>& values);

    void setValue(const std::string& key, std::int64_t value) { setValue(key.c_str(), value); };
    void setValue(const std::string& key, std::int32_t value) { setValue(key.c_str(), value); };
    void setValue(const std::string& key, std::int16_t value) { setValue(key.c_str(), value); };
    void setValue(const std::string& key, std::int8_t value) { setValue(key.c_str(), value); };

    void setValue(const std::string& key, double value) { setValue(key.c_str(), value); };
    void setValue(const std::string& key, float value) { setValue(key.c_str(), value); };

    void setValue(const std::string& key, const std::string& value) { setValue(key.c_str(), value); };

    void setValue(const std::string& key, const unsigned char* value) { setValue(key.c_str(), value); };

    void setValue(const std::string& key, bool value) { setValue(key.c_str(), value); };

    void setValue(const std::string& key, const std::vector<std::string>& values) { setValue(key.c_str(), values); };

    void setValue(const std::string& key, const std::vector<bool>& values) { setValue(key.c_str(), values); };
    void setValue(const std::string& key, const std::vector<unsigned char>& values) { setValue(key.c_str(), values); };

    void setValue(const std::string& key, const std::vector<double>& values) { setValue(key.c_str(), values); };
    void setValue(const std::string& key, const std::vector<float>& values) { setValue(key.c_str(), values); };

    void setValue(const std::string& key, const std::vector<std::int64_t>& values) { setValue(key.c_str(), values); };
    void setValue(const std::string& key, const std::vector<std::int32_t>& values) { setValue(key.c_str(), values); };
    void setValue(const std::string& key, const std::vector<std::int16_t>& values) { setValue(key.c_str(), values); };
    void setValue(const std::string& key, const std::vector<std::int8_t>& values) { setValue(key.c_str(), values); };

    void setMissing(const char* key);
    void setMissing(const std::string& key) { setMissing(key.c_str()); };

    // Set values
    void setDataValues(const float* data, size_t count);

    void setDataValues(const std::vector<double>& data);
    void setDataValues(const std::vector<float>& data);
};


}  // namespace multio::util
