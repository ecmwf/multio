#pragma once

#include <cstdint>
#include <memory>

#include "eckit/filesystem/PathName.h"
#include "metkit/codes/GribHandle.h"

#include "eccodes.h"

namespace multio::util {

// TODO
// Comment from Philipp Geier:
//   The whole metkit::grib::GribHandle needs a proper refactoring to support all necessary methods and to
//   be properly constructed from a `std::unique_ptr<codes_handle>` that can be passed around and supports proper moving
//   to pass ownership. The C++ wrapper should just add methods and no further members/state. Currently we end up using
//   `unique_ptr<GribHandle>`
class MioGribHandle : public metkit::grib::GribHandle {
public:
    // owning constructor
    MioGribHandle(codes_handle* hdl);
    MioGribHandle(const eckit::PathName&);
    // Non owning constructor
    MioGribHandle(codes_handle& hdl);

    ~MioGribHandle() = default;
    using metkit::grib::GribHandle::raw;
    using metkit::grib::GribHandle::setDataValues;
    std::unique_ptr<MioGribHandle> duplicate() const;
    
    static std::unique_ptr<MioGribHandle> makeDefault();

    // Check if key is defined and not missing
    bool hasKey(const char* key) const;
    bool hasKey(const std::string& key) const;

    // Check if key is defined
    bool isDefined(const char* key) const;
    bool isDefined(const std::string& key) const;

    // Check if key is missing
    bool isMissing(const char* key) const;
    bool isMissing(const std::string& key) const;

    std::string getString(const std::string& key) const;
    std::string getString(const char* key) const;
    long getLong(const std::string& key) const;
    long getLong(const char* key) const;
    double getDouble(const std::string& key) const;
    double getDouble(const char* key) const;
    std::size_t getSize(const std::string& key) const;
    std::size_t getSize(const char* key) const;
    std::vector<double> getDoubleArray(const std::string& key) const;
    std::vector<double> getDoubleArray(const char* key) const;
    std::vector<long> getLongArray(const std::string& key) const;
    std::vector<long> getLongArray(const char* key) const;
    
    // getStringArray is missing...


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
