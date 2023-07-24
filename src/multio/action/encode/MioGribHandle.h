#include <cstdint>

#include "metkit/codes/GribHandle.h"

#include "eccodes.h"

namespace multio {
namespace action {

class MioGribHandle : public metkit::grib::GribHandle {
public:
    MioGribHandle(codes_handle* hdl);
    ~MioGribHandle() = default;
    using metkit::grib::GribHandle::setDataValues;
    MioGribHandle* duplicate() const;
    void setValue(const std::string& key, std::int64_t value);
    void setValue(const std::string& key, std::int32_t value);
    void setValue(const std::string& key, std::int16_t value);
    void setValue(const std::string& key, std::int8_t value);

    void setValue(const std::string& key, double value);
    void setValue(const std::string& key, float value);

    void setValue(const std::string& key, const std::string& value);

    void setValue(const std::string& key, const unsigned char* value);

    void setValue(const std::string& key, bool value);

    void setMissing(const std::string& key);
    
    void setValues(const std::string& key, const std::vector<std::string>& values);

    void setValues(const std::string& key, const std::vector<bool>& values);

    void setValues(const std::string& key, const std::vector<double>& values);
    void setValues(const std::string& key, const std::vector<float>& values);

    void setValues(const std::string& key, const std::vector<std::int64_t>& values);
    void setValues(const std::string& key, const std::vector<std::int32_t>& values);
    void setValues(const std::string& key, const std::vector<std::int16_t>& values);
    void setValues(const std::string& key, const std::vector<std::int8_t>& values);

    // Set values
    void setDataValues(const float* values, size_t count);
};


}  // namespace action
}  // namespace multio
