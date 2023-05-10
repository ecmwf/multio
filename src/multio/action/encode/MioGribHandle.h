#include "metkit/codes/GribHandle.h"

#include "eccodes.h"
namespace multio {
namespace action {

class MioGribHandle : public metkit::grib::GribHandle {
public:
    MioGribHandle(codes_handle* hdl);
    ~MioGribHandle() = default;
    using metkit::grib::GribHandle::setDataValues;
    MioGribHandle* MioGribHandle::duplicate() const;
    void setValue(const std::string& key, long value);

    void setValue(const std::string& key, double value);

    void setValue(const std::string& key, const std::string& value);

    void setValue(const std::string& key, const unsigned char* value);

    void setValue(const std::string& key, bool value);

    // Set values
    void setDataValues(const float* data, size_t count);
};


}  // namespace action
}  // namespace multio