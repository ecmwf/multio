#ifndef multio_MaestroCdo_H
#define multio_MaestroCdo_H

#include <map>
#include <string>
#include "eckit/exception/Exceptions.h"

extern "C" {
#include <maestro.h>
}

namespace multio {

class MaestroCdo {
public:
    MaestroCdo();
    MaestroCdo(std::string name);
    MaestroCdo(std::string name, const void* blob, uint64_t size);
    MaestroCdo(const MaestroCdo&) = delete;
    MaestroCdo& operator=(const MaestroCdo&) = delete;
    MaestroCdo(MaestroCdo&&);
    MaestroCdo& operator=(MaestroCdo&&);
    ~MaestroCdo();
    void seal();
    void offer();
    void require();
    void withdraw();
    void demand();
    void retract();
    void dispose();
    template<typename T>
    void set_attribute(const std::string& key, const T& value, bool copy=true) {
        ASSERT(MSTRO_OK == mstro_cdo_attribute_set(cdo_, key.c_str(), (void**)&value, copy));
    }
    template<typename T>
    T get_attribute(const std::string& key) {
        enum mstro_cdo_attr_value_type ttype;
        const T* val = NULL;
        mstro_cdo_attribute_get(cdo_, key.c_str(), &ttype, (const void**)&val);
        return *val;
    }
    uint64_t size() { return size_; }
    const void* data() { return data_; }
private:
    void declare();
    void set_size_and_data(uint64_t size, const void* data);
    void get_size_and_data();
    void print(std::ostream&) const;

    friend std::ostream& operator<<(std::ostream& s, const MaestroCdo& c) {
        c.print(s);
        return s;
    }

    std::string name_ = "";
    uint64_t size_ = 0;
    void* data_ = nullptr;
    mstro_cdo cdo_ = nullptr;
};

}  // namespace multio

#endif  // multio_MaestroCdo_H
