
#ifndef multio_sandbox_print_buffer_H
#define multio_sandbox_print_buffer_H

#include <iomanip>
#include <iostream>
#include <iterator>
#include <algorithm>

namespace multio {
namespace sandbox {

template <typename T, int width>
struct fixed_width_val {
    T v;
    fixed_width_val(T v_) : v(v_) {}
};

template <typename T, int width>
std::ostream& operator<<(std::ostream& ostrm, const fixed_width_val<T, width>& fwv) {
    return ostrm << std::setw(width) << fwv.v;
}

template <typename T, int size>
inline void print_buffer(const T (&buffer)[size], std::ostream& ostrm = std::cout,
                         const char delim[] = " ") {
    std::copy(std::begin(buffer), std::end(buffer),
         std::ostream_iterator<fixed_width_val<T, 3>>(ostrm, delim));
}

template <typename T>
inline void print_buffer(const T* arr, size_t sz, std::ostream& ostrm = std::cout,
                         const char delim[] = " ") {
    for (auto ii = 0u; ii != sz; ++ii) {
        ostrm << arr[ii] << delim;
    }
}

template <typename T>
void print_buffer(const std::vector<T>& vec, std::ostream& ostrm = std::cout,
                  const char delim[] = " ") {
    copy(begin(vec), end(vec), std::ostream_iterator<T>(ostrm, delim));
}

}  // namespace sandbox
}  // namespace multio

#endif
