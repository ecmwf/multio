
#ifndef multio_sandbox_create_random_data_H
#define multio_sandbox_create_random_data_H

#include <random>

namespace multio {
namespace sandbox {

inline std::vector<double> create_random_data(const size_t sz) {
    std::vector<double> field;

    std::random_device rd;  // Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
    std::uniform_real_distribution<double> dis(13.0, 30.0);

    for (auto ii = 0u; ii != sz; ++ii) {
        field.push_back(dis(gen));
    }

    return field;
}

}  // namespace sandbox
}  // namespace multio

#endif
