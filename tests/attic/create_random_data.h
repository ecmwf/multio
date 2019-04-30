
#ifndef multio_create_random_data_H
#define multio_create_random_data_H

#include <random>

namespace multio {
namespace attic {
namespace test {

inline auto create_random_data(const std::string& field_name, const size_t sz)
    -> std::vector<double> {
    std::vector<double> field;
    std::random_device rd;  // Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()

    std::uniform_real_distribution<double> dis;
    if(field_name == "temperature") {
        dis = std::uniform_real_distribution<double>(0.0, 25.0);
    } else if(field_name == "geopotential") {
        dis = std::uniform_real_distribution<double>(13.0, 30.0);
    } else {
        dis = std::uniform_real_distribution<double>(0.0, 1.0);
    }

    for (auto ii = 0u; ii != sz; ++ii) {
        field.push_back(dis(gen));
    }

    return field;
}

}  // namespace attic
}  // namespace test
}  // namespace multio

#endif
