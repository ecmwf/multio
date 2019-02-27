
#ifndef multio_sandbox_TestData_H
#define multio_sandbox_TestData_H

#include <random>

namespace multio {
namespace sandbox {

// Default global values, but each test is allowed to override them
inline size_t& field_size() {
    static size_t val;
    return (!val ? (val = 23) : val);
}

inline size_t& root() {
    static size_t rt;  // = 0 if not set to another value
    return rt;
}

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

inline std::vector<double>& global_test_field(const std::string& field_id, const size_t sz) {
    using eckit::mpi::comm;

    static std::map<std::string, std::vector<double>> test_fields;
    if (test_fields.find(field_id) != end(test_fields)) {
        return test_fields[field_id];
    }
    test_fields[field_id] =
        (comm().rank() == root()) ? create_random_data(sz) : std::vector<double>(sz);
    comm().broadcast(test_fields[field_id], root());

    return test_fields[field_id];
}

inline std::vector<double> create_local_field(const std::vector<double>& global_field,
                                              const std::vector<int>& idxmap) {
    std::vector<double> local_field;
    for (auto idx : idxmap) {
        local_field.push_back(global_field[idx]);
    }

    return local_field;
}

}  // namespace sandbox
}  // namespace multio

#endif
