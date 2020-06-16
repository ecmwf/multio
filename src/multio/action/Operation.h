#ifndef multio_server_actions_Operation_H
#define multio_server_actions_Operation_H

#include <vector>

namespace multio {
namespace action {

//==== Base class =================================

class Operation {
public:
    Operation(long sz);
    virtual ~Operation() = default;

    virtual void update(const double* val, long sz) = 0;

protected:
    std::vector<double> values_;
};

//==== Derived classes ============================

class Instant final : public Operation {
    Instant(long sz);

    void update(const double* val, long sz) override;
};

class Average final : public Operation {
    Average(long sz);

    void update(const double* val, long sz) override;
};

class Minimum final : public Operation {
    Minimum(long sz);

    void update(const double* val, long sz) override;
};

class Maximum final : public Operation {
    Maximum(long sz);

    void update(const double* val, long sz) override;
};

class Sum final : public Operation {
    Sum(long sz);

    void update(const double* val, long sz) override;
};

}  // namespace action
}  // namespace multio

#endif // multio_server_actions_Operation_H
