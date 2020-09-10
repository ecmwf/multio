#ifndef multio_server_actions_Operation_H
#define multio_server_actions_Operation_H

#include <memory>
#include <vector>

namespace multio {
namespace action {

//==== Base class =================================

class Operation {
public:
    Operation(long sz);
    virtual ~Operation() = default;

    virtual const std::vector<double>& compute() = 0;

    virtual void update(const double* val, long sz) = 0;

protected:
    std::vector<double> values_;
};

//==== Derived classes ============================

class Instant final : public Operation {
public:
    Instant(long sz);

    const std::vector<double>& compute() override;

    void update(const double* val, long sz) override;
};

class Average final : public Operation {
    long count_;

public:
    Average(long sz);

    const std::vector<double>& compute() override;

    void update(const double* val, long sz) override;
};

class Minimum final : public Operation {
public:
    Minimum(long sz);

    const std::vector<double>& compute() override;

    void update(const double* val, long sz) override;
};

class Maximum final : public Operation {
public:
    Maximum(long sz);

    const std::vector<double>& compute() override;

    void update(const double* val, long sz) override;
};

class Accumulate final : public Operation {
public:
    Accumulate(long sz);

    const std::vector<double>& compute() override;

    void update(const double* val, long sz) override;
};

//==== Factory function ============================

std::unique_ptr<Operation> make_operation(const std::string& opname, long sz);

}  // namespace action
}  // namespace multio

#endif // multio_server_actions_Operation_H
