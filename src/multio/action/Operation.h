#ifndef multio_server_actions_Operation_H
#define multio_server_actions_Operation_H

#include <memory>
#include <string>
#include <vector>

namespace multio {
namespace action {

//==== Base class =================================

class Operation {
public:
    Operation(const std::string& name, long sz);
    const std::string& name();

    virtual const std::vector<double>& compute() = 0;
    virtual void update(const double* val, long sz) = 0;

    virtual ~Operation() = default;

protected:
    virtual void print(std::ostream& os) const = 0;

    std::string name_;
    std::vector<double> values_;

    friend std::ostream& operator<<(std::ostream& os, const Operation& a);
};

//==== Derived classes ============================

class Instant final : public Operation {
public:
    Instant(const std::string& name, long sz = 0);

    const std::vector<double>& compute() override;

    void update(const double* val, long sz) override;

private:
    void print(std::ostream &os) const override;
};

class Average final : public Operation {
    long count_ = 0;

public:
    Average(const std::string& name, long sz = 0);

    const std::vector<double>& compute() override;

    void update(const double* val, long sz) override;

private:
    void print(std::ostream &os) const override;
};

class Minimum final : public Operation {
public:
    Minimum(const std::string& name, long sz = 0);

    const std::vector<double>& compute() override;

    void update(const double* val, long sz) override;

private:
    void print(std::ostream &os) const override;
};

class Maximum final : public Operation {
public:
    Maximum(const std::string& name, long sz = 0);

    const std::vector<double>& compute() override;

    void update(const double* val, long sz) override;

private:
    void print(std::ostream &os) const override;
};

class Accumulate final : public Operation {
public:
    Accumulate(const std::string& name, long sz = 0);

    const std::vector<double>& compute() override;

    void update(const double* val, long sz) override;

private:
    void print(std::ostream &os) const override;
};

//==== Factory function ============================

std::unique_ptr<Operation> make_operation(const std::string& opname, long sz);

}  // namespace action
}  // namespace multio

#endif // multio_server_actions_Operation_H
