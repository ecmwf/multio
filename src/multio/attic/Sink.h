
#ifndef multio_attic_Sink_H
#define multio_attic_Sink_H

#include "multio/sink/DataSink.h"
#include "multio/attic/Action.h"

namespace atlas {
namespace util {
class Metadata;
}
}  // namespace atlas

namespace multio {
namespace attic {

class Sink : public Action {
public:
    explicit Sink(DataSink* ds, const std::string& nm = "Sink");
    explicit Sink(std::unique_ptr<DataSink>&& ds, const std::string& nm = "Sink");

private:  // overriding methods
    bool doExecute(std::shared_ptr<Message> msg) const override;

private:  // non-overriding methods
    void configure(const atlas::util::Metadata& metadata) const;

    bool write(const Message& msg) const;

    bool flush() const;

private:  // members
    mutable std::unique_ptr<DataSink> dataSink_;
};

}  // namespace attic
}  // namespace multio

#endif
