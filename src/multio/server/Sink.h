
#ifndef multio_server_Sink_H
#define multio_server_Sink_H

#include "multio/DataSink.h"
#include "multio/server/Action.h"

namespace atlas {
namespace util {
class Metadata;
}
}  // namespace atlas

namespace multio {
namespace server {

class Sink : public Action {
public:
    explicit Sink(DataSink* ds, const std::string& nm = "Sink");
    explicit Sink(std::unique_ptr<DataSink>&& ds, const std::string& nm = "Sink");

private:  // overriding methods
    void doExecute(const atlas::Field& field, int) const override;
    bool doComplete(atlas::Field& field) const override;

private:  // non-overriding methods
    void configure(const atlas::util::Metadata& metadata) const;

private:  // members
    mutable std::unique_ptr<DataSink> dataSink_;
};

}  // namespace server
}  // namespace multio

#endif
